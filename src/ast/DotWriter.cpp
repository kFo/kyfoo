#include <kyfoo/ast/DotWriter.hpp>

#include <cassert>

#include <array>
#include <fstream>
#include <map>
#include <string>
#include <string_view>

#include <kyfoo/ast/ControlFlow.hpp>
#include <kyfoo/ast/Declarations.hpp>
#include <kyfoo/ast/Expressions.hpp>
#include <kyfoo/ast/Module.hpp>
#include <kyfoo/ast/Scopes.hpp>
#include <kyfoo/ast/Visitors.hpp>

namespace kyfoo::ast {

void writeDot(Module const& mod, std::filesystem::path const& path)
{
    std::ofstream fout(path);
    writeDot(fout, mod);
}

template <typename Dispatcher>
struct ExprStructWriter
{
    using result_t = void;

    Dispatcher& dispatch;
    std::ostream& stream;

    ExprStructWriter(Dispatcher& dispatch, std::ostream& stream)
        : dispatch(dispatch)
        , stream(stream)
    {
    }

public:
    result_t exprLiteral(LiteralExpression const& lit)
    {
        for ( auto c : lit.token().lexeme() ) {
            if ( c == '"' )
                stream << "\\\"";
            else
                stream << c;
        }
    }

    result_t exprIdentifier(IdentifierExpression const& ident)
    {
        stream << ident.token().lexeme();
    }

    void visit(Slice<Expression const*> exprs)
    {
        if ( exprs )
            dispatch(*exprs.front());

        for ( auto e : exprs(1, $) ) {
            stream << " | ";
            dispatch(*e);
        }
    }

    result_t exprTuple(TupleExpression const& tup)
    {
        switch ( tup.kind() ) {
        case TupleKind::Closed:    stream << "[] | {"; break;
        case TupleKind::Open:      stream << "() | {"; break;
        case TupleKind::OpenLeft:  stream << "(] | {"; break;
        case TupleKind::OpenRight: stream << "[) | {"; break;
        }
        visit(tup.expressions());
        stream << "}";
    }

    result_t exprApply(ApplyExpression const& app)
    {
        stream << "[apply] | {";
        visit(app.expressions());
        stream << "}";
    }

    result_t exprSymbol(SymbolExpression const& sym)
    {
        stream << "[sym][" << sym.token().lexeme() << "] | {";
        visit(sym.expressions());
        stream << "}";
    }

    result_t exprDot(DotExpression const& dot)
    {
        stream << "[.] | {";
        visit(dot.expressions());
        stream << "}";
    }

    result_t exprAssign(AssignExpression const& ass)
    {
        stream << "[=] | {";
        dispatch(ass.left());
        stream << " | ";
        dispatch(ass.right());
        stream << "}";
    }

    result_t exprLambda(LambdaExpression const&)
    {
        stream << "[=>]";
    }

    result_t exprArrow(ArrowExpression const& arrow)
    {
        stream << "[->] | {";
        dispatch(arrow.from());
        stream << " | ";
        dispatch(arrow.to());
        stream << "}";
    }

    result_t exprUniverse(UniverseExpression const& univ)
    {
        stream << "[universe" << univ.level() << "]";
    }
};

template <typename Dispatcher>
struct DotWriter;

template <typename Dispatcher>
struct NodeWriter
{
    using result_t = void;

    Dispatcher& dispatch;
    std::ostream& stream;
    std::string_view id;

    NodeWriter(Dispatcher& dispatch, std::ostream& stream, std::string_view id)
        : dispatch(dispatch)
        , stream(stream)
        , id(id)
    {
    }

    void beginAttrs(std::string_view label)
    {
        stream << id << " [label=" << quoted(label);
    }

    void endAttrs()
    {
        stream << "]\n";
    }

    void attr(std::string_view key, std::string_view value)
    {
        stream << "," << key << "=" << value;
    }

    result_t exprLiteral(LiteralExpression const& lit)
    {
        beginAttrs(lit.token().lexeme());
        endAttrs();
    }

    result_t exprIdentifier(IdentifierExpression const& ident)
    {
        beginAttrs(ident.token().lexeme());
        endAttrs();
    }

    result_t exprTuple(TupleExpression const&)
    {
        beginAttrs("tuple");
        endAttrs();
    }

    result_t exprApply(ApplyExpression const&)
    {
        beginAttrs("apply");
        endAttrs();
    }

    result_t exprSymbol(SymbolExpression const&)
    {
        beginAttrs("symexpr");
        endAttrs();
    }

    result_t exprDot(DotExpression const&)
    {
        beginAttrs("dot");
        endAttrs();
    }

    result_t exprAssign(AssignExpression const&)
    {
        beginAttrs("assign");
        endAttrs();
    }

    result_t exprLambda(LambdaExpression const&)
    {
        beginAttrs("lambda");
        endAttrs();
    }

    result_t exprArrow(ArrowExpression const&)
    {
        beginAttrs("arrow");
        endAttrs();
    }

    result_t exprUniverse(UniverseExpression const&)
    {
        beginAttrs("universe");
        endAttrs();
    }

    result_t stmtExpression(ExpressionStatement const&)
    {
        beginAttrs("stmt");
        endAttrs();
        stream << "subgraph " << id << " {\n";
        stream << "}\n";
    }

    result_t stmtVariable(VariableStatement const&)
    {
        beginAttrs("stmt-var");
        endAttrs();
        stream << "subgraph " << id << " {\n";
        stream << "}\n";
    }

    result_t juncBranch(BranchJunction const&)
    {
        beginAttrs("br");
        endAttrs();
    }

    result_t juncReturn(ReturnJunction const&)
    {
        beginAttrs("return");
        endAttrs();
    }

    result_t juncJump(JumpJunction const&)
    {
        beginAttrs("jump");
        endAttrs();
    }

    void beginDecl(Declaration const& decl)
    {
        beginAttrs(decl.symbol().token().lexeme());
    }

    result_t declDataSum(DataSumDeclaration const& ds)
    {
        beginDecl(ds);
        attr("kind", "data-sum");
        endAttrs();
    }

    result_t declDataSumCtor(DataSumDeclaration::Constructor const& dsCtor)
    {
        beginDecl(dsCtor);
        endAttrs();
    }

    result_t declDataProduct(DataProductDeclaration const& dp)
    {
        beginDecl(dp);
        endAttrs();
    }

    result_t declField(DataProductDeclaration::Field const& dpField)
    {
        beginDecl(dpField);
        endAttrs();
    }

    result_t declSymbol(SymbolDeclaration const& sym)
    {
        beginDecl(sym);
        endAttrs();
    }

    result_t declProcedure(ProcedureDeclaration const& proc)
    {
        std::string_view label = proc.symbol().token().lexeme();
        if ( auto decl = proc.scope().declaration() )
            if ( auto templ = decl->as<TemplateDeclaration>() )
                label = templ->symbol().token().lexeme();

        beginAttrs(label);
        endAttrs();
    }

    result_t declProcedureParameter(ProcedureParameter const& param)
    {
        beginDecl(param);
        endAttrs();
    }

    result_t declVariable(VariableDeclaration const& var)
    {
        beginDecl(var);
        endAttrs();
    }

    result_t declImport(ImportDeclaration const& imp)
    {
        beginDecl(imp);
        endAttrs();
    }

    result_t declSymbolVariable(SymbolVariable const& symVar)
    {
        beginDecl(symVar);
        endAttrs();
    }

    result_t declTemplate(TemplateDeclaration const& templ)
    {
        beginDecl(templ);
        endAttrs();
    }
};

template <typename Dispatcher>
struct DotWriter
{
    using result_t = std::string_view;
    Dispatcher& dispatch;
    std::ostream& stream;

    std::map<void const*, std::string> idMap;

    enum NodeKind {
        NDeclaration,
        NStatement,
        NJunction,
        NExpression,
        NCount
    };

    inline static const char* prefix = "DSJE";

    std::array<unsigned, NCount> counts = {};

    DotWriter(Dispatcher& dispatch, std::ostream& stream)
        : dispatch(dispatch)
        , stream(stream)
    {
        beginFile();
    }

    ~DotWriter()
    {
        endFile();
    }

    void beginFile()
    {
        stream << "digraph {\n"
                  "compound=true\n";
    }

    void endFile()
    {
        stream << "}\n";
    }

    struct NodeID {
        std::string_view id;
        bool isNew;
    };

    NodeID id(NodeKind kind, void const* node)
    {
        auto e = idMap.lower_bound(node);
        if ( e != end(idMap) && e->first == node )
            return { e->second, false };

        std::string id;
        id += prefix[kind];
        id += std::to_string(counts[kind]++);
        return {
            idMap.emplace_hint(e, std::make_pair(node, std::move(id)))->second,
            true
        };
    }

    NodeKind node_kind(Declaration const&) { return NDeclaration; };
    NodeKind node_kind(Statement   const&) { return NStatement;   };
    NodeKind node_kind(Junction    const&) { return NJunction;    };
    NodeKind node_kind(Expression  const&) { return NExpression;  };

    NodeID head(BasicBlock const& bb)
    {
        if ( !bb.statements().empty() )
            return id(*bb.statements().front());

        return id(*bb.junction());
    }

    NodeID tail(BasicBlock const& bb)
    {
        return id(*bb.junction());
    }

    template <typename T>
    NodeID id(T const& item)
    {
        return id(node_kind(item), &item);
    }

    template <typename T>
    std::string_view mkNode(T const& item)
    {
        auto node = id(item);
        if ( node.isNew ) {
            ShallowApply<NodeWriter> op(stream, node.id);
            op(item);
        }

        return node.id;
    }

    void mkEdge(std::string_view from, std::string_view to)
    {
        stream << from << " -> " << to << '\n';
    }

    result_t exprLiteral(LiteralExpression const& lit)
    {
        return mkNode(lit);
    }

    result_t exprIdentifier(IdentifierExpression const& ident)
    {
        return mkNode(ident);
    }

    result_t exprTuple(TupleExpression const& tup)
    {
        auto node = mkNode(tup);
        for ( auto const& e : tup.expressions() )
            mkEdge(node, dispatch(*e));

        return node;
    }

    result_t exprApply(ApplyExpression const& app)
    {
        auto node = mkNode(app);
        for ( auto const& e : app.expressions() )
            mkEdge(node, dispatch(*e));

        return node;
    }

    result_t exprSymbol(SymbolExpression const& sym)
    {
        auto node = mkNode(sym);
        for ( auto const& e : sym.expressions() )
            mkEdge(node, dispatch(*e));

        return node;
    }

    result_t exprDot(DotExpression const& dot)
    {
        auto node = mkNode(dot);
        for ( auto const& e : dot.expressions() )
            mkEdge(node, dispatch(*e));

        return node;
    }

    result_t exprAssign(AssignExpression const& ass)
    {
        auto node = mkNode(ass);
        mkEdge(node, dispatch(ass.left()));
        mkEdge(node, dispatch(ass.right()));

        return node;
    }

    result_t exprLambda(LambdaExpression const& lam)
    {
        auto node = mkNode(lam);
        mkEdge(node, declProcedure(lam.procedure()));

        return node;
    }

    result_t exprArrow(ArrowExpression const& arrow)
    {
        auto node = mkNode(arrow);
        mkEdge(node, dispatch(arrow.from()));
        mkEdge(node, dispatch(arrow.to()));

        return node;
    }

    result_t exprUniverse(UniverseExpression const& univ)
    {
        return mkNode(univ);
    }

    result_t stmtExpression(ExpressionStatement const& stmt)
    {
        ShallowApply<ExprStructWriter> op(stream);
        auto node = id(stmt);
        stream << node.id << " [label=\"";
        op(stmt.expression());
        stream << "\"]\n";

        return node.id;
    }

    result_t stmtVariable(VariableStatement const& stmt)
    {
        ShallowApply<ExprStructWriter> op(stream);
        auto node = id(stmt);
        stream << node.id << " [label=\"[=] | {";
        stream << stmt.variable().symbol().token().lexeme();
        if ( auto expr = stmt.initializer() ) {
            stream << " | ";
            op(*expr);
        }
        stream << "}\"]\n";

        return node.id;
    }

    result_t juncBranch(BranchJunction const& br)
    {
        ShallowApply<ExprStructWriter> op(stream);
        auto node = id(br);
        if ( auto c = br.condition() ) {
            stream << node.id << " [label=\"[?] | ";
            op(*c);
            stream << "\"]\n";
        }
        else {
            stream << node.id << " [label=\"[?]\"]\n";
        }

        return node.id;
    }

    result_t juncReturn(ReturnJunction const& ret)
    {
        ShallowApply<ExprStructWriter> op(stream);
        auto node = id(ret);
        stream << node.id << " [label=\"[:.] | ";
        op(ret.expression());
        stream << "\"]\n";

        return node.id;
    }

    result_t juncJump(JumpJunction const& jmp)
    {
        auto node = id(jmp);
        stream << node.id << " [label=\"["
            << (jmp.jumpKind() == JumpJunction::JumpKind::Continue ? "+" : "-")
            << "]\"]\n";

        return node.id;
    }

    void traceModule(Module const& mod)
    {
        if ( !mod.scope() )
            return;

        for ( auto const& decl : mod.scope()->childDeclarations() )
            dispatch(*decl);
    }

    result_t traceDefinable(DefinableDeclaration const& decl)
    {
        auto node = id(decl);
        stream << "subgraph cluster" << node.id << " {\n"
                  "label=" << quoted(decl.symbol().token().lexeme()) << "\n";
        if ( auto defn = decl.definition() )
            for ( auto const& c : defn->childDeclarations() )
                dispatch(*c);
        stream << "}\n";

        return node.id;
    }

    result_t declDataSum(DataSumDeclaration const& ds)
    {
        return traceDefinable(ds);
    }

    result_t declDataSumCtor(DataSumDeclaration::Constructor const& dsCtor)
    {
        return mkNode(dsCtor);
    }

    result_t declDataProduct(DataProductDeclaration const& dp)
    {
        return traceDefinable(dp);
    }

    result_t declField(DataProductDeclaration::Field const& dpField)
    {
        return mkNode(dpField);
    }

    result_t declSymbol(SymbolDeclaration const& sym)
    {
        return mkNode(sym);
    }

    result_t declProcedure(ProcedureDeclaration const& proc)
    {
        auto defn = proc.definition();
        if ( !defn )
            return mkNode(proc);

        auto node = id(proc);
        stream << "subgraph cluster" << node.id << " {\n"
                  "node [shape=record]\n"
                  "label=" << quoted(proc.scope().declaration()->symbol().token().lexeme()) << "\n";

        ycomb([this](auto rec, ProcedureScope const& scope) -> void {
            for ( auto bb : scope.basicBlocks() ) {
                std::string_view pred;
                if ( bb->statements() ) {
                    pred = dispatch(*bb->statements().front());
                    for ( auto stmt : bb->statements()(1, $) ) {
                        auto succ = dispatch(*stmt);
                        mkEdge(pred, succ);
                        pred = succ;
                    }
                }

                if ( bb->junction() ) {
                    auto succ = dispatch(*bb->junction());
                    if ( !pred.empty() )
                        mkEdge(pred, succ);
                }
            }

            for ( auto c : scope.childScopes() )
                rec(*c);

            for ( auto bb : scope.basicBlocks() ) {
                auto junc = bb->junction();
                if ( !junc )
                    continue;

                auto bbNode = tail(*bb);
                if ( auto br = junc->as<BranchJunction>() ) {
                    assert(br->branch(0));
                    mkEdge(bbNode.id, head(*br->branch(0)).id);

                    std::string_view mb;
                    if ( auto b = br->branch(1) )
                        mb = head(*b).id;
                    else
                        mb = head(*bb->scope()->mergeBlock()).id;

                    stream << bbNode.id << ":w -> " << mb << ":w\n";
                }
                else if ( auto j = junc->as<JumpJunction>() ) {
                    if ( auto b = j->targetBlock() ) {
                        if ( j->jumpKind() == JumpJunction::JumpKind::Break ) {
                            if ( auto m = b->scope()->mergeBlock() )
                                mkEdge(bbNode.id, head(*m).id);
                        }
                        else {
                            mkEdge(bbNode.id, head(*b).id);
                        }
                    }
                }
            }
        })(*defn);

        stream << "}\n";

        return node.id;
    }

    result_t declProcedureParameter(ProcedureParameter const& param)
    {
        return mkNode(param);
    }

    result_t declVariable(VariableDeclaration const& var)
    {
        return mkNode(var);
    }

    result_t declImport(ImportDeclaration const& imp)
    {
        return mkNode(imp);
    }

    result_t declSymbolVariable(SymbolVariable const& symVar)
    {
        return mkNode(symVar);
    }

    result_t declTemplate(TemplateDeclaration const& templ)
    {
        if ( auto defn = templ.definition() )
            for ( auto const& decl : defn->childDeclarations() )
                dispatch(*decl);

        return "";
    }
};

std::ostream& writeDot(std::ostream& stream, Module const& mod)
{
    ShallowApply<DotWriter> op(stream);
    op.getOperator().traceModule(mod);

    return stream;
}

} // namespace kyfoo::ast
