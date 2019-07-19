#include <kyfoo/ast/DotWriter.hpp>

#include <cassert>

#include <array>
#include <fstream>
#include <iomanip>
#include <map>

#include <kyfoo/String.hpp>
#include <kyfoo/ast/ControlFlow.hpp>
#include <kyfoo/ast/Declarations.hpp>
#include <kyfoo/ast/Expressions.hpp>
#include <kyfoo/ast/Module.hpp>
#include <kyfoo/ast/Scopes.hpp>
#include <kyfoo/ast/Visitors.hpp>

namespace kyfoo::ast {

void writeDot(Module const& mod, std::filesystem::path const& path)
{
    GUARD( DefaultOutStream fout, FileOutput::open(path) ) {
        throw SystemException(ec);
    }

    writeDot(fout, mod);
}

template <typename Dispatcher>
struct ExprStructWriter
{
    using Result = void;

    Dispatcher& dispatch;
    DefaultOutStream& stream;

    ExprStructWriter(Dispatcher& dispatch, DefaultOutStream& stream)
        : dispatch(dispatch)
        , stream(stream)
    {
    }

public:
    Result exprLiteral(LiteralExpression const& lit)
    {
        for ( auto c : lit.token().lexeme() ) {
            if ( c == '"' )
                stream("\\\"");
            else
                stream(c);
        }
    }

    Result exprIdentifier(IdentifierExpression const& ident)
    {
        stream(ident.token().lexeme());
    }

    void visit(Slice<Expression const*> exprs)
    {
        if ( exprs ) {
            dispatch(*exprs.front());

            for ( auto e : exprs(1, $) ) {
                stream(" | ");
                dispatch(*e);
            }
        }
    }

    Result exprTuple(TupleExpression const& tup)
    {
        switch ( tup.kind() ) {
        case TupleKind::Closed:    stream("[] | { "); break;
        case TupleKind::Open:      stream("() | { "); break;
        case TupleKind::OpenLeft:  stream("(] | { "); break;
        case TupleKind::OpenRight: stream("[) | { "); break;
        }
        visit(tup.expressions());
        stream(" }");
    }

    Result exprApply(ApplyExpression const& app)
    {
        stream("[apply] | { ");
        visit(app.expressions());
        stream(" }");
    }

    Result exprSymbol(SymbolExpression const& sym)
    {
        stream("[sym][")(sym.token().lexeme())("] | { ");
        visit(sym.expressions());
        stream(" }");
    }

    Result exprDot(DotExpression const& dot)
    {
        stream("[.] | { ");
        visit(dot.expressions());
        stream(" }");
    }

    Result exprAssign(AssignExpression const& ass)
    {
        stream("[=] | { ");
        dispatch(ass.left());
        stream(" | ");
        dispatch(ass.right());
        stream(" }");
    }

    Result exprLambda(LambdaExpression const&)
    {
        stream("[=\\>]");
    }

    Result exprArrow(ArrowExpression const& arrow)
    {
        stream("[-\\>] | { ");
        dispatch(arrow.from());
        stream(" | ");
        dispatch(arrow.to());
        stream(" }");
    }

    Result exprUniverse(UniverseExpression const& univ)
    {
        stream("[universe")(univ.level())("]");
    }
};

template <typename Dispatcher>
struct DotWriter;

template <typename Dispatcher>
struct NodeWriter
{
    using Result = void;

    Dispatcher& dispatch;
    DefaultOutStream& stream;
    stringv id;

    NodeWriter(Dispatcher& dispatch, DefaultOutStream& stream, stringv id)
        : dispatch(dispatch)
        , stream(stream)
        , id(id)
    {
    }

    void beginAttrs(stringv label)
    {
        stream(id)(" [label=")(quoted(label));
    }

    void endAttrs()
    {
        stream("]\n");
    }

    void attr(stringv key, stringv value)
    {
        stream(",")(key)("=")(value);
    }

    Result exprLiteral(LiteralExpression const& lit)
    {
        beginAttrs(lit.token().lexeme());
        endAttrs();
    }

    Result exprIdentifier(IdentifierExpression const& ident)
    {
        beginAttrs(ident.token().lexeme());
        endAttrs();
    }

    Result exprTuple(TupleExpression const&)
    {
        beginAttrs("tuple");
        endAttrs();
    }

    Result exprApply(ApplyExpression const&)
    {
        beginAttrs("apply");
        endAttrs();
    }

    Result exprSymbol(SymbolExpression const&)
    {
        beginAttrs("symexpr");
        endAttrs();
    }

    Result exprDot(DotExpression const&)
    {
        beginAttrs("dot");
        endAttrs();
    }

    Result exprAssign(AssignExpression const&)
    {
        beginAttrs("assign");
        endAttrs();
    }

    Result exprLambda(LambdaExpression const&)
    {
        beginAttrs("lambda");
        endAttrs();
    }

    Result exprArrow(ArrowExpression const&)
    {
        beginAttrs("arrow");
        endAttrs();
    }

    Result exprUniverse(UniverseExpression const&)
    {
        beginAttrs("universe");
        endAttrs();
    }

    Result stmtExpression(ExpressionStatement const&)
    {
        beginAttrs("stmt");
        endAttrs();
        stream("subgraph ")(id)(" {\n");
        stream("}\n");
    }

    Result stmtVariable(VariableStatement const&)
    {
        beginAttrs("stmt-var");
        endAttrs();
        stream("subgraph ")(id)(" {\n");
        stream("}\n");
    }

    Result juncBranch(BranchJunction const&)
    {
        beginAttrs("br");
        endAttrs();
    }

    Result juncReturn(ReturnJunction const&)
    {
        beginAttrs("return");
        endAttrs();
    }

    Result juncJump(JumpJunction const&)
    {
        beginAttrs("jump");
        endAttrs();
    }

    void beginDecl(Declaration const& decl)
    {
        beginAttrs(decl.symbol().token().lexeme());
    }

    Result declDataType(DataTypeDeclaration const& dt)
    {
        beginDecl(dt);
        endAttrs();
    }

    Result declField(Field const& dpField)
    {
        beginDecl(dpField);
        endAttrs();
    }

    Result declSymbol(SymbolDeclaration const& sym)
    {
        beginDecl(sym);
        endAttrs();
    }

    Result declProcedure(ProcedureDeclaration const& proc)
    {
        stringv label = proc.symbol().token().lexeme();
        if ( auto decl = proc.scope().declaration() )
            if ( auto templ = decl->as<TemplateDeclaration>() )
                label = templ->symbol().token().lexeme();

        beginAttrs(label);
        endAttrs();
    }

    Result declProcedureParameter(ProcedureParameter const& param)
    {
        beginDecl(param);
        endAttrs();
    }

    Result declVariable(VariableDeclaration const& var)
    {
        beginDecl(var);
        endAttrs();
    }

    Result declImport(ImportDeclaration const& imp)
    {
        beginDecl(imp);
        endAttrs();
    }

    Result declSymbolVariable(SymbolVariable const& symVar)
    {
        beginDecl(symVar);
        endAttrs();
    }

    Result declTemplate(TemplateDeclaration const& templ)
    {
        beginDecl(templ);
        endAttrs();
    }
};

template <typename Dispatcher>
struct DotWriter
{
    using Result = stringv;
    Dispatcher& dispatch;
    DefaultOutStream& stream;

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

    DotWriter(Dispatcher& dispatch, DefaultOutStream& stream)
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
        stream("digraph {\n"
               "compound=true\n");
    }

    void endFile()
    {
        stream("}\n");
    }

    struct NodeID {
        stringv id;
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
        if ( bb.statements() )
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
    stringv mkNode(T const& item)
    {
        auto node = id(item);
        if ( node.isNew ) {
            ShallowApply<NodeWriter> op(stream, node.id);
            op(item);
        }

        return node.id;
    }

    void mkEdge(stringv from, stringv to)
    {
        stream(from)(" -> ")(to)('\n');
    }

    Result exprLiteral(LiteralExpression const& lit)
    {
        return mkNode(lit);
    }

    Result exprIdentifier(IdentifierExpression const& ident)
    {
        return mkNode(ident);
    }

    Result exprTuple(TupleExpression const& tup)
    {
        auto node = mkNode(tup);
        for ( auto const& e : tup.expressions() )
            mkEdge(node, dispatch(*e));

        return node;
    }

    Result exprApply(ApplyExpression const& app)
    {
        auto node = mkNode(app);
        for ( auto const& e : app.expressions() )
            mkEdge(node, dispatch(*e));

        return node;
    }

    Result exprSymbol(SymbolExpression const& sym)
    {
        auto node = mkNode(sym);
        for ( auto const& e : sym.expressions() )
            mkEdge(node, dispatch(*e));

        return node;
    }

    Result exprDot(DotExpression const& dot)
    {
        auto node = mkNode(dot);
        for ( auto const& e : dot.expressions() )
            mkEdge(node, dispatch(*e));

        return node;
    }

    Result exprAssign(AssignExpression const& ass)
    {
        auto node = mkNode(ass);
        mkEdge(node, dispatch(ass.left()));
        mkEdge(node, dispatch(ass.right()));

        return node;
    }

    Result exprLambda(LambdaExpression const& lam)
    {
        auto node = mkNode(lam);
        mkEdge(node, declProcedure(lam.procedure()));

        return node;
    }

    Result exprArrow(ArrowExpression const& arrow)
    {
        auto node = mkNode(arrow);
        mkEdge(node, dispatch(arrow.from()));
        mkEdge(node, dispatch(arrow.to()));

        return node;
    }

    Result exprUniverse(UniverseExpression const& univ)
    {
        return mkNode(univ);
    }

    Result stmtExpression(ExpressionStatement const& stmt)
    {
        ShallowApply<ExprStructWriter> op(stream);
        auto node = id(stmt);
        stream(node.id)(" [label=\"");
        op(stmt.expression());
        stream("\"]\n");

        return node.id;
    }

    Result stmtVariable(VariableStatement const& stmt)
    {
        ShallowApply<ExprStructWriter> op(stream);
        auto node = id(stmt);
        stream(node.id)(" [label=\"[=] | {")(stmt.variable().symbol().token().lexeme());
        if ( auto expr = stmt.initializer() ) {
            stream(" | ");
            op(*expr);
        }
        stream("}\"]\n");

        return node.id;
    }

    Result juncBranch(BranchJunction const& br)
    {
        ShallowApply<ExprStructWriter> op(stream);
        auto node = id(br);
        if ( auto c = br.condition() ) {
            stream(node.id)(" [label=\"[?] | ");
            op(*c);
            stream("\"]\n");
        }
        else {
            stream(node.id)(" [label=\"[?]\"]\n");
        }

        return node.id;
    }

    Result juncReturn(ReturnJunction const& ret)
    {
        ShallowApply<ExprStructWriter> op(stream);
        auto node = id(ret);
        stream(node.id)(" [label=\"[:.] | ");
        op(ret.expression());
        stream("\"]\n");

        return node.id;
    }

    Result juncJump(JumpJunction const& jmp)
    {
        auto node = id(jmp);
        stream(node.id)
            (" [label=\"[")
            (jmp.jumpKind() == JumpJunction::JumpKind::Continue ? "+" : "-")
            ("]\"]\n");

        return node.id;
    }

    void traceModule(Module const& mod)
    {
        if ( !mod.scope() )
            return;

        for ( auto const& decl : mod.scope()->childDeclarations() )
            dispatch(*decl);
    }

    Result traceDefinable(DefinableDeclaration const& decl)
    {
        auto node = id(decl);
        stream("subgraph cluster")(node.id)(" {\n"
               "label=")(quoted(decl.symbol().token().lexeme()))("\n");
        if ( auto defn = decl.definition() )
            for ( auto const& c : defn->childDeclarations() )
                dispatch(*c);
        stream("}\n");

        return node.id;
    }

    Result declDataType(DataTypeDeclaration const& dt)
    {
        return traceDefinable(dt);
    }

    Result declField(Field const& dpField)
    {
        return mkNode(dpField);
    }

    Result declSymbol(SymbolDeclaration const& sym)
    {
        return mkNode(sym);
    }

    Result declProcedure(ProcedureDeclaration const& proc)
    {
        auto defn = proc.definition();
        if ( !defn )
            return mkNode(proc);

        auto node = id(proc);
        stream("subgraph cluster")(node.id)(" {\n"
               "node [shape=record]\n"
               "label=")(quoted(proc.scope().declaration()->symbol().token().lexeme()))("\n");

        ycomb([this](auto rec, ProcedureScope const& scope) -> void {
            for ( auto bb : scope.basicBlocks() ) {
                stringv pred;
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
                    if ( pred )
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
                    if ( br->branch(0) ) {
                        mkEdge(bbNode.id, head(*br->branch(0)).id);

                        stringv mb;
                        if ( auto b = br->branch(1) )
                            mb = head(*b).id;
                        else
                            mb = head(*bb->scope()->mergeBlock()).id;

                        stream(bbNode.id)(":w -> ")(mb)(":w\n");
                    }
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

        stream("}\n");

        return node.id;
    }

    Result declProcedureParameter(ProcedureParameter const& param)
    {
        return mkNode(param);
    }

    Result declVariable(VariableDeclaration const& var)
    {
        return mkNode(var);
    }

    Result declImport(ImportDeclaration const& imp)
    {
        return mkNode(imp);
    }

    Result declSymbolVariable(SymbolVariable const& symVar)
    {
        return mkNode(symVar);
    }

    Result declTemplate(TemplateDeclaration const& templ)
    {
        if ( auto defn = templ.definition() )
            for ( auto const& decl : defn->childDeclarations() )
                dispatch(*decl);

        return "";
    }
};

DefaultOutStream& writeDot(DefaultOutStream& stream, Module const& mod)
{
    ShallowApply<DotWriter> op(stream);
    op.getOperator().traceModule(mod);

    return stream;
}

} // namespace kyfoo::ast
