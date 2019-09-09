#include <kyfoo/ast/Semantics.hpp>

#include <algorithm>
#include <functional>
#include <set>

#include <kyfoo/Algorithms.hpp>
#include <kyfoo/Diagnostics.hpp>

#include <kyfoo/lexer/TokenKind.hpp>

#include <kyfoo/ast/Axioms.hpp>
#include <kyfoo/ast/Context.hpp>
#include <kyfoo/ast/Expressions.hpp>
#include <kyfoo/ast/Module.hpp>
#include <kyfoo/ast/Scopes.hpp>
#include <kyfoo/ast/Symbol.hpp>
#include <kyfoo/ast/Variance.hpp>
#include <kyfoo/ast/Visitors.hpp>

namespace kyfoo {
    namespace ast {

namespace {
    template <typename T>
    bool compare(Slice<Expression const*> lhs,
                 Slice<Expression const*> rhs,
                 T&& op)
    {
        if ( lhs.card() != rhs.card() )
            return false;

        if ( !lhs )
            return true;

        auto const card = lhs.card();
        for ( uz i = 0; i < card; ++i )
            if ( !op(*lhs[i], *rhs[i]) )
                return false;

        return true;
    }

} // namespace

//
// SymbolDependencyTracker

SymbolDependencyTracker::SymbolDependencyTracker(Module& mod, Diagnostics& dgn)
    : mod(mod)
    , dgn(dgn)
{
}

SymbolDependencyTracker::SymGroup* SymbolDependencyTracker::create(std::string name, uz arity)
{
    groups.append(mk<SymGroup>(std::move(name), arity));
    return groups.back().get();
}

SymbolDependencyTracker::SymGroup* SymbolDependencyTracker::findOrCreate(stringv name, uz arity)
{
    for ( auto& e : groups )
        if ( e->name == name && e->arity == arity )
            return e.get();

    return create(mkString(name), arity);
}

void SymbolDependencyTracker::add(Declaration& decl)
{
    auto group = findOrCreate(decl.symbol().token().lexeme(), decl.symbol().prototype().pattern().card());
    group->add(decl);
}

SymRes SymbolDependencyTracker::addDependency(Declaration& dependent,
                                              stringv name,
                                              uz arity)
{
    auto group = findOrCreate(dependent.symbol().token().lexeme(), dependent.symbol().prototype().pattern().card());
    auto dependency = findOrCreate(name, arity);

    dependency->addDependent(*group);

    if ( group->level <= dependency->level ) {
        if ( !group->defer(group, dependency->level + 1) ) {
            auto err = dgn.error(mod, diag::cycle, dependent.symbol().token());
            for ( auto const& d : dependency->declarations )
                err.see(*d);

            return SymRes::Fail;
        }
    }

    return SymRes::Success;
}

void SymbolDependencyTracker::sortPasses()
{
    std::stable_sort(begin(groups), end(groups),
                     [](auto const& lhs, auto const& rhs) { return lhs->level < rhs->level; });
}

//
// operators

template <typename Dispatcher>
struct SymbolDependencyBuilder
{
    using Result = SymRes;
    Dispatcher& dispatch;
    SymbolDependencyTracker& tracker;
    Declaration& dependent;

    struct LocalDecl {
        ab<stringv> names;
    };

    ab<LocalDecl> localDecls;

    SymbolDependencyBuilder(Dispatcher& dispatch,
                            SymbolDependencyTracker& tracker,
                            Declaration& dependent)
        : dispatch(dispatch)
        , tracker(tracker)
        , dependent(dependent)
    {
    }

private:
    Result addDep(stringv name, uz arity)
    {
        if ( arity == 0 )
            for ( auto localDecl : Retro(localDecls()) )
                for ( auto const& n : localDecl.names )
                    if ( n == name )
                        return SymRes::Success;

        return tracker.addDependency(dependent, name, arity);
    }

    struct Pop {
        SymbolDependencyBuilder& outer;
        Pop(SymbolDependencyBuilder& outer) : outer(outer) {}
        ~Pop() { outer.localDecls.pop(); }
    };

    Pop pushLocal(Declaration const&)
    {
        localDecls.append();
        return Pop(*this);
    }

    void pushName(stringv name)
    {
        localDecls.back().names.append(name);
    }

    SymRes traceDecl(Declaration const& decl)
    {
        return traceSymbol(decl.symbol());
    }

    SymRes traceSymbol(Symbol const& sym)
    {
        return tracePrototype(sym.prototype());
    }

    SymRes tracePrototype(PatternsPrototype const& proto)
    {
        SymRes ret = SymRes::Success;
        for ( auto const& p : proto.pattern() )
            ret |= dispatch(*p);

        return ret;
    }

    SymRes traceBasicBlock(BasicBlock const& bb)
    {
        SymRes ret = SymRes::Success;
        for ( auto stmt : bb.statements() )
            ret |= dispatch(*stmt);

        if ( auto junc = bb.junction() )
            ret |= dispatch(*junc);

        return ret;
    }

    // expressions
public:
    Result exprLiteral(LiteralExpression const&)
    {
        return SymRes::Success;
    }

    Result exprIdentifier(IdentifierExpression const& p)
    {
        switch (p.token().kind())
        {
        case lexer::TokenKind::Identifier:
            return addDep(p.token().lexeme(), 0);
        case lexer::TokenKind::MetaVariable:
            pushName(p.token().lexeme());

        default:
            return SymRes::Success;
        }
    }

    Result exprTuple(TupleExpression const& t)
    {
        SymRes ret = SymRes::Success;
        for ( auto const& e : t.expressions() )
            ret |= dispatch(*e);

        return ret;
    }

    Result exprApply(ApplyExpression const& a)
    {
        // todo: failover to implicit proc call semantics
        SymRes ret = SymRes::Success;
        auto subject = a.expressions()[0]->as<LiteralExpression>();
        if ( subject && subject->token().kind() == lexer::TokenKind::Identifier )
            ret |= addDep(subject->token().lexeme(), a.expressions().card() - 1);

        for ( auto const& e : a.expressions() )
            ret |= dispatch(*e);

        return ret;
    }

    Result exprSymbol(SymbolExpression const& s)
    {
        SymRes ret = SymRes::Success;
        if ( s.token().kind() == lexer::TokenKind::Identifier )
            ret |= addDep(s.token().lexeme(), s.expressions().card());

        for ( auto const& e : s.expressions() )
            ret |= dispatch(*e);

        return ret;
    }

    Result exprDot(DotExpression const& d)
    {
        SymRes ret = SymRes::Success;
        for ( auto const& e : d.expressions() )
            ret |= dispatch(*e);

        return ret;
    }

    Result exprAssign(AssignExpression const& v)
    {
        return dispatch(v.left()) | dispatch(v.right());
    }

    Result exprLambda(LambdaExpression const& l)
    {
        return declProcedure(l.procedure());
    }

    Result exprArrow(ArrowExpression const& a)
    {
        return dispatch(a.from()) | dispatch(a.to());
    }

    Result exprUniverse(UniverseExpression const&)
    {
        return SymRes::Success;
    }

    // statements
public:
    Result stmtExpression(ExpressionStatement const& s)
    {
        return dispatch(s.expression());
    }

    Result stmtVariable(VariableStatement const& s)
    {
        auto ret = dispatch(s.variable());
        if ( s.initializer() )
            ret |= *s.initializer();

        return ret;
    }

    Result juncBranch(BranchJunction const& b)
    {
        SymRes ret = SymRes::Success;
        if ( b.condition() )
            ret |= dispatch(*b.condition());

        for ( uz i = 0; i < 2; ++i )
            if ( auto bb = b->branch(i) )
                ret |= traceBasicBlock(*bb);

        return ret;
    }

    Result juncReturn(ReturnJunction const& r)
    {
        SymRes ret = SymRes::Success;
        if ( r.expression() )
            ret |= dispatch(r.expression());

        return ret;
    }

    Result juncJump(JumpJunction const&)
    {
        return SymRes::Success;
    }

    // declarations
public:
    Result declDataType(DataTypeDeclaration const& dt)
    {
        REVERT = pushLocal(dt);
        return traceDecl(dt);
    }

    Result declField(Field const& dpField)
    {
        REVERT = pushLocal(dpField);
        return traceDecl(dpField);
    }

    Result declSymbol(SymbolDeclaration const& s)
    {
        REVERT = pushLocal(s);
        auto ret = traceDecl(s);
        if ( s.expression() )
            ret |= dispatch(*s.expression());

        return ret;
    }

    Result declProcedure(ProcedureDeclaration const& proc)
    {
        REVERT = pushLocal(proc);
        SymRes ret = traceDecl(proc);
        if ( proc.returnType() )
            ret |= dispatch(*proc.returnType());

        return ret;
    }

    Result declProcedureParameter(ProcedureParameter const&)
    {
        return SymRes::Success;
    }

    Result declVariable(VariableDeclaration const& var)
    {
        REVERT = pushLocal(var);
        auto ret = traceDecl(var);
        for ( auto const& c : var.constraints() )
            ret |= dispatch(*c);

        return ret;
    }

    Result declImport(ImportDeclaration const&)
    {
        return SymRes::Success;
    }

    Result declSymbolVariable(SymbolVariable const&)
    {
        return SymRes::Success;
    }

    Result declTemplate(TemplateDeclaration const& templ)
    {
        REVERT = pushLocal(templ);
        Result ret = traceDecl(templ);
        for ( auto const& d : templ.definition()->childDeclarations() )
            ret |= dispatch(*d);

        return ret;
    }
};

SymRes traceDependencies(SymbolDependencyTracker& tracker, Declaration& decl)
{
    DeepApply<SymbolDependencyBuilder> op(tracker, decl);
    tracker.add(decl);
    auto ret = op(decl);

    // data-sum ctors are visible from their parent's scope
    if ( auto dt = decl.as<DataTypeDeclaration>() ) {
        if ( auto defn = dt->definition() ) {
            for ( auto v : defn->variations() ) {
                tracker.add(*v);
                ret |= op(*v);
            }
        }
    }

    return ret;
}

struct MatchEquivalent
{
    explicit MatchEquivalent()
    {
    }

    // Literal match

    bool operator()(LiteralExpression const& l, LiteralExpression const& r)
    {
        return variance(l.token(), r.token()).exact();
    }

    // Tuple match

    bool operator()(TupleExpression const& l, TupleExpression const& r)
    {
        if ( l.kind() != r.kind() )
            return false;

        return matchEquivalent(l.expressions(), r.expressions());
    }

    // Apply match

    bool operator()(ApplyExpression const& l, ApplyExpression const& r)
    {
        return matchEquivalent(l.expressions(), r.expressions());
    }

    // Symbol match

    bool operator()(SymbolExpression const& l, SymbolExpression const& r)
    {
        if ( !variance(l.token(), r.token()).exact() )
            return false;

        return matchEquivalent(l.expressions(), r.expressions());
    }

    // else

    bool operator()(Expression const& l_, Expression const& r_)
    {
        auto l = resolveIndirections(&l_);
        auto r = resolveIndirections(&r_);

        ENFORCE(l && r, "unresolved indirection");

        if ( auto leftDecl = getDeclaration(*l) ) {
            auto rightDecl = getDeclaration(*r);
            if ( !rightDecl )
                return false;

            // todo: check var constraints
            if ( leftDecl->kind() == Declaration::Kind::SymbolVariable )
                return rightDecl->kind() == Declaration::Kind::SymbolVariable;

            if ( rightDecl->kind() == Declaration::Kind::SymbolVariable )
                return true;

            return leftDecl == rightDecl;
        }

        if ( l != &l_ || r != &r_ )
            return matchEquivalent(*l, *r);

        return false;
    }
};

bool matchEquivalent(Expression const& lhs, Expression const& rhs)
{
    MatchEquivalent op;
    return noncommute(op, lhs, rhs);
}

bool matchEquivalent(Slice<Expression const*> lhs, Slice<Expression const*> rhs)
{
    return compare(lhs, rhs, [](auto& l, auto& r) { return matchEquivalent(l, r); });
}

Expression const* lookThrough(Expression const* expr)
{
    auto decl = getDeclaration(expr);
    if ( !decl || !hasIndirection(decl->kind()) )
        return expr;

    return lookThrough(decl);
}

Expression const* lookThrough(Declaration const* decl)
{
    Expression const* ret = nullptr;
    while ( decl ) {
        Expression const* expr = nullptr;
        if ( auto s = decl->as<SymbolDeclaration>() ) {
            expr = s->expression();
        }
        else if ( auto symVar = decl->as<SymbolVariable>() ) {
            if ( symVar->boundExpression() ) {
                expr = symVar->boundExpression();
            }
        }

        if ( !expr )
            break;

        ret = expr;
        auto id = identify(*ret);
        if ( !id )
            break;

        decl = id->declaration();
    }

    return ret;
}

Declaration const* resolveIndirections(Declaration const* decl)
{
    while ( decl ) {
        Expression const* expr = nullptr;
        switch (decl->kind()) {
        case Declaration::Kind::Symbol:
            expr = static_cast<SymbolDeclaration const*>(decl)->expression();
            break;
        case Declaration::Kind::SymbolVariable:
            expr = static_cast<SymbolVariable const*>(decl)->boundExpression();
            break;
        default:
            return decl;
        }

        if ( !expr )
            return decl;

        auto next = getDeclaration(expr);
        if ( !next )
            return decl;

        decl = next;
    }

    return nullptr;
}

Declaration const* resolveIndirections(Declaration const& decl)
{
    return resolveIndirections(&decl);
}

Expression const* resolveIndirections(Expression const* expr)
{
    while ( expr ) {
        auto decl = getDeclaration(*expr);
        if ( !decl )
            return expr;

        Expression const* next = nullptr;
        switch ( decl->kind() ) {
        case Declaration::Kind::Symbol:
            next = static_cast<SymbolDeclaration const*>(decl)->expression();
            break;
        case Declaration::Kind::SymbolVariable:
            next = static_cast<SymbolVariable const*>(decl)->boundExpression();
            break;

        default:
            break;
        }

        if ( !next )
            return expr;

        expr = next;
    }

    return nullptr;
}

Expression* resolveIndirections(Expression* expr)
{
    return const_cast<Expression*>(resolveIndirections(const_cast<Expression const*>(expr)));
}

Expression const* resolveIndirections(Expression const& expr)
{
    return resolveIndirections(&expr);
}

bool needsSubstitution(Expression const& expr)
{
    return lookThrough(&expr) == nullptr;
}

bool needsSubstitution(Declaration const& decl)
{
    return hasIndirection(decl.kind()) && lookThrough(&decl) == nullptr;
}

bool needsSubstitutions(Symbol const& sym)
{
    for ( auto& v : sym.prototype().symbolVariables() )
        if ( !v->boundExpression() )
            return true;

    return false;
}

bool requiresSubstitutions(Symbol const& sym)
{
    return sym.prototype().symbolVariables().card();
}

bool hasSubstitutions(Symbol const& sym)
{
    return requiresSubstitutions(sym) && !needsSubstitutions(sym);
}

Symbol const* rootTemplate(Symbol const& symbol)
{
    auto ret = &symbol;
    while ( ret->prototypeParent() )
        ret = ret->prototypeParent();

    return ret;
}

bool descendsFromTemplate(Symbol const& parent, Symbol const& instance)
{
    auto p = &instance;
    while ( p ) {
        if ( p == &parent )
            return true;

        p = p->prototypeParent();
    }

    return false;
}

Expression const* refType(Declaration const& decl)
{
    if ( rootTemplate(decl.symbol()) == &decl.scope().module().axioms().intrinsic(intrin::type::ReferenceTemplate)->symbol() )
        return &removeReference(decl);

    return nullptr;
}

Expression const* refType(Expression const& expr)
{
    if ( auto d = getDeclaration(expr) )
        return refType(*d);

    return nullptr;
}

Expression const& removeReference(Declaration const& decl)
{
    return *decl.symbol().prototype().pattern().front();
}

Declaration const* removeAllReferences(Declaration const& decl)
{
    auto ret = resolveIndirections(decl);
    if ( !ret )
        return nullptr;

    while ( auto r = refType(*ret) ) {
        ret = getDeclaration(resolveIndirections(*r));
        if ( !ret )
            break;
    }

    return ret;
}

Expression const* removeAllReferences(Expression const& expr)
{
    auto ret = resolveIndirections(expr);
    if ( !ret )
        return nullptr;

    while ( auto r = refType(*ret) )
        ret = resolveIndirections(r);

    return ret;
}

Scope const* staticAccessorScope(Expression const& expr)
{
    if ( auto decl = resolveIndirections(getDeclaration(expr)) )
        return staticAccessorScope(*decl);

    return nullptr;
}

Scope const* staticAccessorScope(Declaration const& decl)
{
    return getDefinition(*removeAllReferences(*resolveIndirections(decl)));
}

AccessorScope instanceAccessorScope(Expression const& expr)
{
    auto e = resolveIndirections(expr);
    if ( auto scope = staticAccessorScope(*e) )
        return { nullptr, scope };

    return { e, staticAccessorScope(*e->type()) };
}

TemplateDeclaration const* procTemplate(ProcedureDeclaration const& proc)
{
    if ( !proc.scope().declaration() )
        return nullptr;

    return proc.scope().declaration()->as<TemplateDeclaration>();
}

DataTypeDeclaration const* outerDataDeclaration(Declaration const& decl)
{
    for ( auto scope = &decl.scope(); scope; scope = scope->parent() ) {
        if ( !scope->declaration() )
            return nullptr;

        if ( auto dt = scope->declaration()->as<DataTypeDeclaration>() )
            return dt;
    }

    return nullptr;
}

DataTypeDeclaration* outerDataDeclaration(Declaration& decl)
{
    return const_cast<DataTypeDeclaration*>(outerDataDeclaration(const_cast<Declaration const&>(decl)));
}

Declaration const* callingContextDeclaration(Declaration const& decl)
{
    for ( auto scope = &decl.scope(); scope; scope = scope->parent() ) {
        if ( !scope->declaration() )
            return nullptr;

        if ( isCallableDeclaration(scope->declaration()->kind()) )
            return scope->declaration();
    }

    return nullptr;
}

Declaration* callingContextDeclaration(Declaration& decl)
{
    return const_cast<Declaration*>(callingContextDeclaration(const_cast<Declaration const&>(decl)));
}

DataTypeDeclaration const* methodType(ProcedureDeclaration const& proc)
{
    auto decl = outerDataDeclaration(proc);
    if ( !decl )
        return nullptr;

    if ( auto dt = decl->as<DataTypeDeclaration>() )
        return dt;

    return nullptr;
}

Expression const* dataType(Expression const& expr_)
{
    auto expr = resolveIndirections(&expr_);
    if ( auto decl = getDeclaration(*expr) ) {
        if ( decl->as<DataTypeDeclaration>() )
            return expr;

        return nullptr;
    }

    if ( auto tup = expr->as<TupleExpression>() ) {
        for ( auto const& e : tup->expressions() )
            if ( !dataType(*e) )
                return nullptr;

        return expr;
    }

    if ( expr->kind() == Expression::Kind::Arrow )
        return expr;

    return nullptr;
}

uz variationOrdinal(DataTypeDeclaration const& dt)
{
    if ( auto s = dt.super() )
        return indexOf(s->definition()->variations(), &dt);

    return 1;
}

UnificationResult unify(Context& ctx, Report::Subject gov, Slice<Expression const*> exprs)
{
    ab<Expression const*> potentialTypes;
    potentialTypes.reserve(exprs.card());
    for ( auto e : exprs )
        potentialTypes.append(e->type()); // todo: type?

    if ( !potentialTypes ) {
        ctx.error(diag::no_type, gov);
        return { SymRes::Fail, nullptr };
    }

    if ( potentialTypes.card() == 1 )
        return { SymRes::Success, potentialTypes.front() };

    auto commonKind = potentialTypes.front()->kind();
    auto const card = potentialTypes.card();
    for ( uz i = 1; i < card; ++i ) {
        if ( potentialTypes[i]->kind() != commonKind ) {
            ctx.error(diag::conflict_types, gov)
                .see(ctx.resolver().scope(), *potentialTypes.front())
                .see(ctx.resolver().scope(), *potentialTypes[i]);
            return { SymRes::Fail, nullptr };
        }
    }

    if ( commonKind != Expression::Kind::Identifier ) {
        ctx.error(diag::not_implemented, gov);
        return { SymRes::Fail, nullptr };
    }

    struct ExprType {
        Expression const* expr;
        Declaration const* type;
    } common { potentialTypes.front(), getDeclaration(potentialTypes.front()) };
    for ( uz i = 1; i < card; ++i ) {
        auto type = getDeclaration(*potentialTypes[i]);
        auto const v = variance(ctx, *common.type, *type);
        if ( v.contravariant() ) {
            common = ExprType{ potentialTypes[i], type };
        }
        else if ( v.invariant() ) {
            ctx.error(diag::conflict_types, gov)
                .see(ctx.resolver().scope(), *common.expr)
                .see(ctx.resolver().scope(), *potentialTypes[i]);

            return { SymRes::Fail, nullptr };
        }
    }

    return { SymRes::Success, common.expr };
}

template <typename Dispatcher>
struct MetaVariableVisitor
{
    using Result = bool;
    Dispatcher& dispatch;
    
    using Visitor = std::function<void(IdentifierExpression&)>;
    Visitor visitor;

    MetaVariableVisitor(Dispatcher& dispatch, Visitor visitor)
        : dispatch(dispatch)
        , visitor(visitor)
    {
    }

    Result exprLiteral(LiteralExpression&)
    {
        return false;
    }

    Result exprIdentifier(IdentifierExpression& p)
    {
        if ( p.token().kind() == lexer::TokenKind::MetaVariable ) {
            visitor(p);
            return true;
        }

        return false;
    }

    Result exprTuple(TupleExpression& t)
    {
        return dispatch(t.expressions());
    }

    Result exprApply(ApplyExpression& a)
    {
        return dispatch(a.expressions());
    }

    Result exprSymbol(SymbolExpression& s)
    {
        return dispatch(s.expressions());
    }

    Result exprDot(DotExpression& d)
    {
        return dispatch(d.expressions());
    }

    Result exprAssign(AssignExpression& v)
    {
        return dispatch(v.left()) | dispatch(v.right());
    }

    Result exprLambda(LambdaExpression& l)
    {
        Result ret = false;
        for ( auto p : l.procedure().parameters() )
            ret |= dispatch(p->constraints());

        for ( auto c : l.procedure().result()->constraints() )
            ret |= dispatch(*c);

        // todo: defn
        return ret;
    }

    Result exprArrow(ArrowExpression& a)
    {
        return dispatch(a.from()) | dispatch(a.to());
    }

    Result exprUniverse(UniverseExpression&)
    {
        return false;
    }

    Result stmtExpression(ExpressionStatement& s)
    {
        return dispatch(s.expression());
    }

    Result stmtVariable(VariableStatement& s)
    {
        auto ret = dispatch(s.variable());
        if ( s.initializer() )
            ret |= dispatch(*s.initializer());

        return ret;
    }

    Result juncBranch(BranchJunction& b)
    {
        Result ret = false;
        if ( b.condition() )
            ret |= dispatch(*b.condition());

        for ( uz i = 0; i < 2; ++i )
            if ( b.branch(i) )
                ret |= dispatch.procScope(*b.branch(i)->scope());

        return ret;
    }

    Result juncReturn(ReturnJunction& r)
    {
        if ( r.expression() )
            return dispatch(r.expression());

        return false;
    }

    Result juncJump(JumpJunction&)
    {
        return false;
    }
};

template <typename F>
void visitMetaVariables(Expression& expr, F&& f)
{
    DeepApply<MetaVariableVisitor> op(f);
    op(expr);
}

ab<IdentifierExpression*> gatherMetaVariables(Expression& expr)
{
    ab<IdentifierExpression*> ret;
    visitMetaVariables(expr, [&ret](IdentifierExpression& p) {
        ret.append(&p);
    });

    return ret;
}

template <typename Dispatcher>
struct HasMetaVariable
{
    using Result = bool;
    Dispatcher& dispatch;

    HasMetaVariable(Dispatcher& dispatch)
        : dispatch(dispatch)
    {
    }

    Result exprLiteral(LiteralExpression const&)
    {
        return false;
    }

    Result exprIdentifier(IdentifierExpression const& p)
    {
        return p.token().kind() == lexer::TokenKind::MetaVariable;
    }

    Result exprTuple(TupleExpression const& t)
    {
        for ( auto const& e : t.expressions() )
            if ( dispatch(*e) )
                return true;

        return false;
    }

    Result exprApply(ApplyExpression const& a)
    {
        for ( auto const& e : a.expressions() )
            if ( dispatch(*e) )
                return true;

        return false;
    }

    Result exprSymbol(SymbolExpression const& s)
    {
        for ( auto const& e : s.expressions() )
            if ( dispatch(*e) )
                return true;

        return false;
    }

    Result exprDot(DotExpression const& d)
    {
        for ( auto const& e : d.expressions() )
            if ( dispatch(*e) )
                return true;

        return false;
    }

    Result exprAssign(AssignExpression const& v)
    {
        return dispatch(v.left()) || dispatch(v.right());
    }

    Result exprLambda(LambdaExpression const& l)
    {
        for ( auto p : l.procedure().parameters() ) {
            for ( auto c : p->constraints() ) {
                if ( dispatch(*c) )
                    return true;
            }
        }

        for ( auto c : l.procedure().result()->constraints() )
            if ( dispatch(*c) )
                return true;

        return false;
    }

    Result exprArrow(ArrowExpression const& a)
    {
        return dispatch(a.from()) || dispatch(a.to());
    }

    Result exprUniverse(UniverseExpression const&)
    {
        return false;
    }

    Result stmtExpression(ExpressionStatement const& s)
    {
        return dispatch(s.expression());
    }

    Result stmtVariable(VariableStatement const& s)
    {
        return dispatch(s.variable()) | dispatch(s.initializer());
    }

    Result juncBranch(BranchJunction const& b)
    {
        if ( b.condition() && dispatch(*b.condition()) )
            return true;

        for ( uz i = 0; i < 2; ++i ) {
            if ( auto bb = b->branch(i) ) {
                if ( traceBasicBlock(*bb) )
                    return true;
            }
        }

        return false;
    }

    Result juncReturn(ReturnJunction const& r)
    {
        if ( r.expression() )
            return dispatch(r.expression());

        return false;
    }

    Result juncJump(JumpJunction const&)
    {
        return false;
    }
};

bool hasMetaVariable(Expression const& expr)
{
    ShallowApply<HasMetaVariable> op;
    return op(expr);
}

template <typename Dispatcher>
struct FrontToken
{
    using Result = lexer::Token const&;

    Dispatcher& dispatch;

    FrontToken(Dispatcher& dispatch)
        : dispatch(dispatch)
    {
    }

    Result exprLiteral(LiteralExpression const& p)
    {
        return p.token();
    }

    Result exprIdentifier(IdentifierExpression const& p)
    {
        return p.token();
    }

    Result exprTuple(TupleExpression const& t)
    {
        if ( !t.expressions() )
            return t.openToken();

        return dispatch(*t.expressions()[0]);
    }

    Result exprApply(ApplyExpression const& a)
    {
        return dispatch(*a.expressions()[0]);
    }

    Result exprSymbol(SymbolExpression const& s)
    {
        if ( s.token().kind() != lexer::TokenKind::Undefined )
            return s.token();

        if ( !s.expressions() )
            return s.openToken();

        return dispatch(*s.expressions()[0]);
    }

    Result exprDot(DotExpression const& d)
    {
        return dispatch(*d.expressions()[0]);
    }

    Result exprAssign(AssignExpression const& v)
    {
        return dispatch(v.left());
    }

    Result exprLambda(LambdaExpression const& l)
    {
        if ( l.procedure().parameters() )
            return l.procedure().parameters().front()->symbol().token();

        auto bb = l.procedure().definition()->as<ProcedureScope>()->basicBlocks().front();
        if ( !bb->statements() )
            return dispatch(*bb->junction());

        return dispatch(*bb->statements().front());
    }

    Result exprArrow(ArrowExpression const& a)
    {
        return dispatch(a.from());
    }

    Result exprUniverse(UniverseExpression const&)
    {
        ENFORCEU("no front token for universe-expression");
    }

    Result stmtExpression(ExpressionStatement const& s)
    {
        return dispatch(s.expression());
    }

    Result stmtVariable(VariableStatement const& s)
    {
        return s.variable().symbol().token();
    }

    Result juncBranch(BranchJunction const& b)
    {
        return b.token();
    }

    Result juncReturn(ReturnJunction const& r)
    {
        return r.token();
    }

    Result juncJump(JumpJunction const& j)
    {
        return j.token();
    }
};

lexer::Token const& front(lexer::Token const& tok)
{
    return tok;
}

lexer::Token const& front(Expression const& expr)
{
    ShallowApply<FrontToken> op;
    return op(expr);
}

lexer::Token const& front(Statement const& stmt)
{
    ShallowApply<FrontToken> op;
    return op(stmt);
}

lexer::Token const& front(Junction const& junc)
{
    ShallowApply<FrontToken> op;
    return op(junc);
}

lexer::Token const& front(Declaration const& decl)
{
    return decl.symbol().token();
}

template <typename Dispatcher>
struct PrintOperator
{
    using Result = void;

    Dispatcher& dispatch;
    DefaultOutStream& sink;
    int nest = 0;

    PrintOperator(Dispatcher& dispatch, DefaultOutStream& sink)
        : dispatch(dispatch)
        , sink(sink)
    {
    }

    Result printConstraints(Slice<Expression const*> exprs)
    {
        for ( auto c : exprs ) {
            if ( auto id = c->as<IdentifierExpression>() )
                if ( id->token().kind() == lexer::TokenKind::Undefined )
                    continue;

            sink(" : ");
            dispatch(*c);
        }
    }

    Result printConstraints(Expression const& expr)
    {
        return printConstraints(expr.constraints());
    }

    Result printType(Expression const& expr)
    {
        if ( expr.type() ) {
            sink(" : ");
            return dispatch(*expr.type());
        }

        sink(" : ~err");
    }

    Result showTyped(Expression const& expr)
    {
        dispatch(expr);
        printType(expr);
    }

    Result exprLiteral(LiteralExpression const& p)
    {
        sink(p.token().lexeme());
        printConstraints(p);
    }

    Result exprIdentifier(IdentifierExpression const& id)
    {
        auto beg = id.token().lexeme().begin();
        if ( id.token().kind() == lexer::TokenKind::MetaVariable )
            --beg;
        sink(slice(beg, id.token().lexeme().end()));
        printConstraints(id);

        if ( auto decl = getDeclaration(id) )
            if ( auto binder = getBinder(*decl) )
                printConstraints(binder->constraints());
    }

    Result exprTuple(TupleExpression const& t)
    {
        sink(presentTupleOpen(t.kind()));

        if ( t.expressions() ) {
            showTyped(*t.expressions()[0]);

            for ( auto const& e : t.expressions()(1, $) ) {
                sink(presentTupleWeave(t.kind()));
                showTyped(*e);
            }
        }

        sink(presentTupleClose(t.kind()));
        printConstraints(t);
    }

    Result exprApply(ApplyExpression const& a)
    {
        if ( nest )
            sink("(");

        ++nest;
        auto first = true;
        auto e = a.expressions();
        if ( auto id = e.front()->as<IdentifierExpression>() )
            if ( id->token().kind() == lexer::TokenKind::Undefined )
                e.popFront();

        for ( ; e; e.popFront() ) {
            if ( !first )
                sink(" ");
            else
                first = false;

            dispatch(*e.front());
        }
        --nest;
        if ( nest )
            sink(")");

        printConstraints(a);
    }

    Result exprSymbol(SymbolExpression const& s)
    {
        auto const& id = s.token().lexeme();
        if ( id )
            sink(id);

        if ( s.expressions() ) {
            sink('<');
            dispatch(*s.expressions()[0]);

            for ( auto const& e : s.expressions()(1, $) ) {
                sink(", ");
                dispatch(*e);
            }

            sink('>');

            return printConstraints(s);
        }

        if ( !id )
            sink("<>");
        printConstraints(s);
    }

    Result exprDot(DotExpression const& d)
    {
        if ( d.isModuleScope() )
            sink(".");

        auto l = begin(d.expressions());
        auto r = end(d.expressions());
        if ( l != r )
            dispatch(*(*l));

        ++l;
        for ( ; l != r; ++l ) {
            sink(".");
            dispatch(*(*l));
        }

        printConstraints(d);
    }

    Result exprAssign(AssignExpression const& v)
    {
        if ( auto decl = getDeclaration(v.left()) ) {
            if ( auto var = decl->as<VariableDeclaration>() )
                if ( !var->symbol().token().lexeme() )
                    return dispatch(v.right());
        }

        if ( nest )
            sink("(");

        dispatch(v.left());
        sink(" = ");
        dispatch(v.right());

        if ( nest )
            sink(")");

        printConstraints(v);
    }

    Result exprLambda(LambdaExpression const& l)
    {
        sink("(");
        for ( auto p : l.procedure().parameters() ) {
            sink(p->symbol().token().lexeme());
            if ( p->constraints() )
                sink(" : ");

            for ( auto c : p->constraints() )
                dispatch(*c);
        }

        sink(")");
        printConstraints(l);
    }

    Result exprArrow(ArrowExpression const& a)
    {
        dispatch(a.from());
        sink(" -> ");
        dispatch(a.to());

        printConstraints(a);
    }

    Result exprUniverse(UniverseExpression const& u)
    {
        sink("Universe<");
        sink(u.level());
        sink(">");

        printConstraints(u);
    }

    Result stmtExpression(ExpressionStatement const& s)
    {
        return dispatch(s.expression());
    }

    Result stmtVariable(VariableStatement const& s)
    {
        sink(":= ");
        sink(s.variable().symbol().token().lexeme());
        if ( auto e = s.initializer() ) {
            sink(" = ");
            dispatch(*e);
        }
    }

    Result juncBranch(BranchJunction const& b)
    {
        sink(":? ");
        return dispatch(*b.condition());
    }

    Result juncReturn(ReturnJunction const& r)
    {
        sink("return ");
        return dispatch(r.expression());
    }

    Result juncJump(JumpJunction const& j)
    {
        sink(j.token().lexeme());
        if ( j.targetLabel().kind() != lexer::TokenKind::Undefined )
            sink(j.targetLabel().lexeme());
    }
};

template <typename Dispatcher>
struct LevelFinder
{
    using Result = uz;

    Dispatcher& dispatch;

    LevelFinder(Dispatcher& dispatch)
        : dispatch(dispatch)
    {
    }

    Result exprLiteral(LiteralExpression const&)
    {
        return 0;
    }

    Result exprIdentifier(IdentifierExpression const&)
    {
        return 1; // todo: higher level types
    }

    Result max(Slice<Expression const*> exprs)
    {
        if ( !exprs )
            return 0;

        auto max = dispatch(*exprs.front());
        for ( auto const& e : slice(exprs, 1) )
            max = std::max(max, dispatch(*e));

        return max;
    }

    Result exprTuple(TupleExpression const& t)
    {
        return max(t.expressions());
    }

    Result exprApply(ApplyExpression const& a)
    {
        return max(a.expressions());
    }

    Result exprSymbol(SymbolExpression const& s)
    {
        return exprIdentifier(s);
    }

    Result exprDot(DotExpression const& d)
    {
        return dispatch(*d.expressions().back());
    }

    Result exprAssign(AssignExpression const& v)
    {
        return std::max(dispatch(v.left()), dispatch(v.right()));
    }

    Result exprLambda(LambdaExpression const& l)
    {
        Result ret = 1;
        for ( auto const p : l.procedure().parameters() )
            ret = std::max(ret, dispatch(*p->type()));

        return std::max(ret, dispatch(*l.procedure().result()->type()));
    }

    Result exprArrow(ArrowExpression const& a)
    {
        return std::max(dispatch(a.from()), dispatch(a.to()));
    }

    Result exprUniverse(UniverseExpression const& u)
    {
        return u.level();
    }
};

uz level(Expression const& expr)
{
    ShallowApply<LevelFinder> op;
    return op(expr);
}

    } // namespace ast

    namespace ascii {
        void write(DefaultOutStream& sink, ast::Expression const& expr)
        {
            ast::ShallowApply<ast::PrintOperator> op(sink);
            return op(expr);
        }

        void write(DefaultOutStream& sink, ast::Statement const& stmt)
        {
            ast::ShallowApply<ast::PrintOperator> op(sink);
            return op(stmt);
        }

        void write(DefaultOutStream& sink, ast::Junction const& junc)
        {
            ast::ShallowApply<ast::PrintOperator> op(sink);
            return op(junc);
        }
    }

} // namespace kyfoo
