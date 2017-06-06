#include <kyfoo/ast/Semantics.hpp>

#include <kyfoo/ast/Declarations.hpp>
#include <kyfoo/ast/Module.hpp>
#include <kyfoo/ast/Scopes.hpp>
#include <kyfoo/ast/Symbol.hpp>

namespace kyfoo {
    namespace ast {

//
// ScopeResolver

ScopeResolver::ScopeResolver(DeclarationScope* scope)
    : myScope(scope)
{
    if ( !scope )
        throw std::runtime_error("scope resolver scope cannot be null");
}

Module const* ScopeResolver::module() const
{
    return myScope->module();
}

LookupHit ScopeResolver::inScope(SymbolReference const& symbol) const
{
    if ( auto hit = myScope->findEquivalent(symbol) )
        return hit;

    for ( auto const& e : mySupplementarySymbols )
        if ( auto symVar = e->findVariable(symbol.name()) )
            return LookupHit(symVar);

    return LookupHit();
}

LookupHit ScopeResolver::matchEquivalent(SymbolReference const& symbol) const
{
    if ( auto hit = inScope(symbol) )
        return hit;

    for ( auto scope = myScope->parent(); scope; scope = scope->parent() ) {
        if ( auto hit = scope->findEquivalent(symbol) )
            return hit;

        if ( symbol.parameters().empty() )
            if ( auto decl = scope->declaration() )
                if ( auto s = decl->symbol().findVariable(symbol.name()) )
                    return LookupHit(s);
    }

    for ( auto m : module()->imports() )
        if ( auto hit = m->scope()->findEquivalent(symbol) )
            return hit;

    return LookupHit();
}

LookupHit ScopeResolver::matchValue(SymbolReference const& symbol) const
{
    for ( auto scope = myScope; scope; scope = scope->parent() ) {
        if ( auto hit = scope->findValue(symbol) )
            return hit;

        if ( symbol.parameters().empty() )
            if ( auto decl = scope->declaration() )
                if ( auto s = decl->symbol().findVariable(symbol.name()) )
                    return LookupHit(s);
    }

    for ( auto m : module()->imports() )
        if ( auto hit = m->scope()->findValue(symbol) )
            return hit;

    return LookupHit();
}

LookupHit ScopeResolver::matchProcedure(SymbolReference const& procOverload) const
{
    for ( auto scope = myScope; scope; scope = scope->parent() ) {
        if ( auto hit = scope->findProcedureOverload(procOverload) )
            return hit;
    }

    for ( auto m : module()->imports() )
        if ( auto hit = m->scope()->findProcedureOverload(procOverload) )
            return hit;

    return LookupHit();
}

void ScopeResolver::addSupplementarySymbol(Symbol const& sym)
{
    mySupplementarySymbols.push_back(&sym);
}

//
// SymbolVariableCreatorFailoverResolver

SymbolVariableCreatorFailoverResolver::SymbolVariableCreatorFailoverResolver(IResolver& resolver, Symbol& symbol)
    : myResolver(&resolver)
    , mySymbol(&symbol)
{
}

SymbolVariableCreatorFailoverResolver::~SymbolVariableCreatorFailoverResolver() = default;

Module const* SymbolVariableCreatorFailoverResolver::module() const
{
    return myResolver->module();
}

LookupHit SymbolVariableCreatorFailoverResolver::inScope(SymbolReference const& symbol) const
{
    return myResolver->inScope(symbol);
}

LookupHit SymbolVariableCreatorFailoverResolver::matchEquivalent(SymbolReference const& symbol) const
{
    if ( auto hit = myResolver->matchEquivalent(symbol) )
        return hit;

    if ( symbol.parameters().empty() )
        return LookupHit(mySymbol->createVariable(symbol.name()));

    return LookupHit();
}

LookupHit SymbolVariableCreatorFailoverResolver::matchValue(SymbolReference const& symbol) const
{
    if ( auto hit = myResolver->matchValue(symbol) )
        return hit;

    if ( symbol.parameters().empty() )
        return LookupHit(mySymbol->createVariable(symbol.name()));

    return LookupHit();
}

LookupHit SymbolVariableCreatorFailoverResolver::matchProcedure(SymbolReference const& procOverload) const
{
    return myResolver->matchProcedure(procOverload);
}

//
// operators

template <typename Dispatcher>
struct StateCounter
{
    using result_t = std::size_t;
    Dispatcher& dispatch;
    Context& ctx;
    Expression const& expr;

    StateCounter(Dispatcher& dispatch,
                 Context& ctx,
                 Expression const& expr)
        : dispatch(dispatch)
        , ctx(ctx)
        , expr(expr)
    {
    }

    result_t declDataSum(DataSumDeclaration const& ds)
    {
        result_t ret = 0;
        for ( auto const& decl : ds.definition()->childDeclarations() )
            ret += dispatch(*decl);

        return ret;
    }

    result_t declDataSumCtor(DataSumDeclaration::Constructor const& dsCtor)
    {
        result_t ret = 1;
        for ( auto const& e : dsCtor.fields() )
            ret += declVariable(*e);

        return ret;
    }

    result_t declDataProduct(DataProductDeclaration const& dp)
    {
        result_t ret = 0;
        for ( auto const& e : dp.definition()->childDeclarations() )
            dispatch(*e);

        return ret;
    }

    result_t declSymbol(SymbolDeclaration const& sym)
    {
        auto s = sym.expression()->as<SymbolExpression>();
        if ( !s ) {
            ctx.error(*sym.expression()) << "expected symbol expression in this context";
            return 0;
        }

        return dispatch(*s->declaration());
    }

    result_t declProcedure(ProcedureDeclaration const&)
    {
        return 0;
    }

    result_t declVariable(VariableDeclaration const& var)
    {
        auto s = var.constraint()->as<SymbolExpression>();
        if ( !s ) {
            ctx.error(*var.constraint()) << "does not identify a data type";
            return 0;
        }

        return dispatch(*s->declaration());
    }

    result_t declImport(ImportDeclaration const&)
    {
        ctx.error(expr) << "does not identify a data type";
        return 0;
    }

    result_t declSymbolVariable(SymbolVariable const&)
    {
        return std::numeric_limits<result_t>::max();
    }
};

std::size_t stateCount(Context& ctx, Expression const& expr)
{
    auto s = expr.as<SymbolExpression>();
    if ( !s ) {
        ctx.error(expr) << "expected symbol expression";
        return 0;
    }

    if ( s->declaration()->kind() == DeclKind::Variable ) {
        ctx.error(*s) << "expected a data type, received a variable";
        return 0;
    }

    ShallowApply<StateCounter> op(ctx, expr);
    return op(*s->declaration());
}

template <typename O>
auto commute(O& o, Expression const& lhs, Expression const& rhs)
{
    if ( auto l = lhs.as<PrimaryExpression>() ) {
        if ( auto r = rhs.as<PrimaryExpression>() )    return o(*l, *r);
        if ( auto r = rhs.as<TupleExpression>() )      return o(*l, *r);
        if ( auto r = rhs.as<ConstraintExpression>() ) return o(*l, *r);

        goto L_error;
    }
    
    if ( auto l = lhs.as<TupleExpression>() ) {
        if ( auto r = rhs.as<TupleExpression>() )      return o(*l, *r);
        if ( auto r = rhs.as<ConstraintExpression>() ) return o(*l, *r);

        goto L_error;
    }
    
    if ( auto l = lhs.as<ConstraintExpression>() ) {
        if ( auto r = rhs.as<ConstraintExpression>() ) return o(*l, *r);
    }
    
L_error:
    throw std::runtime_error("invalid dispatch");
}

template <typename O>
auto noncommute(O& o, Expression const& lhs, Expression const& rhs)
{
    if ( auto l = lhs.as<PrimaryExpression>() ) {
        if ( auto r = rhs.as<PrimaryExpression>() )    return o(*l, *r);
        if ( auto r = rhs.as<TupleExpression>() )      return o(*l, *r);
        if ( auto r = rhs.as<ApplyExpression>() )      return o(*l, *r);
        if ( auto r = rhs.as<SymbolExpression>() )     return o(*l, *r);
        if ( auto r = rhs.as<ConstraintExpression>() ) return o(*l, *r);
        goto L_error;
    }
    if ( auto l = lhs.as<TupleExpression>() ) {
        if ( auto r = rhs.as<PrimaryExpression>() )    return o(*l, *r);
        if ( auto r = rhs.as<TupleExpression>() )      return o(*l, *r);
        if ( auto r = rhs.as<ApplyExpression>() )      return o(*l, *r);
        if ( auto r = rhs.as<SymbolExpression>() )     return o(*l, *r);
        if ( auto r = rhs.as<ConstraintExpression>() ) return o(*l, *r);
        goto L_error;
    }
    if ( auto l = lhs.as<ApplyExpression>() ) {
        if ( auto r = rhs.as<PrimaryExpression>() )    return o(*l, *r);
        if ( auto r = rhs.as<TupleExpression>() )      return o(*l, *r);
        if ( auto r = rhs.as<ApplyExpression>() )      return o(*l, *r);
        if ( auto r = rhs.as<SymbolExpression>() )     return o(*l, *r);
        if ( auto r = rhs.as<ConstraintExpression>() ) return o(*l, *r);
        goto L_error;
    }
    if ( auto l = lhs.as<SymbolExpression>() ) {
        if ( auto r = rhs.as<PrimaryExpression>() )    return o(*l, *r);
        if ( auto r = rhs.as<TupleExpression>() )      return o(*l, *r);
        if ( auto r = rhs.as<ApplyExpression>() )      return o(*l, *r);
        if ( auto r = rhs.as<SymbolExpression>() )     return o(*l, *r);
        if ( auto r = rhs.as<ConstraintExpression>() ) return o(*l, *r);
        goto L_error;
    }
    if ( auto l = lhs.as<ConstraintExpression>() ) {
        if ( auto r = rhs.as<PrimaryExpression>() )    return o(*l, *r);
        if ( auto r = rhs.as<TupleExpression>() )      return o(*l, *r);
        if ( auto r = rhs.as<ApplyExpression>() )      return o(*l, *r);
        if ( auto r = rhs.as<SymbolExpression>() )     return o(*l, *r);
        if ( auto r = rhs.as<ConstraintExpression>() ) return o(*l, *r);
    }

L_error:
    throw std::runtime_error("invalid dispatch");
}

struct MatchEquivalent
{
    // Primary match

    bool operator()(PrimaryExpression const& l, PrimaryExpression const& r)
    {
        if ( l.token().kind() != r.token().kind() )
            return false;

        if ( l.token().kind() != lexer::TokenKind::Identifier )
            return l.token().lexeme() == r.token().lexeme();

        if ( l.declaration()->kind() == DeclKind::SymbolVariable
            && r.declaration()->kind() == DeclKind::SymbolVariable )
            return true; // todo: compare constraints

        return l.declaration() == r.declaration();
    }

    bool operator()(PrimaryExpression const& l, ConstraintExpression const& r)
    {
        if ( !r.subject() )
            return false;

        return noncommute(*this, l, *r.subject());
    }

    // Tuple match

    bool operator()(TupleExpression const& l, TupleExpression const& r)
    {
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
        return matchEquivalent(l.expressions(), r.expressions());
    }

    // Constraint match

    bool operator()(ConstraintExpression const& l, ConstraintExpression const& r)
    {
        if ( !l.subject() || !r.subject() )
            return false;

        return noncommute(*this, *l.subject(), *r.subject());
    }

    // else

    bool operator()(Expression const&, Expression const&)
    {
        return false;
    }
};

bool matchEquivalent(Expression const& lhs, Expression const& rhs)
{
    MatchEquivalent op;
    return noncommute(op, lhs, rhs);
}

template <typename Dispatcher>
struct DeclOp
{
    using result_t = Declaration const*;
    Dispatcher& dispatch;

    DeclOp(Dispatcher& dispatch)
        : dispatch(dispatch)
    {
    }

    result_t exprPrimary(PrimaryExpression const& p)
    {
        return p.declaration();
    }

    result_t exprTuple(TupleExpression const&)
    {
        // todo: tuple of types
        return nullptr;
    }

    result_t exprApply(ApplyExpression const& a)
    {
        return a.declaration();
    }

    result_t exprSymbol(SymbolExpression const& s)
    {
        return s.declaration();
    }

    result_t exprConstraint(ConstraintExpression const& c)
    {
        return dispatch(*c.subject());
    }
};

bool isCovariant(Declaration const& target, Declaration const& query)
{
    if ( &target == &query )
        return true;

    if ( auto dsCtor = query.as<DataSumDeclaration::Constructor>() )
        return isCovariant(target, *dsCtor->parent());

    return false;
}

bool matchValue(Expression const& lhs, Expression const& rhs)
{
    ShallowApply<DeclOp> op;
    auto targetDecl = op(lhs);
    auto queryDecl = op(rhs);

    if ( !targetDecl )
        return false;

    if ( !queryDecl ) {
        if ( auto p = rhs.as<PrimaryExpression>() ) {
            if ( p->token().kind() == lexer::TokenKind::Integer )
                return targetDecl->symbol().name() == "integer";
            else if ( p->token().kind() == lexer::TokenKind::Decimal )
                return targetDecl->symbol().name() == "rational";
        }

        return false;
    }

    if ( targetDecl->kind() == DeclKind::SymbolVariable ) {
        // todo: symvar binding
        return true;
    }

    if ( !isDataDeclaration(targetDecl->kind())
      || !isDataDeclaration(queryDecl->kind()) )
        return false;

    return isCovariant(*targetDecl, *queryDecl);
}

template <typename T>
bool compare(SymbolReference::paramlist_t lhs,
             SymbolReference::paramlist_t rhs,
             T& op)
{
    if ( lhs.size() != rhs.size() )
        return false;

    if ( lhs.empty() && rhs.empty() )
        return true;

    auto const size = lhs.size();
    for ( std::size_t i = 0; i < size; ++i ) {
        if ( !op(*lhs[i], *rhs[i]) )
            return false;
    }

    return true;
}

bool matchEquivalent(SymbolReference::paramlist_t lhs,
                     SymbolReference::paramlist_t rhs)
{
    auto op = [](auto const& l, auto const& r) { return matchEquivalent(l, r); };
    return compare(lhs, rhs, op);
}

bool matchValue(SymbolReference::paramlist_t lhs,
                SymbolReference::paramlist_t rhs)
{
    auto op = [](auto const& l, auto const& r) { return matchValue(l, r); };
    return compare(lhs, rhs, op);
}

    } // namespace ast
} // namespace kyfoo
