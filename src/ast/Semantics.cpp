#include <kyfoo/ast/Semantics.hpp>

#include <functional>

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
    auto hit = myScope->findEquivalent(symbol);
    if ( hit )
        return hit;

    if ( symbol.parameters().empty() )
        for ( auto& s : mySupplementarySymbols )
            if ( auto symVar = s->findVariable(symbol.name()) )
                return std::move(hit.lookup(symVar));

    return hit;
}

LookupHit ScopeResolver::matchEquivalent(SymbolReference const& symbol) const
{
    LookupHit hit;
    if ( hit = inScope(symbol) )
        return hit;

    for ( auto scope = myScope->parent(); scope; scope = scope->parent() ) {
        if ( hit.append(scope->findEquivalent(symbol)) )
            return hit;

        if ( symbol.parameters().empty() )
            if ( auto decl = scope->declaration() )
                if ( auto s = decl->symbol().findVariable(symbol.name()) )
                    return std::move(hit.lookup(s));
    }

    for ( auto m : module()->imports() )
        if ( hit.append(m->scope()->findEquivalent(symbol)) )
            return hit;

    return hit;
}

LookupHit ScopeResolver::matchValue(SymbolReference const& symbol) const
{
    LookupHit hit;
    for ( auto scope = myScope; scope; scope = scope->parent() ) {
        if ( hit.append(scope->findValue(symbol)) )
            return hit;

        if ( symbol.parameters().empty() )
            if ( auto decl = scope->declaration() )
                if ( auto s = decl->symbol().findVariable(symbol.name()) )
                    return std::move(hit.lookup(s));
    }

    for ( auto m : module()->imports() )
        if ( hit.append(m->scope()->findValue(symbol)) )
            return hit;

    return hit;
}

LookupHit ScopeResolver::matchProcedure(SymbolReference const& procOverload) const
{
    LookupHit hit;
    for ( auto scope = myScope; scope; scope = scope->parent() ) {
        if ( hit.append(scope->findProcedureOverload(procOverload)) )
            return hit;
    }

    for ( auto m : module()->imports() )
        if ( hit.append(m->scope()->findProcedureOverload(procOverload)) )
            return hit;

    return hit;
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
        if ( isIdentifier(l.token().kind()) ) {
            if ( l.declaration()->kind() == DeclKind::SymbolVariable )
                return r.declaration() && r.declaration()->kind() == DeclKind::SymbolVariable;

            if ( auto s = l.declaration()->as<SymbolDeclaration>() )
                return noncommute(*this, *s->expression(), r);

            if ( isIdentifier(r.token().kind()) ) {
                if ( r.declaration()->kind() == DeclKind::SymbolVariable )
                    return true; // todo: symvar binding

                if ( auto s = r.declaration()->as<SymbolDeclaration>() )
                    return noncommute(*this, l, *s->expression());

                return l.declaration() == r.declaration();
            }

            return false;
        }

        if ( isIdentifier(r.token().kind()) ) {
            if ( r.declaration()->kind() == DeclKind::SymbolVariable )
                return true;

            if ( auto s = r.declaration()->as<SymbolDeclaration>() )
                return noncommute(*this, l, *s->expression());

            return false;
        }

        return isCovariant(l.token(), r.token());
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

    bool operator()(ConstraintExpression const& l, Expression const& r)
    {
        if ( !l.subject() )
            return false;

        if ( auto rc = r.as<ConstraintExpression>() ) {
            if ( !rc->subject() )
                return false;

            return noncommute(*this, *l.subject(), *rc->subject());
        }

        return noncommute(*this, *l.subject(), r);
    }

    // else

    bool operator()(Expression const& l, Expression const& r)
    {
        // todo: symvar binding
        if ( auto p = l.as<PrimaryExpression>() )
            return p->declaration()->kind() == DeclKind::SymbolVariable;

        if ( auto p = r.as<PrimaryExpression>() )
            return p->declaration()->kind() == DeclKind::SymbolVariable;

        return false;
    }
};

/**
 * Matches lhs :> rhs semantically
 *
 * Answers whether the type \p rhs is covered by type \p lhs
 *
 * \todo Match should return degree in presence of specialization
 */
bool matchEquivalent(Expression const& lhs, Expression const& rhs)
{
    MatchEquivalent op;
    return noncommute(op, lhs, rhs);
}

struct MatchInstantiable
{
    // Primary match

    bool operator()(PrimaryExpression const& l, PrimaryExpression const& r)
    {
        if ( isIdentifier(l.token().kind()) ) {
            if ( l.declaration()->kind() == DeclKind::SymbolVariable )
                return true; // todo: symvar binding

            if ( auto s = l.declaration()->as<SymbolDeclaration>() )
                return noncommute(*this, *s->expression(), r);

            if ( isIdentifier(r.token().kind()) ) {
                if ( r.declaration()->kind() == DeclKind::SymbolVariable )
                    return true; // todo: symvar binding

                if ( auto s = r.declaration()->as<SymbolDeclaration>() )
                    return noncommute(*this, l, *s->expression());

                return l.declaration() == r.declaration();
            }

            return isCovariant(*l.declaration(), r.token());
        }

        if ( isIdentifier(r.token().kind()) ) {
            if ( r.declaration()->kind() == DeclKind::SymbolVariable )
                throw std::runtime_error("symvar should be instantiated");

            if ( auto s = r.declaration()->as<SymbolDeclaration>() )
                return noncommute(*this, l, *s->expression());

            return false;
        }

        return isCovariant(l.token(), r.token());
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

    bool operator()(ConstraintExpression const& l, Expression const& r)
    {
        if ( !l.subject() )
            return false;

        if ( auto rc = r.as<ConstraintExpression>() ) {
            if ( !rc->subject() )
                return false;

            return noncommute(*this, *l.subject(), *rc->subject());
        }

        return noncommute(*this, *l.subject(), r);
    }

    // else

    bool operator()(Expression const& l, Expression const& r)
    {
        // todo: symvar binding
        if ( auto p = l.as<PrimaryExpression>() )
            return p->declaration()->kind() == DeclKind::SymbolVariable;

        if ( auto p = r.as<PrimaryExpression>() )
            return p->declaration()->kind() == DeclKind::SymbolVariable;

        return false;
    }
};

/**
 * Answers whether \p lhs could be instantiated by \p rhs for an
 * unknown binding of symbol variables
 */
bool matchInstantiable(Expression const& lhs, Expression const& rhs)
{
    MatchInstantiable op;
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

bool isCovariant(lexer::Token const& target, lexer::Token const& query)
{
    return target.lexeme() == query.lexeme();
}

bool isCovariant(Declaration const& target, lexer::Token const& query)
{
    if ( isDataDeclaration(target.kind()) ) {
        if ( query.kind() == lexer::TokenKind::Integer )
            return target.symbol().name() == "integer";
        else if ( query.kind() == lexer::TokenKind::Decimal )
            return target.symbol().name() == "rational";
    }

    return false;
}

bool isCovariant(Declaration const& target, Declaration const& query)
{
    if ( &target == &query )
        return true;

    if ( auto dsCtor = query.as<DataSumDeclaration::Constructor>() )
        return isCovariant(target, *dsCtor->parent());

    return false;
}

/**
 * Matches lhs :> value(rhs) semantically
 *
 * Answers whether \p rhs 's value is covariant with type \p lhs
 */
bool matchValue(Expression const& lhs, Expression const& rhs)
{
    if ( hasFreeVariable(lhs) || hasFreeVariable(rhs) )
        return matchInstantiable(lhs, rhs);

    ShallowApply<DeclOp> op;
    auto targetDecl = op(lhs);
    auto queryDecl = op(rhs);

    if ( !targetDecl )
        return false;

    if ( !queryDecl ) {
        if ( auto p = rhs.as<PrimaryExpression>() )
            return isCovariant(*targetDecl, p->token());

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

template <typename Dispatcher>
struct FreeVariableVisitor
{
    using result_t = void;
    Dispatcher& dispatch;
    
    using visitor_t = std::function<void(PrimaryExpression&)>;
    visitor_t visitor;

    FreeVariableVisitor(Dispatcher& dispatch, visitor_t visitor)
        : dispatch(dispatch)
        , visitor(visitor)
    {
    }

    result_t exprPrimary(PrimaryExpression& p)
    {
        if ( p.token().kind() == lexer::TokenKind::FreeVariable )
            return visitor(p);
    }

    result_t exprTuple(TupleExpression& t)
    {
        for ( auto const& e : t.expressions() )
            dispatch(*e);
    }

    result_t exprApply(ApplyExpression& a)
    {
        for ( auto const& e : a.expressions() )
            dispatch(*e);
    }

    result_t exprSymbol(SymbolExpression& s)
    {
        for ( auto const& e : s.expressions() )
            dispatch(*e);
    }

    result_t exprConstraint(ConstraintExpression& c)
    {
        if ( c.subject() )
            dispatch(*c.subject());

        if ( c.constraint() )
            dispatch(*c.constraint());
    }
};

template <typename F>
void visitFreeVariables(Expression& expr, F&& f)
{
    ShallowApply<FreeVariableVisitor> op(f);
    op(expr);
}

std::vector<PrimaryExpression*> gatherFreeVariables(Expression& expr)
{
    std::vector<PrimaryExpression*> ret;
    visitFreeVariables(expr, [&ret](PrimaryExpression& p) {
        ret.push_back(&p);
    });

    return ret;
}

template <typename Dispatcher>
struct HasFreeVariable
{
    using result_t = bool;
    Dispatcher& dispatch;

    HasFreeVariable(Dispatcher& dispatch)
        : dispatch(dispatch)
    {
    }

    result_t exprPrimary(PrimaryExpression const& p)
    {
        return p.token().kind() == lexer::TokenKind::FreeVariable;
    }

    result_t exprTuple(TupleExpression const& t)
    {
        for ( auto const& e : t.expressions() )
            if ( dispatch(*e) )
                return true;

        return false;
    }

    result_t exprApply(ApplyExpression const& a)
    {
        for ( auto const& e : a.expressions() )
            if ( dispatch(*e) )
                return true;

        return false;
    }

    result_t exprSymbol(SymbolExpression const& s)
    {
        for ( auto const& e : s.expressions() )
            if ( dispatch(*e) )
                return true;

        return false;
    }

    result_t exprConstraint(ConstraintExpression const& c)
    {
        if ( c.subject() )
            if ( dispatch(*c.subject()) )
                return true;

        if ( c.constraint() )
            if ( dispatch(*c.constraint()) )
                return true;

        return false;
    }
};

bool hasFreeVariable(Expression const& expr)
{
    ShallowApply<HasFreeVariable> op;
    return op(expr);
}

    } // namespace ast
} // namespace kyfoo
