#include <kyfoo/ast/Semantics.hpp>

#include <algorithm>
#include <functional>
#include <set>

#include <kyfoo/Diagnostics.hpp>
#include <kyfoo/ast/Module.hpp>
#include <kyfoo/ast/Scopes.hpp>
#include <kyfoo/ast/Symbol.hpp>
#include <kyfoo/ast/Context.hpp>

namespace kyfoo {
    namespace ast {

//
// SymbolDependencyTracker

SymbolDependencyTracker::SymbolDependencyTracker(Module* mod, Diagnostics& dgn)
    : mod(mod)
    , dgn(dgn)
{
}

SymbolDependencyTracker::SymGroup* SymbolDependencyTracker::create(std::string const& name, std::size_t arity)
{
    groups.emplace_back(std::make_unique<SymGroup>(name, arity));
    return groups.back().get();
}

SymbolDependencyTracker::SymGroup* SymbolDependencyTracker::findOrCreate(std::string const& name, std::size_t arity)
{
    for ( auto const& e : groups)
        if ( e->name == name && e->arity == arity )
            return e.get();

    return create(name, arity);
}

void SymbolDependencyTracker::add(Declaration& decl)
{
    auto group = findOrCreate(decl.symbol().name(), decl.symbol().parameters().size());
    group->add(decl);
}

void SymbolDependencyTracker::addDependency(Declaration& decl,
                               std::string const& name,
                               std::size_t arity)
{
    auto group = findOrCreate(decl.symbol().name(), decl.symbol().parameters().size());
    auto dependency = findOrCreate(name, arity);

    dependency->addDependent(*group);

    if ( group->pass < dependency->pass ) {
        auto& err = dgn.error(mod, decl.symbol().identifier()) << "circular reference detected";
        for ( auto const& d : dependency->declarations )
            err.see(d);
        return;
    }
    else if ( group->pass == dependency->pass ) {
        group->defer(dependency->pass + 1);
    }
}

void SymbolDependencyTracker::sortPasses()
{
    stable_sort(begin(groups), end(groups),
                [](auto const& lhs, auto const& rhs) { return lhs->pass < rhs->pass; });
}

//
// operators

template <typename Dispatcher>
struct SymbolDependencyBuilder
{
    using result_t = void;
    Dispatcher& dispatch;
    SymbolDependencyTracker& tracker;
    Declaration& decl;

    SymbolDependencyBuilder(Dispatcher& dispatch,
                            SymbolDependencyTracker& tracker,
                            Declaration& decl)
        : dispatch(dispatch)
        , tracker(tracker)
        , decl(decl)
    {
    }

    // expressions

    result_t exprPrimary(PrimaryExpression& p)
    {
        if ( p.token().kind() == lexer::TokenKind::Identifier )
            tracker.addDependency(decl, p.token().lexeme(), 0);
    }

    result_t exprTuple(TupleExpression& t)
    {
        for ( auto const& e : t.expressions() )
            dispatch(*e);
    }

    result_t exprApply(ApplyExpression& a)
    {
        // todo: failover to implicit proc call semantics
        auto subject = a.expressions()[0]->as<PrimaryExpression>();
        if ( subject && subject->token().kind() == lexer::TokenKind::Identifier ) {
            tracker.addDependency(decl, subject->token().lexeme(), a.expressions().size() - 1);
            return;
        }

        for ( auto const& e : a.expressions() )
            dispatch(*e);
    }

    result_t exprSymbol(SymbolExpression& s)
    {
        if ( s.identifier().kind() == lexer::TokenKind::Identifier ) {
            tracker.addDependency(decl, s.identifier().lexeme(), s.expressions().size());
            return;
        }

        for ( auto const& e : s.expressions() )
            dispatch(*e);
    }

    result_t exprConstraint(ConstraintExpression& c)
    {
        dispatch(*c.subject());
        dispatch(*c.constraint());
    }

    // declarations

    void traceSymbol(Symbol& sym)
    {
        for ( auto const& p : sym.parameters() )
            dispatch(*p);
    }

    void traceSymbol()
    {
        traceSymbol(decl.symbol());
    }

    result_t declDataSum(DataSumDeclaration&)
    {
        traceSymbol();
    }

    result_t declDataSumCtor(DataSumDeclaration::Constructor& dsCtor)
    {
        traceSymbol();
        for ( auto const& field : dsCtor.fields() )
            dispatch.operator()<Declaration>(*field);
    }

    result_t declDataProduct(DataProductDeclaration&)
    {
        traceSymbol();
    }

    result_t declSymbol(SymbolDeclaration&)
    {
        traceSymbol();
    }

    result_t declProcedure(ProcedureDeclaration& proc)
    {
        traceSymbol();
        for ( auto const& param : proc.parameters() )
            dispatch(*param->constraint());
    }

    result_t declVariable(VariableDeclaration& var)
    {
        traceSymbol();
        if ( var.constraint() )
            dispatch(*var.constraint());
    }

    result_t declImport(ImportDeclaration&)
    {
        // nop
    }

    result_t declSymbolVariable(SymbolVariable&)
    {
        // nop
    }
};

void traceDependencies(SymbolDependencyTracker& tracker, Declaration& decl)
{
    ShallowApply<SymbolDependencyBuilder> op(tracker, decl);
    tracker.add(decl);
    op(decl);
}

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
        else if ( query.kind() == lexer::TokenKind::String ) {
            return target.symbol().name() == "ascii";
        }
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

Expression const& lookThrough(SymbolDeclaration const& symDecl)
{
    ShallowApply<DeclOp> op;
    Expression const* expr = symDecl.expression();
    for (;;) {
        auto targetDecl = op(*expr);
        if ( targetDecl ) {
            if ( auto s = targetDecl->as<SymbolDeclaration>() ) {
                expr = s->expression();
                continue;
            }
        }

        break;
    }

    return *expr;
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
    auto queryDecl = op(rhs);
    if ( queryDecl ) {
        if ( auto v = queryDecl->as<VariableDeclaration>() )
            return matchValue(lhs, *v->constraint());
    }

    auto targetDecl = op(lhs);

    {
        Expression const* l = nullptr;
        if ( targetDecl )
            if ( auto s = targetDecl->as<SymbolDeclaration>() )
                l = &lookThrough(*s);

        Expression const* r = nullptr;
        if ( queryDecl )
            if ( auto s = queryDecl->as<SymbolDeclaration>() )
                r = &lookThrough(*s);

        if ( l || r )
            return matchValue(l ? *l : lhs, r ? *r : rhs);
    }

    if ( !targetDecl ) {
        // lhs is a literal
        auto lhsPrimary = lhs.as<PrimaryExpression>();
        if ( !lhsPrimary )
            return false;

        if ( !queryDecl ) {
            auto rhsPrimary = rhs.as<PrimaryExpression>();
            if ( !rhsPrimary )
                return false;

            return isCovariant(lhsPrimary->token(), rhsPrimary->token());
        }
        else {
            // todo: compile time execute
        }
    }
    else {
        // lhs is an identifier

        if ( !queryDecl ) {
            // rhs is a literal
            if ( auto p = rhs.as<PrimaryExpression>() )
                return isCovariant(*targetDecl, p->token());

            return false;
        }

        if ( targetDecl->kind() == DeclKind::SymbolVariable ) {
            // todo: symvar binding
            return true;
        }

        return isCovariant(*targetDecl, *queryDecl);
    }

    return false;
}

template <typename T>
bool compare(Slice<Expression*> lhs,
             Slice<Expression*> rhs,
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

bool matchEquivalent(Slice<Expression*> lhs, Slice<Expression*> rhs)
{
    auto op = [](auto const& l, auto const& r) { return matchEquivalent(l, r); };
    return compare(lhs, rhs, op);
}

bool matchValue(Slice<Expression*> lhs, Slice<Expression*> rhs)
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
