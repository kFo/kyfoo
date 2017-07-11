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

namespace {
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
        for ( std::size_t i = 0; i < size; ++i )
            if ( !op(*lhs[i], *rhs[i]) )
                return false;

        return true;
    }

    bool bind(binding_set_t& bindings, SymbolVariable const& symVar, Expression const& expr)
    {
        auto e = bindings.find(&symVar);
        if ( e == end(bindings) ) {
            // new binding
            bindings[&symVar] = &expr;
            return true;
        }

        // existing binding must be consistent
        // todo: print diagnostics on mismatch
        return matchEquivalent(*e->second, expr);
    }
} // namespace

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

    if ( group->pass <= dependency->pass ) {
        if ( !group->defer(group, dependency->pass + 1) ) {
            auto& err = dgn.error(mod, decl.symbol().identifier()) << "circular reference detected";
            for ( auto const& d : dependency->declarations )
                err.see(d);
        }
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

    result_t declSymbol(SymbolDeclaration& s)
    {
        traceSymbol();
        if ( s.expression() )
            dispatch(*s.expression());
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

        goto L_error;
    }
    
    if ( auto l = lhs.as<TupleExpression>() ) {
        if ( auto r = rhs.as<TupleExpression>() )      return o(*l, *r);

        goto L_error;
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
        goto L_error;
    }
    if ( auto l = lhs.as<TupleExpression>() ) {
        if ( auto r = rhs.as<PrimaryExpression>() )    return o(*l, *r);
        if ( auto r = rhs.as<TupleExpression>() )      return o(*l, *r);
        if ( auto r = rhs.as<ApplyExpression>() )      return o(*l, *r);
        if ( auto r = rhs.as<SymbolExpression>() )     return o(*l, *r);
        goto L_error;
    }
    if ( auto l = lhs.as<ApplyExpression>() ) {
        if ( auto r = rhs.as<PrimaryExpression>() )    return o(*l, *r);
        if ( auto r = rhs.as<TupleExpression>() )      return o(*l, *r);
        if ( auto r = rhs.as<ApplyExpression>() )      return o(*l, *r);
        if ( auto r = rhs.as<SymbolExpression>() )     return o(*l, *r);
        goto L_error;
    }
    if ( auto l = lhs.as<SymbolExpression>() ) {
        if ( auto r = rhs.as<PrimaryExpression>() )    return o(*l, *r);
        if ( auto r = rhs.as<TupleExpression>() )      return o(*l, *r);
        if ( auto r = rhs.as<ApplyExpression>() )      return o(*l, *r);
        if ( auto r = rhs.as<SymbolExpression>() )     return o(*l, *r);
        goto L_error;
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

bool matchEquivalent(Slice<Expression*> lhs, Slice<Expression*> rhs)
{
    auto op = [](auto const& l, auto const& r) { return matchEquivalent(l, r); };
    return compare(lhs, rhs, op);
}

struct MatchInstantiable
{
    binding_set_t& bindingSet;

    MatchInstantiable(binding_set_t& bindingSet)
        : bindingSet(bindingSet)
    {
    }

    bool bindSymVar(SymbolVariable const& sym, Expression const& expr)
    {
        auto e = bindingSet.find(&sym);
        if ( e != end(bindingSet) ) {
            // expr must agree with what is already bound
            return matchEquivalent(*e->second, expr);
        }

        bindingSet[&sym] = &expr;
        return true;
    }

    // Primary match

    bool operator()(PrimaryExpression const& l, PrimaryExpression const& r)
    {
        if ( isIdentifier(l.token().kind()) ) {
            if ( auto symVar = l.declaration()->as<SymbolVariable>() )
                return bindSymVar(*symVar, r);

            // todo: are these needed?
            if ( auto s = l.declaration()->as<SymbolDeclaration>() )
                return noncommute(*this, *s->expression(), r);

            if ( isIdentifier(r.token().kind()) ) {
                if ( r.declaration()->kind() == DeclKind::SymbolVariable )
                    return false;

                // todo: are these needed?
                if ( auto s = r.declaration()->as<SymbolDeclaration>() )
                    return noncommute(*this, l, *s->expression());

                return l.declaration() == r.declaration();
            }

            return isCovariant(*l.declaration(), r.token());
        }

        if ( isIdentifier(r.token().kind()) ) {
            if ( r.declaration()->kind() == DeclKind::SymbolVariable )
                return false;

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
bool matchInstantiable(binding_set_t& bindingSet,
                       Expression const& lhs,
                       Expression const& rhs)
{
    MatchInstantiable op(bindingSet);
    return noncommute(op, lhs, rhs);
}

bool matchInstantiable(binding_set_t& bindingSet,
                       Slice<Expression*> lhs,
                       Slice<Expression*> rhs)
{
    if ( lhs.empty() )
        return rhs.empty();

    auto const size = lhs.size();
    if ( size != rhs.size() )
        return false;

    for ( std::size_t i = 0; i < size; ++i ) {
        if ( !matchInstantiable(bindingSet, *lhs[i], *rhs[i]) )
            return false;
    }

    return true;
}

bool isCovariant(lexer::Token const& target, lexer::Token const& query)
{
    return target.lexeme() == query.lexeme();
}

bool isCovariant(Declaration const& target, lexer::Token const& query)
{
    if ( isDataDeclaration(target.kind()) ) {
        // todo: implicit conversions
        if ( query.kind() == lexer::TokenKind::Integer )
            return target.symbol().name() == "integer";
        else if ( query.kind() == lexer::TokenKind::Rational )
            return target.symbol().name() == "rational";
        else if ( query.kind() == lexer::TokenKind::String )
            return target.symbol().name() == "ascii";
    }

    return false;
}

bool isCovariant(Declaration const& target, Declaration const& query)
{
    if ( &target == &query )
        return true;

    if ( auto dsCtor = query.as<DataSumDeclaration::Constructor>() )
        return isCovariant(target, *dsCtor->parent());

    if ( auto f = query.as<ProcedureDeclaration>() )
        return isCovariant(target, *f->returnType()->declaration());

    if ( auto v = query.as<VariableDeclaration>() )
        return isCovariant(target, *v->constraint()->declaration());

    return false;
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
        decl = ret->declaration();
    }

    return ret;
}

Declaration const* resolveIndirections(Declaration const* decl)
{
    if ( decl ) {
        if ( auto expr = lookThrough(decl) ) {
            if ( !expr->declaration() )
                throw std::runtime_error("unresolved indirection");

            return expr->declaration();
        }
    }

    return decl;
}

Expression const* resolveIndirections(Expression const* expr)
{
    if ( expr ) {
        if ( auto e = lookThrough(expr->declaration()) )
            return e;
    }

    return expr;
}

Symbol const* rootTemplate(Symbol const& symbol)
{
    auto ret = &symbol;
    while ( ret->parentTemplate() )
        ret = ret->parentTemplate();

    return ret;
}

bool descendsFromTemplate(Symbol const& parent, Symbol const& instance)
{
    auto p = &instance;
    while ( p ) {
        if ( p == &parent )
            return true;

        p = p->parentTemplate();
    }

    return false;
}

//
// ValueMatcher

void ValueMatcher::reset()
{
    leftBindings.clear();
    rightBindings.clear();
}

/**
 * Matches lhs :> value(rhs) semantically
 * 
 * Answers whether \p rhs 's value is covariant with type \p lhs
 */
bool ValueMatcher::matchValue(Expression const& lhs, Expression const& rhs)
{
    auto queryDecl = rhs.declaration();
    auto targetDecl = lhs.declaration();

    {
        // resolve ast aliases and run again (normalizing)
        auto l = lookThrough(targetDecl);
        auto r = lookThrough(queryDecl);

        if ( l || r )
            return matchValue(l ? *l : lhs, r ? *r : rhs);
    }

    if ( queryDecl ) {
        // look through storage declarations to their constraints
        // similar to looking through ast aliases (more normalization)
        if ( auto proc = queryDecl->as<ProcedureDeclaration>() )
            return matchValue(lhs, *proc->returnType());

        if ( auto v = queryDecl->as<VariableDeclaration>() )
            return matchValue(lhs, *v->constraint());
    }

    // assumes lhs is:
    // - literal
    // - data declaration
    // - symbol variable

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
        if ( auto symVar = targetDecl->as<SymbolVariable>() )
            return bind(leftBindings, *symVar, rhs);

        if ( !queryDecl ) {
            // rhs is a literal
            if ( auto p = rhs.as<PrimaryExpression>() )
                return isCovariant(*targetDecl, p->token());

            return false;
        }

        if ( rootTemplate(targetDecl->symbol()) == rootTemplate(queryDecl->symbol()) )
            return matchValue(targetDecl->symbol().parameters(), queryDecl->symbol().parameters());

        return isCovariant(*targetDecl, *queryDecl);
    }

    return false;
}

bool ValueMatcher::matchValue(Slice<Expression*> lhs, Slice<Expression*> rhs)
{
    auto op = [this](auto const& l, auto const& r) { return matchValue(l, r); };
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
};

bool hasFreeVariable(Expression const& expr)
{
    ShallowApply<HasFreeVariable> op;
    return op(expr);
}

    } // namespace ast
} // namespace kyfoo
