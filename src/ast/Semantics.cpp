#include <kyfoo/ast/Semantics.hpp>

#include <algorithm>
#include <functional>
#include <set>

#include <boost/multiprecision/cpp_int.hpp>
#include <boost/numeric/interval.hpp>

#include <kyfoo/Diagnostics.hpp>

#include <kyfoo/lexer/TokenKind.hpp>

#include <kyfoo/ast/Axioms.hpp>
#include <kyfoo/ast/Module.hpp>
#include <kyfoo/ast/Scopes.hpp>
#include <kyfoo/ast/Symbol.hpp>
#include <kyfoo/ast/Context.hpp>

namespace kyfoo {
    namespace ast {

    using boost::multiprecision::cpp_int;
    using boost::numeric::interval;
    using bounds_t = interval<cpp_int>;

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

    bool bindSymbol(Context& ctx, binding_set_t& bindings, SymbolVariable const& symVar, Expression const& expr)
    {
        auto i = bindings.findKeyIndex(&symVar);
        if ( i == bindings.size() ) {
            // new binding
            bindings.push_back(&symVar, &expr);
            return true;
        }

        // existing binding must be consistent
        // todo: print diagnostics on mismatch
        return matchEquivalent(ctx, *bindings.values()[i], expr);
    }

    bounds_t bitsToBounds(int bits)
    {
        cpp_int const c(pow(cpp_int(2), std::abs(bits)));
        if ( bits < 0 )
            return bounds_t(-c / 2, c / 2 - 1);

        return bounds_t(0, c - 1);
    }

} // namespace

//
// SymbolDependencyTracker

SymbolDependencyTracker::SymbolDependencyTracker(Module& mod, Diagnostics& dgn)
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
    auto group = findOrCreate(decl.symbol().identifier().lexeme(), decl.symbol().prototype().pattern().size());
    group->add(decl);
}

void SymbolDependencyTracker::addDependency(Declaration& decl,
                                            std::string const& name,
                                            std::size_t arity)
{
    auto group = findOrCreate(decl.symbol().identifier().lexeme(), decl.symbol().prototype().pattern().size());
    auto dependency = findOrCreate(name, arity);

    dependency->addDependent(*group);

    if ( group->pass <= dependency->pass ) {
        if ( !group->defer(group, dependency->pass + 1) ) {
            auto& err = dgn.error(mod, decl.symbol().identifier()) << "circular reference detected";
            for ( auto const& d : dependency->declarations )
                err.see(*d);
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

    result_t exprPrimary(PrimaryExpression const& p)
    {
        if ( p.token().kind() == lexer::TokenKind::Identifier )
            tracker.addDependency(decl, p.token().lexeme(), 0);
    }

    result_t exprTuple(TupleExpression const& t)
    {
        for ( auto const& e : t.expressions() )
            dispatch(*e);
    }

    result_t exprApply(ApplyExpression const& a)
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

    result_t exprSymbol(SymbolExpression const& s)
    {
        if ( s.identifier().kind() == lexer::TokenKind::Identifier ) {
            tracker.addDependency(decl, s.identifier().lexeme(), s.expressions().size());
            return;
        }

        for ( auto const& e : s.expressions() )
            dispatch(*e);
    }

    result_t exprDot(DotExpression const& d)
    {
        for ( auto const& e : d.expressions() )
            dispatch(*e);
    }

    // declarations

    void traceSymbol(Symbol& sym)
    {
        tracePrototype(sym.prototype());
    }

    void tracePrototype(PatternsPrototype const& proto)
    {
        for ( auto const& p : proto.pattern() )
            dispatch(*p);
    }

    void traceSymbol()
    {
        traceSymbol(decl.symbol());
    }

    result_t declDataSum(DataSumDeclaration const&)
    {
        traceSymbol();
        // todo
    }

    result_t declDataSumCtor(DataSumDeclaration::Constructor const& dsCtor)
    {
        traceSymbol();
        for ( auto const& field : dsCtor.fields() )
            dispatch.operator()<Declaration>(*field);
    }

    result_t declDataProduct(DataProductDeclaration const& dp)
    {
        traceSymbol();
        if ( auto defn = dp.definition() )
            for ( auto const& field : defn->fields() )
                declField(*field);
    }

    result_t declField(DataProductDeclaration::Field const&)
    {
        traceSymbol();
    }

    result_t declSymbol(SymbolDeclaration const& s)
    {
        traceSymbol();
        if ( s.expression() )
            dispatch(*s.expression());
    }

    result_t declProcedure(ProcedureDeclaration const& proc)
    {
        traceSymbol();
        tracePrototype(proc.prototype());
    }

    result_t declProcedureParameter(ProcedureParameter const&)
    {
        // nop
    }

    result_t declVariable(VariableDeclaration const& var)
    {
        traceSymbol();
        if ( var.constraint() )
            dispatch(*var.constraint());
    }

    result_t declImport(ImportDeclaration const&)
    {
        // nop
    }

    result_t declSymbolVariable(SymbolVariable const&)
    {
        // nop
    }

    result_t declTemplate(TemplateDeclaration const&)
    {
        traceSymbol();
    }
};

void traceDependencies(SymbolDependencyTracker& tracker, Declaration& decl)
{
    ShallowApply<SymbolDependencyBuilder> op(tracker, decl);
    tracker.add(decl);
    op(decl);
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
    Context& ctx;
    explicit MatchEquivalent(Context& ctx)
        : ctx(ctx)
    {
    }

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

        return variance(ctx, l.token(), r.token()).equivalent();
    }

    // Tuple match

    bool operator()(TupleExpression const& l, TupleExpression const& r)
    {
        return matchEquivalent(ctx, l.expressions(), r.expressions());
    }

    // Apply match

    bool operator()(ApplyExpression const& l, ApplyExpression const& r)
    {
        return matchEquivalent(ctx, l.expressions(), r.expressions());
    }

    // Symbol match

    bool operator()(SymbolExpression const& l, SymbolExpression const& r)
    {
        return matchEquivalent(ctx, l.expressions(), r.expressions());
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
bool matchEquivalent(Context& ctx, Expression const& lhs, Expression const& rhs)
{
    MatchEquivalent op(ctx);
    return noncommute(op, lhs, rhs);
}

bool matchEquivalent(Context& ctx, Slice<Expression*> lhs, Slice<Expression*> rhs)
{
    auto op = [&ctx](auto const& l, auto const& r) { return matchEquivalent(ctx, l, r); };
    return compare(lhs, rhs, op);
}

VarianceResult variance(Context&, lexer::Token const& target, lexer::Token const& query)
{
    return target.lexeme() == query.lexeme() ? Equivalent : Invariant;
}

VarianceResult variance(Context& ctx, Declaration const& target, lexer::Token const& query)
{
    auto const& axioms = ctx.axioms();

    if ( auto intMeta = axioms.integerMetaData(target) ) {
        if ( query.kind() != lexer::TokenKind::Integer )
            return Invariant; // todo: diagnostics

        cpp_int const n(query.lexeme());
        bounds_t const bounds = bitsToBounds(intMeta->bits);

        if ( !in(n, bounds) )
            return Contravariant; // todo: error diagnostics

        return Covariant;
    }

    return Invariant;
}

VarianceResult variance(Context& ctx, Declaration const& target, Declaration const& query)
{
    if ( &target == &query )
        return Equivalent;

    if ( auto dsCtor = query.as<DataSumDeclaration::Constructor>() )
        return variance(ctx, target, *dsCtor->parent());

    if ( auto f = query.as<ProcedureDeclaration>() )
        return variance(ctx, target, *f->returnType()->declaration());

    if ( auto v = query.as<VariableDeclaration>() )
        return variance(ctx, target, *v->constraint()->declaration());

    if ( auto targetInteger = ctx.axioms().integerMetaData(target) ) {
        if ( auto queryInteger = ctx.axioms().integerMetaData(query) ) {
            auto const targetBounds = bitsToBounds(targetInteger->bits);
            auto const queryBounds = bitsToBounds(queryInteger->bits);

            return subset(queryBounds, targetBounds) ? Covariant : Contravariant;
        }
    }

    // todo: removeme
    if ( &query == ctx.axioms().intrinsic(PointerNullLiteralType) )
        if ( descendsFromTemplate(ctx.axioms().intrinsic(PointerTemplate)->symbol(), target.symbol()) )
            return Covariant;

    return Invariant;
}

/**
 * Matches lhs :> value(rhs) semantically
 * 
 * Answers whether \p rhs 's value is covariant with type \p lhs
 */
VarianceResult variance(Context& ctx,
                        binding_set_t& leftBindings,
                        Expression const& lhs,
                        Expression const& rhs)
{
    auto targetDecl = lhs.declaration();
    auto queryDecl = rhs.declaration();

    if ( !targetDecl ) {
        ctx.error(lhs) << "compilation stopped due to unresolved expression";
        return Invariant;
    }
    else if ( !queryDecl ) {
        ctx.error(rhs) << "compilation stopped due to unresolved expression";
        return Invariant;
    }

    {
        // resolve ast aliases and run again (normalizing)
        auto l = lookThrough(targetDecl);
        auto r = lookThrough(queryDecl);

        if ( l || r )
            return variance(ctx, leftBindings, l ? *l : lhs, r ? *r : rhs);
    }

    // look through storage declarations to their constraints
    // similar to looking through ast aliases (more normalization)
    if ( auto proc = queryDecl->as<ProcedureDeclaration>() )
        return variance(ctx, leftBindings, lhs, *proc->returnType());

    if ( auto v = queryDecl->as<VariableDeclaration>() )
        return variance(ctx, leftBindings, lhs, *v->constraint());

    if ( auto f = queryDecl->as<DataProductDeclaration::Field>() )
        return variance(ctx, leftBindings, lhs, f->constraint());

    // assumes lhs is:
    // - literal
    // - data declaration
    // - symbol variable

    auto lhsPrimary = lhs.as<PrimaryExpression>();
    auto rhsPrimary = rhs.as<PrimaryExpression>();
    if ( lhsPrimary && isLiteral(lhsPrimary->token().kind()) ) {
        // lhs is a literal
        if ( rhsPrimary && isLiteral(rhsPrimary->token().kind()) ) {
            return variance(ctx, lhsPrimary->token(), rhsPrimary->token());
        }
        else {
            // todo: compile time execute
            return Invariant;
        }
    }
    else {
        // lhs is an identifier
        if ( auto symVar = targetDecl->as<SymbolVariable>() )
            return bindSymbol(ctx, leftBindings, *symVar, rhs) ? Covariant : Invariant;

        if ( rhsPrimary ) {
            if ( isLiteral(rhsPrimary->token().kind()) ) {
                // rhs is a literal
                return variance(ctx, *targetDecl, rhsPrimary->token());
            }
            else {
                // rhs is an identifier
            }
        }

        if ( rootTemplate(targetDecl->symbol()) == rootTemplate(queryDecl->symbol()) )
            return variance(ctx, leftBindings, targetDecl->symbol().prototype().pattern(), queryDecl->symbol().prototype().pattern());

        return variance(ctx, *targetDecl, *queryDecl);
    }
}

VarianceResult variance(Context& ctx,
                        binding_set_t& leftBindings,
                        Slice<Expression*> lhs,
                        Slice<Expression*> rhs)
{
    auto const size = lhs.size();
    if ( size != rhs.size() )
        return Invariant;

    for ( std::size_t i = 0; i < size; ++i ) {
        auto v = variance(ctx, leftBindings, *lhs[i], *rhs[i]);
        if ( !v.covariant() )
            return v;
    }

    return Covariant;
}

VarianceResult variance(Context& ctx, Slice<Expression*> lhs, Slice<Expression*> rhs)
{
    binding_set_t bindings;
    return variance(ctx, bindings, lhs, rhs);
}

VarianceResult variance(Context& ctx, SymbolReference const& lhs, SymbolReference const& rhs)
{
    if ( lhs.name() != rhs.name() )
        return Invariant;

    binding_set_t bindings;
    return variance(ctx, bindings, lhs.pattern(), rhs.pattern());
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

DeclarationScope const* memberScope(Declaration const& decl)
{
    if ( auto var = decl.as<VariableDeclaration>() )
        return memberScope(*resolveIndirections(var->constraint()->declaration()));

    if ( auto field = decl.as<DataProductDeclaration::Field>() )
        return memberScope(*resolveIndirections(field->constraint().declaration()));

    if ( auto ds = decl.as<DataSumDeclaration>() )
        return ds->definition();

    if ( auto dp = decl.as<DataProductDeclaration>() )
        return dp->definition();

    // todo: imports
    return nullptr;
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

    result_t exprDot(DotExpression& d)
    {
        for ( auto const& e : d.expressions() )
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

    result_t exprDot(DotExpression const& d)
    {
        for ( auto const& e : d.expressions() )
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
