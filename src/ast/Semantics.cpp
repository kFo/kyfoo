#include <kyfoo/ast/Semantics.hpp>

#include <algorithm>
#include <functional>
#include <set>

#include <boost/multiprecision/cpp_int.hpp>
#include <boost/numeric/interval.hpp>

#include <kyfoo/Diagnostics.hpp>

#include <kyfoo/lexer/TokenKind.hpp>

#include <kyfoo/ast/Axioms.hpp>
#include <kyfoo/ast/Context.hpp>
#include <kyfoo/ast/Expressions.hpp>
#include <kyfoo/ast/Module.hpp>
#include <kyfoo/ast/Scopes.hpp>
#include <kyfoo/ast/Symbol.hpp>

namespace kyfoo {
    namespace lexer {
        bool isBefore(lexer::Token const& lhs, lexer::Token const& rhs)
        {
            return std::make_tuple(lhs.line(), lhs.column()) < std::make_tuple(rhs.line(), rhs.column());
        }
    }

    namespace ast {

    using boost::multiprecision::cpp_int;
    using boost::numeric::interval;
    using bounds_t = interval<cpp_int>;

namespace {
    template <typename T>
    bool compare(Slice<Expression const*> lhs,
                 Slice<Expression const*> rhs,
                 T&& op)
    {
        if ( lhs.size() != rhs.size() )
            return false;

        if ( lhs.empty() && rhs.empty() )
            return true;

        auto const size = lhs.size();
        for ( uz i = 0; i < size; ++i )
            if ( !op(*lhs[i], *rhs[i]) )
                return false;

        return true;
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

SymbolDependencyTracker::SymGroup* SymbolDependencyTracker::create(std::string name, uz arity)
{
    groups.emplace_back(mk<SymGroup>(std::move(name), arity));
    return groups.back().get();
}

SymbolDependencyTracker::SymGroup* SymbolDependencyTracker::findOrCreate(std::string_view name, uz arity)
{
    for ( auto const& e : groups)
        if ( e->name == name && e->arity == arity )
            return e.get();

    return create(std::string(name), arity);
}

void SymbolDependencyTracker::add(Declaration& decl)
{
    auto group = findOrCreate(decl.symbol().token().lexeme(), decl.symbol().prototype().pattern().size());
    group->add(decl);
}

void SymbolDependencyTracker::addDependency(Declaration& decl,
                                            std::string_view name,
                                            uz arity)
{
    auto group = findOrCreate(decl.symbol().token().lexeme(), decl.symbol().prototype().pattern().size());
    auto dependency = findOrCreate(name, arity);

    dependency->addDependent(*group);

    if ( group->pass <= dependency->pass ) {
        if ( !group->defer(group, dependency->pass + 1) ) {
            auto& err = dgn.error(mod, decl.symbol().token()) << "circular reference detected";
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

    result_t exprLiteral(LiteralExpression const&)
    {
        // nop
    }

    result_t exprIdentifier(IdentifierExpression const& p)
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
        auto subject = a.expressions()[0]->as<LiteralExpression>();
        if ( subject && subject->token().kind() == lexer::TokenKind::Identifier ) {
            tracker.addDependency(decl, subject->token().lexeme(), a.expressions().size() - 1);
            return;
        }

        for ( auto const& e : a.expressions() )
            dispatch(*e);
    }

    result_t exprSymbol(SymbolExpression const& s)
    {
        if ( s.token().kind() == lexer::TokenKind::Identifier )
            tracker.addDependency(decl, s.token().lexeme(), s.expressions().size());

        for ( auto const& e : s.expressions() )
            dispatch(*e);
    }

    result_t exprDot(DotExpression const& d)
    {
        for ( auto const& e : d.expressions() )
            dispatch(*e);
    }

    result_t exprAssign(AssignExpression const& v)
    {
        dispatch(v.left());
        dispatch(v.right());
    }

    result_t exprLambda(LambdaExpression const& l)
    {
        return declProcedure(l.procedure());
    }

    result_t exprArrow(ArrowExpression const& a)
    {
        dispatch(a.from());
        dispatch(a.to());
    }

    result_t exprUniverse(UniverseExpression const&)
    {
        // nop
    }

    result_t stmtExpression(Statement const& s)
    {
        dispatch(s.expression());
    }

    result_t juncBranch(BranchJunction const& b)
    {
        if ( b.condition() )
            dispatch(*b.condition());

        for ( auto const& stmt : b.scope()->statements() )
            dispatch(stmt.expression());

        if ( b.next() )
            exprBranch(*b.next());
    }

    result_t juncReturn(ReturnJunction const& r)
    {
        if ( r.expression() )
            dispatch(*r.expression());
    }

    result_t juncJump(JumpJunction const&)
    {
        // nop
    }

    // declarations

    void traceSymbol(Symbol const& sym)
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
        traceSymbol(dsCtor.symbol());
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
        if ( proc.returnType() )
            dispatch(*proc.returnType());
    }

    result_t declProcedureParameter(ProcedureParameter const&)
    {
        // nop
    }

    result_t declVariable(VariableDeclaration const& var)
    {
        traceSymbol();
        for ( auto const& c : var.constraints() )
            dispatch(*c);
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
    DeepApply<SymbolDependencyBuilder> op(tracker, decl);
    tracker.add(decl);
    op(decl);
}

template <typename O>
auto noncommute(O& o, Expression const& lhs, Expression const& rhs)
{
    auto other = [&o, &rhs](auto l) {
#define RHS(a,b) if ( auto r = rhs.as<b>() ) return o(*l, *r);
        EXPRESSION_KINDS(RHS)
#undef RHS
        throw std::runtime_error("invalid dispatch");
    };

#define LHS(a,b) if ( auto l = lhs.as<b>() ) return other(l);
    EXPRESSION_KINDS(LHS)
#undef LHS

    throw std::runtime_error("invalid dispatch");
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
        return matchEquivalent(l.expressions(), r.expressions());
    }

    // else

    bool operator()(Expression const& l_, Expression const& r_)
    {
        auto l = resolveIndirections(&l_);
        auto r = resolveIndirections(&r_);

        if ( !l || !r )
            throw std::runtime_error("unresolved indirection");

        if ( auto leftDecl = getDeclaration(*l) ) {
            auto rightDecl = getDeclaration(*r);
            if ( !rightDecl )
                return false;

            // todo: check var constraints
            if ( leftDecl->kind() == DeclKind::SymbolVariable )
                return rightDecl->kind() == DeclKind::SymbolVariable;

            if ( rightDecl->kind() == DeclKind::SymbolVariable )
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

VarianceResult variance(lexer::Token const& target, lexer::Token const& query)
{
    return target.lexeme() == query.lexeme() ? Exact : Invariant;
}

VarianceResult variance(Context& ctx, Declaration const& target, lexer::Token const& query)
{
    auto const& axioms = ctx.axioms();

    if ( auto intMeta = axioms.integerMetaData(target) ) {
        if ( query.kind() != lexer::TokenKind::Integer )
            return Invariant; // todo: diagnostics

        std::string s(query.lexeme());
        cpp_int const n(s);
        bounds_t const bounds = bitsToBounds(intMeta->bits);

        if ( !in(n, bounds) )
            return Contravariant; // todo: error diagnostics

        return Covariant;
    }

    if ( query.kind() == lexer::TokenKind::String ) {
        if ( &target == axioms.intrinsic(Sliceu8) )
            return Covariant;
    }

    return Invariant;
}

VarianceResult variance(Context& ctx, Declaration const& target, Declaration const& query)
{
    if ( &target == &query )
        return Exact;

    if ( auto targetInteger = ctx.axioms().integerMetaData(target) ) {
        if ( auto queryInteger = ctx.axioms().integerMetaData(query) ) {
            if ( targetInteger->bits == queryInteger->bits )
                return Exact;

            auto const targetBounds = bitsToBounds(targetInteger->bits);
            auto const queryBounds = bitsToBounds(queryInteger->bits);

            return subset(queryBounds, targetBounds) ? Covariant : Contravariant;
        }

        return Invariant;
    }

    // todo: removeme
    if ( &query == ctx.axioms().intrinsic(PointerNullLiteralType) ) {
        if ( descendsFromTemplate(ctx.axioms().intrinsic(PointerTemplate)->symbol(), target.symbol()) )
            return Covariant;

        return Invariant;
    }

    // todo: this is a hack for covariance
    if ( rootTemplate(target.symbol()) == rootTemplate(query.symbol()) ) {
        return variance(ctx,
                        target.symbol().prototype().pattern(),
                        query.symbol().prototype().pattern());
    }

    return Invariant;
}

VarianceResult variance(Context& ctx,
                        Expression const& target,
                        Expression const& query)
{
    auto t = lookThrough(&target);
    auto q = lookThrough(&query);

    if ( !t ) {
        ctx.error(target) << "is an unresolved alias";
        return Invariant;
    }

    if ( !q ) {
        ctx.error(query) << "is an unresolved alias";
        return Invariant;
    }

    auto targetType = lookThrough(t->type());
    auto queryType = lookThrough(q->type());

    if ( !targetType ) {
        ctx.error(target) << "is not typed";
        return Invariant;
    }
    else if ( !queryType ) {
        ctx.error(query) << "is not typed";
        return Invariant;
    }

    if ( auto targetLiteral = t->as<LiteralExpression>() ) {
        if ( auto queryLiteral = q->as<LiteralExpression>() )
            return variance(targetLiteral->token(), queryLiteral->token());

        // todo: compile time execute
        return Invariant;
    }

    if ( auto targetDecl = getDeclaration(*t) ) {
        if ( isReference(*targetDecl) ) {
            if ( auto queryRef = getRefType(*q) )
                return variance(ctx, targetDecl->symbol().prototype().pattern(), *queryRef);

            if ( auto queryDecl = getDeclaration(*q) ) {
                if ( isReference(*queryDecl) )
                    return variance(ctx, targetDecl->symbol().prototype().pattern(), queryDecl->symbol().prototype().pattern());

                if ( auto queryBinder = getBinder(*queryDecl) )
                    return variance(ctx, *t, *queryType);
            }

            // todo: hack
            if ( auto app = q->as<AssignExpression>() )
                return variance(ctx, *t, *queryType);

            return Invariant;
        }
        else if ( auto queryDecl = getDeclaration(*q) ) {
            if ( isReference(*queryDecl) )
                if ( variance(ctx, *t, queryDecl->symbol().prototype().pattern()) )
                    return Covariant;
        }

        if ( auto targetBinder = getBinder(*targetDecl) ) {
            auto ret = variance(ctx, *targetBinder->type(), *q);
            if ( ret.invariant() )
                return variance(ctx, *targetBinder->type(), *q->type());

            return ret;
        }

        if ( auto queryLiteral = q->as<LiteralExpression>() )
            return variance(ctx, *targetDecl, queryLiteral->token());

        if ( auto queryDecl = getDeclaration(*q) ) {
            auto ret = variance(ctx, *targetDecl, *queryDecl);
            if ( !ret.invariant() )
                return ret;
        }

        if ( queryType->kind() == Expression::Kind::Universe )
            return Invariant;

        return variance(ctx, *t, *queryType);
    }

    if ( auto targetTuple = t->as<TupleExpression>() ) {
        auto queryTuple = q->as<TupleExpression>();
        if ( !queryTuple )
            return Invariant;

        if ( targetTuple->kind() != queryTuple->kind() )
            return Invariant;

        auto const l = targetTuple->elements();
        auto const r = queryTuple->elements();
        if ( l.size() != r.size() )
            return Invariant;

        VarianceResult ret = Exact;
        for ( auto i = begin(l), j = begin(r); i != end(l); ++i, ++j ) {
            auto v = variance(ctx, **i, **j);
            if ( !v )
                return v;

            if ( !v.exact() )
                ret = Covariant;
        }

        return ret;
    }

    if ( auto targetArrow = t->as<ArrowExpression>() ) {
        auto queryArrow = q->as<ArrowExpression>();
        if ( !queryArrow ) {
            queryArrow = queryType->as<ArrowExpression>();
            if ( !queryArrow )
                return Invariant;
        }

        auto inputVariance = variance(ctx, targetArrow->from(), queryArrow->from());
        auto outputVariance = variance(ctx, targetArrow->to(), queryArrow->to());

        if ( outputVariance ) {
            if ( inputVariance.exact() )
                return outputVariance.exact() ? Exact : Covariant;
            else if ( inputVariance.contravariant() )
                return Covariant;
            else if ( inputVariance.covariant() )
                return Contravariant;
        }
        else if ( outputVariance.contravariant() ) {
            if ( inputVariance.exact() || inputVariance.covariant() )
                return Contravariant;
            else if ( inputVariance.contravariant() )
                return Covariant;
        }

        return Invariant;
    }

    if ( auto targetUniv = t->as<UniverseExpression>() ) {
        auto queryUniv = q->as<UniverseExpression>();
        if ( !queryUniv )
            return Invariant; // todo

        if ( targetUniv->level() == queryUniv->level() )
            return Exact;
        else if ( targetUniv->level() > queryUniv->level() )
            return Covariant;
        else
            return Contravariant;
    }

    return variance(ctx, *targetType, *queryType);
}

VarianceResult variance(Context& ctx,
                        Slice<Expression const*> lhs,
                        Slice<Expression const*> rhs)
{
    auto const size = lhs.size();
    if ( size != rhs.size() )
        return Invariant;

    auto ret = Exact;
    for ( uz i = 0; i < size; ++i ) {
        auto v = variance(ctx, *lhs[i], *rhs[i]);
        if ( !v )
            return v;

        if ( !v.exact() )
            ret = Covariant;
    }

    return ret;
}

VarianceResult variance(Context& ctx, Expression const& lhs, Slice<Expression const*> rhs)
{
    if ( auto l = lhs.as<TupleExpression>() )
        if ( l->kind() == TupleKind::Open )
            return variance(ctx, l->expressions(), rhs);

    if ( rhs.size() == 1 )
        return variance(ctx, lhs, *rhs[0]);

    return Invariant;
}

VarianceResult variance(Context& ctx, Slice<Expression const*> lhs, Expression const& rhs)
{
    if ( auto r = rhs.as<TupleExpression>() )
        if ( r->kind() == TupleKind::Open )
            return variance(ctx, lhs, r->expressions());

    if ( lhs.size() == 1 )
        return variance(ctx, *lhs[0], rhs);

    return Invariant;
}

VarianceResult variance(Context& ctx, SymbolReference const& lhs, SymbolReference const& rhs)
{
    if ( lhs.name() != rhs.name() )
        return Invariant;

    return variance(ctx, lhs.pattern(), rhs.pattern());
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
        case DeclKind::Symbol:
            expr = static_cast<SymbolDeclaration const*>(decl)->expression();
            break;
        case DeclKind::SymbolVariable:
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
        case DeclKind::Symbol:
            next = static_cast<SymbolDeclaration const*>(decl)->expression();
            break;
        case DeclKind::SymbolVariable:
            next = static_cast<SymbolVariable const*>(decl)->boundExpression();
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

bool hasSubstitutions(Symbol const& sym)
{
    if ( sym.prototype().symbolVariables().empty() )
        return false;

    for ( auto& v : sym.prototype().symbolVariables() )
        if ( !v->boundExpression() )
            return false;

    return true;
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

bool isReference(Declaration const& decl)
{
    return descendsFromTemplate(decl.scope().module().axioms().intrinsic(ReferenceTemplate)->symbol(), decl.symbol());
}

bool isReference(Context const& ctx, Symbol const& sym)
{
    return descendsFromTemplate(ctx.axioms().intrinsic(ReferenceTemplate)->symbol(), sym);
}

bool isReference(Expression const& expr)
{
    if ( auto d = getDeclaration(expr) )
        return isReference(*d);

    return false;
}

DeclarationScope const* memberScope(Declaration const& decl_)
{
    auto decl = resolveIndirections(decl_);
    if ( auto b = getBinder(*decl) ) {
        auto d = resolveIndirections(getDeclaration(b->type()));
        if ( !d )
            return nullptr;

        return memberScope(*d);
    }

    while ( isReference(*decl) )
        decl = resolveIndirections(getDeclaration(decl->symbol().prototype().pattern().front()));

    if ( auto ds = decl->as<DataSumDeclaration>() )
        return ds->definition();

    if ( auto dp = decl->as<DataProductDeclaration>() )
        return dp->definition();

    // todo: imports
    return nullptr;
}

TemplateDeclaration const* procTemplate(ProcedureDeclaration const& proc)
{
    if ( !proc.scope().declaration() )
        return nullptr;

    return proc.scope().declaration()->as<TemplateDeclaration>();
}

Declaration const* outerDataDeclaration(Declaration const& decl)
{
    for ( auto scope = &decl.scope(); scope; scope = scope->parent() ) {
        if ( !scope->declaration() )
            return nullptr;

        if ( isDataDeclaration(scope->declaration()->kind()) )
            return scope->declaration();
    }

    return nullptr;
}

Declaration* outerDataDeclaration(Declaration& decl)
{
    return const_cast<Declaration*>(outerDataDeclaration(const_cast<Declaration const&>(decl)));
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

DataProductDeclaration const* methodType(ProcedureDeclaration const& proc)
{
    auto decl = outerDataDeclaration(proc);
    if ( !decl )
        return nullptr;

    if ( auto dp = decl->as<DataProductDeclaration>() )
        return dp;

    return nullptr;
}

Expression const* dataType(Expression const& expr_)
{
    auto expr = resolveIndirections(&expr_);
    if ( auto decl = getDeclaration(*expr) ) {
        if ( isDataDeclaration(decl->kind()) )
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

template <typename Dispatcher>
struct MetaVariableVisitor
{
    using result_t = void;
    Dispatcher& dispatch;
    
    using visitor_t = std::function<void(IdentifierExpression&)>;
    visitor_t visitor;

    MetaVariableVisitor(Dispatcher& dispatch, visitor_t visitor)
        : dispatch(dispatch)
        , visitor(visitor)
    {
    }

    result_t exprLiteral(LiteralExpression&)
    {
        // nop
    }

    result_t exprIdentifier(IdentifierExpression& p)
    {
        if ( p.token().kind() == lexer::TokenKind::MetaVariable )
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

    result_t exprAssign(AssignExpression& v)
    {
        dispatch(v.left());
        dispatch(v.right());
    }

    result_t exprLambda(LambdaExpression& l)
    {
        for ( auto p : l.procedure().parameters() )
            for ( auto c : p->constraints() )
                dispatch(*c);

        for ( auto c : l.procedure().result()->constraints() )
            dispatch(*c);

        // todo: defn
    }

    result_t exprArrow(ArrowExpression& a)
    {
        dispatch(a.from());
        dispatch(a.to());
    }

    result_t exprUniverse(UniverseExpression&)
    {
        // nop
    }

    result_t stmtExpression(Statement& e)
    {
        return dispatch(e.expression());
    }

    result_t juncBranch(BranchJunction& b)
    {
        if ( b.condition() )
            dispatch(*b.condition());

        for ( uz i = 0; i < 2; ++i )
            if ( b.branch(i) )
                dispatch.procScope(*b.branch(i)->scope());
    }

    result_t juncReturn(ReturnJunction& r)
    {
        if ( r.expression() )
            dispatch(*r.expression());
    }

    result_t juncJump(JumpJunction&)
    {
        // nop
    }
};

template <typename F>
void visitMetaVariables(Expression& expr, F&& f)
{
    DeepApply<MetaVariableVisitor> op(f);
    op(expr);
}

std::vector<IdentifierExpression*> gatherMetaVariables(Expression& expr)
{
    std::vector<IdentifierExpression*> ret;
    visitMetaVariables(expr, [&ret](IdentifierExpression& p) {
        ret.push_back(&p);
    });

    return ret;
}

template <typename Dispatcher>
struct HasMetaVariable
{
    using result_t = bool;
    Dispatcher& dispatch;

    HasMetaVariable(Dispatcher& dispatch)
        : dispatch(dispatch)
    {
    }

    result_t exprLiteral(LiteralExpression const&)
    {
        return false;
    }

    result_t exprIdentifier(IdentifierExpression const& p)
    {
        return p.token().kind() == lexer::TokenKind::MetaVariable;
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

    result_t exprAssign(AssignExpression const& v)
    {
        return dispatch(v.left()) || dispatch(v.right());
    }

    result_t exprLambda(LambdaExpression const& l)
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

    result_t exprArrow(ArrowExpression const& a)
    {
        return dispatch(a.from()) || dispatch(a.to());
    }

    result_t exprUniverse(UniverseExpression const&)
    {
        return false;
    }

    result_t stmtExpression(Statement const& e)
    {
        return dispatch(e.expression());
    }

    result_t juncBranch(BranchJunction const& b)
    {
        if ( b.condition() && dispatch(*b.condition()) )
            return true;

        for ( auto& bb : b.scope()->basicBlocks() ) {
            for ( auto& s : bb->statements() )
                if ( dispatch(s) )
                    return true;

            if ( bb->junction() && dispatch(*bb->junction()) )
                return true;
        }

        return false;
    }

    result_t juncReturn(ReturnJunction const& r)
    {
        if ( r.expression() )
            return dispatch(*r.expression());

        return false;
    }

    result_t juncJump(JumpJunction const&)
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
    using result_t = lexer::Token const&;

    Dispatcher& dispatch;

    FrontToken(Dispatcher& dispatch)
        : dispatch(dispatch)
    {
    }

    result_t exprLiteral(LiteralExpression const& p)
    {
        return p.token();
    }

    result_t exprIdentifier(IdentifierExpression const& p)
    {
        return p.token();
    }

    result_t exprTuple(TupleExpression const& t)
    {
        if ( t.expressions().empty() )
            return t.openToken();

        return dispatch(*t.expressions()[0]);
    }

    result_t exprApply(ApplyExpression const& a)
    {
        return dispatch(*a.expressions()[0]);
    }

    result_t exprSymbol(SymbolExpression const& s)
    {
        if ( s.token().kind() != lexer::TokenKind::Undefined )
            return s.token();

        if ( s.expressions().empty() )
            return s.openToken();

        return dispatch(*s.expressions()[0]);
    }

    result_t exprDot(DotExpression const& d)
    {
        return dispatch(*d.expressions()[0]);
    }

    result_t exprAssign(AssignExpression const& v)
    {
        return dispatch(v.left());
    }

    result_t exprLambda(LambdaExpression const& l)
    {
        if ( !l.procedure().parameters().empty() )
            return l.procedure().parameters().front()->symbol().token();

        return l.yieldToken();
    }

    result_t exprArrow(ArrowExpression const& a)
    {
        return dispatch(a.from());
    }

    result_t exprUniverse(UniverseExpression const&)
    {
        throw std::runtime_error("no front token for universe-expression");
    }

    result_t stmtExpression(Statement const& e)
    {
        return dispatch(e.expression());
    }

    result_t juncBranch(BranchJunction const& b)
    {
        return b.token();
    }

    result_t juncReturn(ReturnJunction const& r)
    {
        return r.token();
    }

    result_t juncJump(JumpJunction const& j)
    {
        return j.token();
    }
};

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

template <typename Dispatcher>
struct PrintOperator
{
    using result_t = std::ostream&;

    Dispatcher& dispatch;
    result_t stream;
    int nest = 0;

    PrintOperator(Dispatcher& dispatch, result_t stream)
        : dispatch(dispatch)
        , stream(stream)
    {
    }

    result_t printConstraints(Expression const& expr)
    {
        for ( auto c : expr.constraints() ) {
            stream << " : ";
            dispatch(*c);
        }

        return stream;
    }

    result_t printType(Expression const& expr)
    {
        if ( expr.type() ) {
            stream << " : ";
            return dispatch(*expr.type());
        }

        return stream << " : ~err";
    }

    result_t showTyped(Expression const& expr)
    {
        dispatch(expr);
        return printType(expr);
    }

    result_t exprLiteral(LiteralExpression const& p)
    {
        return stream << p.token().lexeme();
    }

    result_t exprIdentifier(IdentifierExpression const& id)
    {
        return stream << id.token().lexeme();
    }

    result_t exprTuple(TupleExpression const& t)
    {
        stream << presentTupleOpen(t.kind());

        if ( !t.expressions().empty() ) {
            showTyped(*t.expressions()[0]);

            for ( auto const& e : t.expressions()(1, t.expressions().size()) ) {
                stream << presentTupleWeave(t.kind());
                showTyped(*e);
            }
        }

        return stream << presentTupleClose(t.kind());;
    }

    result_t exprApply(ApplyExpression const& a)
    {
        if ( nest )
            stream << "(";

        ++nest;
        auto first = true;
        for ( auto const& e : a.expressions() ) {
            if ( !first )
                stream << " ";
            else
                first = false;

            dispatch(*e);
        }
        --nest;
        if ( nest )
            stream << ")";

        return stream;
    }

    result_t exprSymbol(SymbolExpression const& s)
    {
        auto const& id = s.token().lexeme();
        if ( !id.empty() )
            stream << id;

        if ( !s.expressions().empty() ) {
            stream << '<';
            dispatch(*s.expressions()[0]);

            for ( auto const& e : s.expressions()(1, s.expressions().size()) ) {
                stream << ", ";
                dispatch(*e);
            }

            return stream << '>';
        }

        if ( id.empty() )
            stream << "<>";

        return stream;
    }

    result_t exprDot(DotExpression const& d)
    {
        if ( d.isModuleScope() )
            stream << ".";

        auto l = begin(d.expressions());
        auto r = end(d.expressions());
        if ( l != r )
            dispatch(*(*l));

        ++l;
        for ( ; l != r; ++l ) {
            stream << ".";
            dispatch(*(*l));
        }

        return stream;
    }

    result_t exprAssign(AssignExpression const& v)
    {
        if ( auto decl = getDeclaration(v.left()) ) {
            if ( auto var = decl->as<VariableDeclaration>() )
                if ( var->symbol().token().lexeme().empty() )
                    return dispatch(v.right());
        }

        if ( nest )
            stream << "(";

        dispatch(v.left());
        stream << " = ";
        dispatch(v.right());

        if ( nest )
            stream << ")";

        return stream;
    }

    result_t exprLambda(LambdaExpression const& l)
    {
        stream << "(";
        for ( auto p : l.procedure().parameters() ) {
            stream << p->symbol().token().lexeme();
            if ( !p->constraints().empty() )
                stream << " : ";

            for ( auto c : p->constraints() )
                dispatch(*c);
        }

        return stream << ")";
    }

    result_t exprArrow(ArrowExpression const& a)
    {
        dispatch(a.from());
        stream << " -> ";
        return dispatch(a.to());
    }

    result_t exprUniverse(UniverseExpression const& u)
    {
        return stream << "Universe<" << u.level() << ">";
    }

    result_t stmtExpression(Statement const& s)
    {
        return dispatch(s.expression());
    }

    result_t juncBranch(BranchJunction const& b)
    {
        stream << ":? ";
        return dispatch(*b.condition());
    }

    result_t juncReturn(ReturnJunction const& r)
    {
        stream << "return ";
        if ( r.expression() )
            return dispatch(*r.expression());

        return stream;
    }

    result_t juncJump(JumpJunction const& j)
    {
        stream << j.token().lexeme();
        if ( j.targetLabel().kind() != lexer::TokenKind::Undefined )
            stream << j.targetLabel().lexeme();

        return stream;
    }
};

std::ostream& print(std::ostream& stream, Expression const& expr)
{
    ShallowApply<PrintOperator> op(stream);
    return op(expr);
}

std::ostream& print(std::ostream& stream, Statement const& stmt)
{
    ShallowApply<PrintOperator> op(stream);
    return op(stmt);
}

std::ostream& print(std::ostream& stream, Junction const& junc)
{
    ShallowApply<PrintOperator> op(stream);
    return op(junc);
}

template <typename Dispatcher>
struct LevelFinder
{
    using result_t = uz;

    Dispatcher& dispatch;

    LevelFinder(Dispatcher& dispatch)
        : dispatch(dispatch)
    {
    }

    result_t exprLiteral(LiteralExpression const&)
    {
        return 0;
    }

    result_t exprIdentifier(IdentifierExpression const&)
    {
        return 1; // todo: higher level types
    }

    result_t max(Slice<Expression const*> exprs)
    {
        if ( exprs.empty() )
            return 0;

        auto max = dispatch(*exprs.front());
        for ( auto const& e : slice(exprs, 1) )
            max = std::max(max, dispatch(*e));

        return max;
    }

    result_t exprTuple(TupleExpression const& t)
    {
        return max(t.expressions());
    }

    result_t exprApply(ApplyExpression const& a)
    {
        return max(a.expressions());
    }

    result_t exprSymbol(SymbolExpression const& s)
    {
        return exprIdentifier(s);
    }

    result_t exprDot(DotExpression const& d)
    {
        return dispatch(*d.expressions().back());
    }

    result_t exprAssign(AssignExpression const& v)
    {
        return std::max(dispatch(v.left()), dispatch(v.right()));
    }

    result_t exprLambda(LambdaExpression const& l)
    {
        result_t ret = 1;
        for ( auto const p : l.procedure().parameters() )
            ret = std::max(ret, dispatch(*p->type()));

        return std::max(ret, dispatch(*l.procedure().result()->type()));
    }

    result_t exprArrow(ArrowExpression const& a)
    {
        return std::max(dispatch(a.from()), dispatch(a.to()));
    }

    result_t exprUniverse(UniverseExpression const& u)
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
} // namespace kyfoo
