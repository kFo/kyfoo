#include <kyfoo/ast/Variance.hpp>

#include <kyfoo/BigInt.hpp>
#include <kyfoo/Interval.hpp>

#include <kyfoo/ast/Axioms.hpp>
#include <kyfoo/ast/Declarations.hpp>
#include <kyfoo/ast/Expressions.hpp>
#include <kyfoo/ast/Scopes.hpp>

namespace kyfoo::ast {

namespace {
    Interval<BigInt> bitsToBounds(int bits)
    {
        BigInt const c = BigInt(1) << std::abs(bits);
        if ( bits < 0 )
            return Interval<BigInt>(-(c >> 1), (c >> 1) - BigInt(1));

        return Interval<BigInt>(BigInt(0), c - BigInt(1));
    }

    AxiomsModule const& getAxioms(Declaration const& decl)
    {
        return decl.scope().module().axioms();
    }
} // namespace

Variance variance(lexer::Token const& target, lexer::Token const& query)
{
    return target.lexeme() == query.lexeme() ? Variance::Exact : Variance::Invariant;
}

Variance variance(DiagnosticsContext /*dgn*/, Declaration const& target, lexer::Token const& query)
{
    auto const& axioms = getAxioms(target);

    if ( query.kind() == lexer::TokenKind::Integer ) {
        if ( &target == axioms.intrinsic(IntegerLiteralType) )
            return Variance::Exact;
    }

    if ( query.kind() == lexer::TokenKind::String ) {
        if ( &target == axioms.intrinsic(StringLiteralType) )
            return Variance::Exact;
    }

    return Variance::Invariant;
}

Variance variance(DiagnosticsContext dgn, Declaration const& target, Declaration const& query)
{
    if ( &target == &query )
        return Variance::Exact;

    auto const& axioms = getAxioms(target);
    if ( auto targetInteger = axioms.integerMetaData(target) ) {
        if ( auto queryInteger = axioms.integerMetaData(query) ) {
            if ( targetInteger->bits == queryInteger->bits )
                return Variance::Exact;

            auto const targetBounds = bitsToBounds(targetInteger->bits);
            auto const queryBounds = bitsToBounds(queryInteger->bits);

            return subset(queryBounds, targetBounds) ? Variance::Covariant : Variance::Contravariant;
        }

        return Variance::Invariant;
    }

    // todo: removeme
    if ( &query == axioms.intrinsic(PointerNullLiteralType) ) {
        if ( descendsFromTemplate(axioms.intrinsic(PointerTemplate)->symbol(), target.symbol()) )
            return Variance::Covariant;

        return Variance::Invariant;
    }

    // todo: this is a hack for covariance
    if ( rootTemplate(target.symbol()) == rootTemplate(query.symbol()) ) {
        return variance(dgn,
                        target.symbol().prototype().pattern(),
                        query.symbol().prototype().pattern());
    }

    return Variance::Invariant;
}

Variance variance(DiagnosticsContext dgn,
                        Expression const& target,
                        Expression const& query)
{
    auto t = lookThrough(&target);
    auto q = lookThrough(&query);

    if ( !t ) {
        dgn.error(target) << "is an unresolved alias";
        return Variance::Invariant;
    }

    if ( !q ) {
        dgn.error(query) << "is an unresolved alias";
        return Variance::Invariant;
    }

    auto targetType = lookThrough(t->type());
    auto queryType = lookThrough(q->type());

    if ( !targetType ) {
        dgn.error(target) << "is not typed";
        return Variance::Invariant;
    }
    else if ( !queryType ) {
        dgn.error(query) << "is not typed";
        return Variance::Invariant;
    }

    if ( auto targetLiteral = t->as<LiteralExpression>() ) {
        if ( auto queryLiteral = q->as<LiteralExpression>() )
            return variance(targetLiteral->token(), queryLiteral->token());

        // todo: compile time execute
        return Variance::Invariant;
    }

    if ( auto targetDecl = getDeclaration(*t) ) {
        if ( isReference(*targetDecl) ) {
            if ( auto queryRef = getRefType(*q) )
                return variance(dgn, targetDecl->symbol().prototype().pattern(), *queryRef);

            if ( auto queryDecl = getDeclaration(*q) ) {
                if ( isReference(*queryDecl) )
                    return variance(dgn, targetDecl->symbol().prototype().pattern(), queryDecl->symbol().prototype().pattern());

                if ( auto queryBinder = getBinder(*queryDecl) )
                    return variance(dgn, *t, *queryType);
            }

            // todo: hack
            if ( auto app = q->as<AssignExpression>() )
                return variance(dgn, *t, *queryType);

            return Variance::Invariant;
        }
        else if ( auto queryDecl = getDeclaration(*q) ) {
            if ( isReference(*queryDecl) )
                if ( variance(dgn, *t, queryDecl->symbol().prototype().pattern()) )
                    return Variance::Covariant;
        }

        if ( auto targetBinder = getBinder(*targetDecl) ) {
            auto ret = variance(dgn, *targetBinder->type(), *q);
            if ( ret.invariant() )
                return variance(dgn, *targetBinder->type(), *q->type());

            return ret;
        }

        if ( auto queryLiteral = q->as<LiteralExpression>() )
            return variance(dgn, *targetDecl, queryLiteral->token());

        if ( auto queryDecl = getDeclaration(*q) ) {
            auto ret = variance(dgn, *targetDecl, *queryDecl);
            if ( !ret.invariant() )
                return ret;
        }

        if ( queryType->kind() == Expression::Kind::Universe )
            return Variance::Invariant;

        return variance(dgn, *t, *queryType);
    }

    if ( auto targetTuple = t->as<TupleExpression>() ) {
        auto queryTuple = q->as<TupleExpression>();
        if ( !queryTuple )
            return Variance::Invariant;

        if ( targetTuple->kind() != queryTuple->kind() )
            return Variance::Invariant;

        auto const l = targetTuple->elements();
        auto const r = queryTuple->elements();
        if ( l.size() != r.size() )
            return Variance::Invariant;

        Variance ret = Variance::Exact;
        for ( auto i = begin(l), j = begin(r); i != end(l); ++i, ++j ) {
            auto v = variance(dgn, **i, **j);
            if ( !v )
                return v;

            if ( !v.exact() )
                ret = Variance::Covariant;
        }

        return ret;
    }

    if ( auto targetArrow = t->as<ArrowExpression>() ) {
        auto queryArrow = q->as<ArrowExpression>();
        if ( !queryArrow ) {
            queryArrow = queryType->as<ArrowExpression>();
            if ( !queryArrow )
                return Variance::Invariant;
        }

        auto inputVariance = variance(dgn, targetArrow->from(), queryArrow->from());
        auto outputVariance = variance(dgn, targetArrow->to(), queryArrow->to());

        if ( outputVariance ) {
            if ( inputVariance.exact() )
                return outputVariance.exact() ? Variance::Exact : Variance::Covariant;
            else if ( inputVariance.contravariant() )
                return Variance::Covariant;
            else if ( inputVariance.covariant() )
                return Variance::Contravariant;
        }
        else if ( outputVariance.contravariant() ) {
            if ( inputVariance.exact() || inputVariance.covariant() )
                return Variance::Contravariant;
            else if ( inputVariance.contravariant() )
                return Variance::Covariant;
        }

        return Variance::Invariant;
    }

    if ( auto targetUniv = t->as<UniverseExpression>() ) {
        auto queryUniv = q->as<UniverseExpression>();
        if ( !queryUniv )
            return Variance::Invariant; // todo

        if ( targetUniv->level() == queryUniv->level() )
            return Variance::Exact;
        else if ( targetUniv->level() > queryUniv->level() )
            return Variance::Covariant;
        else
            return Variance::Contravariant;
    }

    return variance(dgn, *targetType, *queryType);
}

Variance variance(DiagnosticsContext dgn,
                  Slice<Expression const*> lhs,
                  Slice<Expression const*> rhs)
{
    auto const size = lhs.size();
    if ( size != rhs.size() )
        return Variance::Invariant;

    auto ret = Variance::Exact;
    for ( uz i = 0; i < size; ++i ) {
        auto v = variance(dgn, *lhs[i], *rhs[i]);
        if ( !v )
            return v;

        if ( !v.exact() )
            ret = Variance::Covariant;
    }

    return ret;
}

Variance variance(DiagnosticsContext dgn, Expression const& lhs, Slice<Expression const*> rhs)
{
    if ( auto l = lhs.as<TupleExpression>() )
        if ( l->kind() == TupleKind::Open )
            return variance(dgn, l->expressions(), rhs);

    if ( rhs.size() == 1 )
        return variance(dgn, lhs, *rhs[0]);

    return Variance::Invariant;
}

Variance variance(DiagnosticsContext dgn, Slice<Expression const*> lhs, Expression const& rhs)
{
    if ( auto r = rhs.as<TupleExpression>() )
        if ( r->kind() == TupleKind::Open )
            return variance(dgn, lhs, r->expressions());

    if ( lhs.size() == 1 )
        return variance(dgn, *lhs[0], rhs);

    return Variance::Invariant;
}

Variance variance(DiagnosticsContext dgn, SymbolReference const& lhs, SymbolReference const& rhs)
{
    if ( lhs.name() != rhs.name() )
        return Variance::Invariant;

    return variance(dgn, lhs.pattern(), rhs.pattern());
}

} // namespace kyfoo::ast
