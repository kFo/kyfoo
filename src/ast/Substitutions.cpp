#include <kyfoo/ast/Substitutions.hpp>

#include <algorithm>

#include <kyfoo/ast/Expressions.hpp>
#include <kyfoo/ast/Fabrication.hpp>
#include <kyfoo/ast/Semantics.hpp>
#include <kyfoo/ast/Symbol.hpp>

namespace kyfoo::ast {

Substitutions::Substitutions() = default;

Substitutions::Substitutions(BYOS, Declaration const& target, Slice<Item> items)
{
    auto symVars = target.symbol().prototype().symbolVariables();
    auto const symVarCount = symVars.card();
    ENFORCE(symVarCount == items.card(), "substitution mismatch");

    for ( uz i = 0; i < symVarCount; ++i ) {
        ENFORCE(bind(*items[i].symVar, *items[i].expr), "substituion mismatch");
    }
}

Substitutions::Substitutions(BYOS, Declaration const& target, Slice<Expression const*> exprs)
{
    auto symVars = target.symbol().prototype().symbolVariables();
    auto const symVarCount = symVars.card();
    ENFORCE(symVarCount == exprs.card(), "substitution mismatch");

    for ( uz i = 0; i < symVarCount; ++i ) {
        ENFORCE(bind(*symVars[i], *exprs[i]), "substituion mismatch");
    }
}

/**
 * Determines suitable substitutions for any meta-variables in \a target
 *
 * \param substs Substitution table
 * \param target Declaration whose pattern has unbound meta-variables
 * \param query  Query expressions used as arguments
 *
 * \precondition All expressions in \a query are resolved
 *
 * \note No substitution is determined for any expression in \a target that is already resolved
 */
Substitutions::Substitutions(Declaration const& target, Slice<Expression const*> query)
{
    auto const& targetPattern = target.symbol().prototype().pattern();

    if ( auto proc = target.as<ProcedureDeclaration>() ) {
        auto const paramCount = targetPattern.card();
        if ( paramCount != query.card() ) {
            setMismatch();
            return;
        }

        for ( uz i = 0; i < paramCount; ++i ) {
            auto const ordinal = proc->ordinals()[i];
            if ( ordinal == -1 ) {
                if ( targetPattern[i]->type() )
                    continue;
            }
            else if ( proc->parameters()[ordinal]->type() ) {
                continue;
            }

            deduce(*targetPattern[i], *query[i]);
        }
    }
    else if ( target.symbol().prototype().metaVariableCount() ) {
        deduce(targetPattern, query);
    }

    if ( card() != target.symbol().prototype().metaVariableCount() )
        setMismatch();
}

Substitutions::Substitutions(Slice<Expression const*> target, Slice<Expression const*> query)
{
    deduce(target, query);
}

Substitutions::~Substitutions() = default;

bool Substitutions::deduce(Slice<Expression const*> target, Slice<Expression const*> query)
{
    auto const n = target.card();
    if ( n != query.card() )
        return false;

    bool ret = true;
    for ( uz i = 0; i < n; ++i )
        ret &= deduce(*target[i], *query[i]);

    return ret;
}

bool Substitutions::deduce(Slice<Expression const*> target, Expression const& query)
{
    if ( auto tup = query.as<TupleExpression>() )
        return deduce(target, tup->expressions());

    if ( target.card() == 1 )
        return deduce(*target[0], query);

    return false;
}

bool Substitutions::deduce(Expression const& target, Slice<Expression const*> query)
{
    if ( auto tup = target.as<TupleExpression>() )
        return deduce(tup->expressions(), query);

    if ( query.card() == 1 )
        return deduce(target, *query[0]);

    return false;
}

bool Substitutions::deduce(Expression const& target, Expression const& query)
{
    // todo: generalized constraint substitution

    auto l = resolveIndirections(&target);
    auto r = resolveIndirections(&query);

    auto tryImplicitRef = [&]() {
        if ( auto rr = r->as<SymbolExpression>() )
            if ( rr->token().lexeme() == "ref" )
                return deduce(*l, rr->expressions());

        return false;
    };

    if ( auto targetId = l->as<IdentifierExpression>() ) {
        if ( auto targetDecl = resolveIndirections(targetId->declaration()) ) {
            if ( auto param = targetDecl->as<ProcedureParameter>() ) {
                if ( param->type() )
                    return true;

                // todo: first constraint treated specially
                return deduce(*param->constraints()[0], *r->type());
            }

            if ( auto symVar = targetDecl->as<SymbolVariable>() )
                return bind(*symVar, *r);
        }

        return tryImplicitRef();
    }

    if ( auto lit = l->as<LiteralExpression>() ) {
        if ( !lit->type() )
            return false;

        return lit->type() == r->type();
    }

    if ( auto ll = l->as<SymbolExpression>() ) {
        auto rr = r->as<SymbolExpression>();
        if ( !rr )
            return false;

        if ( ll->token().lexeme() == rr->token().lexeme() )
            if ( deduce(ll->expressions(), rr->expressions()) )
                return true;

        // todo: generalize to deduction guides
        if ( ll->token().lexeme() == "ref" )
            return deduce(ll->expressions(), *r);

        return tryImplicitRef();
    }

    if ( auto a = l->as<ApplyExpression>() ) {
        if ( auto s = r->as<SymbolExpression>() ) {
            auto subject = a->expressions().front()->as<IdentifierExpression>();
            if ( !subject )
                return false;

            if ( subject->token().lexeme() != s->token().lexeme() ) {
                // todo: generalize to deduction guides
                if ( subject->token().lexeme() == "ref" )
                    return deduce(a->arguments(), *r);

                return tryImplicitRef();
            }

            return deduce(a->expressions()(1, $), s->expressions());
        }

        auto aa = r->as<ApplyExpression>();
        if ( !aa ) {
            // todo: generalize to deduction guides
            if ( auto id = a->expressions().front()->as<IdentifierExpression>() )
                if ( id->token().lexeme() == "ref" )
                    return deduce(a->arguments(), *r);

            return tryImplicitRef();
        }

        return deduce(a->expressions(), aa->expressions());
    }

    return l->kind() == r->kind();
}

bool Substitutions::empty() const
{
    return !myVariables;
}

uz Substitutions::card() const
{
    return myVariables.card();
}

SymbolVariable const& Substitutions::var(uz index) const
{
    return *myVariables[index];
}

Expression const& Substitutions::expr(uz index) const
{
    return *myContexts[index];
}

bool Substitutions::bind(SymbolVariable const& symVar, Expression const& expr)
{
    auto const i = findVarIndex(symVar);
    if ( i == myVariables.card() ) {
        // new substitution
        myVariables.append(&symVar);
        myContexts.append(&expr);
        return true;
    }

    // existing substitution must be consistent
    // todo: print diagnostics on mismatch
    return matchEquivalent(*myContexts[i], expr);
}

Substitutions::operator bool() const
{
    return myState == 0;
}

uz Substitutions::findVarIndex(SymbolVariable const& symVar)
{
    return std::distance(begin(myVariables), std::find(begin(myVariables), end(myVariables), &symVar));
}

void Substitutions::setMismatch()
{
    myState |= Mismatch;
}

} // namespace kyfoo::ast
