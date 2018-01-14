#include <kyfoo/ast/Substitutions.hpp>

#include <algorithm>

#include <kyfoo/ast/Expressions.hpp>
#include <kyfoo/ast/Semantics.hpp>
#include <kyfoo/ast/Symbol.hpp>

namespace kyfoo {
    namespace ast {

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
Substitutions::Substitutions(Declaration const& target, Slice<Expression*> const& query)
{
    auto const& targetPattern = target.symbol().prototype().pattern();

    if ( auto proc = target.as<ProcedureDeclaration>() ) {
        auto const paramCount = targetPattern.size();
        if ( paramCount != query.size() ) {
            setMismatch();
            return;
        }

        for ( std::size_t i = 0; i < paramCount; ++i ) {
            auto const ordinal = proc->ordinal(i);
            if ( ordinal == -1 ) {
                if ( targetPattern[i]->declaration() )
                    continue;
            }
            else if ( proc->parameters()[ordinal]->dataType() ) {
                continue;
            }

            deduce(*targetPattern[i], *query[i]);
        }
    }
    else if ( target.symbol().prototype().metaVariableCount() ) {
        deduce(targetPattern, query);
    }

    if ( size() != target.symbol().prototype().metaVariableCount() )
        setMismatch();
}

Substitutions::Substitutions(Slice<Expression*> const& lhs, Slice<Expression*> const& rhs)
{
    deduce(lhs, rhs);
}

Substitutions::~Substitutions() = default;

bool Substitutions::deduce(Slice<Expression*> const& lhs, Slice<Expression*> const& rhs)
{
    auto const n = lhs.size();
    if ( n != rhs.size() )
        return false;

    bool ret = true;
    for ( std::size_t i = 0; i < n; ++i )
        ret &= deduce(*lhs[i], *rhs[i]);

    return ret;
}

bool Substitutions::deduce(Expression const& lhs, Expression const& rhs)
{
    auto l = lookThrough(lhs.declaration());
    if ( !l )
        l = &lhs;

    auto r = lookThrough(rhs.declaration());
    if ( !r )
        r = &rhs;

    // todo: generalized constraint substitution

    if ( l->declaration() ) {
        if ( auto param = l->declaration()->as<ProcedureParameter>() ) {
            if ( param->dataType() )
                return true;

            auto const dt = dataType(r->declaration());
            if ( !dt )
                return false;

            // todo: first constraint treated specially
            return deduce(*l->constraints()[0], *dt);
        }
    }

    if ( auto p = l->as<PrimaryExpression>() ) {
        if ( !p->declaration() )
            return false;

        if ( auto symVar = p->declaration()->as<SymbolVariable>() )
            return bind(*symVar, *r);

        return l->kind() == r->kind();
    }

    if ( auto ll = l->as<SymbolExpression>() ) {
        auto rr = r->as<SymbolExpression>();
        if ( !rr )
            return false;

        if ( ll->identifier().lexeme() != rr->identifier().lexeme() )
            return false;

        return deduce(ll->expressions(), rr->expressions());
    }

    if ( auto a = l->as<ApplyExpression>() ) {
        if ( auto s = r->as<SymbolExpression>() ) {
            auto subject = a->expressions().front()->as<PrimaryExpression>();
            if ( !subject )
                return false;

            if ( subject->token().lexeme() != s->identifier().lexeme() )
                return false;

            return deduce(a->expressions()(1, a->expressions().size()), s->expressions());
        }

        auto aa = r->as<ApplyExpression>();
        if ( !aa )
            return false;

        return deduce(a->expressions(), aa->expressions());
    }

    return l->kind() == r->kind();
}

bool Substitutions::deduce(Expression const& lhs, Declaration const& rhs)
{
    auto l = lookThrough(lhs.declaration());
    if ( !l )
        l = &lhs;

    if ( auto p = l->as<PrimaryExpression>() ) {
        if ( !p->declaration() )
            return false;

        if ( auto symVar = p->declaration()->as<SymbolVariable>() )
            return bind(*symVar, rhs);

        return p->token().kind() == lexer::TokenKind::Identifier && rhs.symbol().prototype().pattern().empty();
    }

    Slice<Expression*> exprs;
    if ( auto s = l->as<SymbolExpression>() ) {
        if ( s->identifier().lexeme() != rhs.symbol().identifier().lexeme() )
            return false;

        exprs = s->expressions();
    }
    else if ( auto a = l->as<ApplyExpression>() ) {
        auto p = a->expressions().front()->as<PrimaryExpression>();
        if ( !p || p->token().kind() != lexer::TokenKind::Identifier || p->token().lexeme() != rhs.symbol().identifier().lexeme() )
            return false;

        exprs = a->expressions()(1, a->expressions().size());
    }
    else {
        return false;
    }

    return deduce(exprs, rhs.symbol().prototype().pattern());
}

bool Substitutions::empty() const
{
    return myVariables.empty();
}

std::size_t Substitutions::size() const
{
    return myVariables.size();
}

SymbolVariable const& Substitutions::var(std::size_t index) const
{
    return *myVariables[index];
}

Declaration const& Substitutions::decl(std::size_t index) const
{
    return *myDeclarations[index];
}

Expression const& Substitutions::expr(std::size_t index) const
{
    return *myContexts[index];
}

bool Substitutions::bind(SymbolVariable const& symVar, Declaration const& decl)
{
    auto const i = findVarIndex(symVar);
    if ( i == myVariables.size() ) {
        // new substitution
        myBunkStorage.emplace_back(std::make_unique<PrimaryExpression>(lexer::Token()));
        myBunkStorage.back()->setDeclaration(decl);

        myVariables.push_back(&symVar);
        myDeclarations.push_back(&decl);
        myContexts.push_back(myBunkStorage.back().get());
        return true;
    }

    // existing substitution must be consistent
    // todo: print diagnostics on mismatch
    return myDeclarations[i] == &decl;
}

bool Substitutions::bind(SymbolVariable const& symVar, Expression const& expr)
{
    auto const i = findVarIndex(symVar);
    if ( i == myVariables.size() ) {
        // new substitution
        myVariables.push_back(&symVar);
        myDeclarations.push_back(expr.declaration());
        myContexts.push_back(&expr);
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

std::size_t Substitutions::findVarIndex(SymbolVariable const& symVar)
{
    return distance(begin(myVariables), find(begin(myVariables), end(myVariables), &symVar));
}

void Substitutions::setMismatch()
{
    myState |= Mismatch;
}

    } // namespace ast
} // namespace kyfoo
