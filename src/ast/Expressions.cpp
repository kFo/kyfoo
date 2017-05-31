#include <kyfoo/ast/Expressions.hpp>

#include <iterator>

#include <kyfoo/Diagnostics.hpp>

#include <kyfoo/ast/Semantics.hpp>

namespace kyfoo {
    namespace ast {

//
// Expression

Expression::Expression(Kind kind)
    : myKind(kind)
{
}

Expression::Kind Expression::kind() const
{
    return myKind;
}

//
// PrimaryExpression

PrimaryExpression::PrimaryExpression(lexer::Token const& token)
    : Expression(Expression::Kind::Primary)
    , myToken(token)
{
}

PrimaryExpression::~PrimaryExpression() = default;

void PrimaryExpression::io(IStream& stream) const
{
    stream.next("primary", myToken);
}

void PrimaryExpression::resolveSymbols(Diagnostics& dgn, IResolver& resolver)
{
    if ( myToken.kind() != lexer::TokenKind::Identifier )
        return;

    auto decl = resolver.lookup(myToken.lexeme());
    if ( !decl ) {
        dgn.error(resolver.module(), myToken) << "undeclared identifier";
        return;
    }

    myDeclaration = decl;
}

lexer::Token const& PrimaryExpression::token() const
{
    return myToken;
}

Declaration const* PrimaryExpression::declaration() const
{
    return myDeclaration;
}

//
// TupleExpression

TupleKind toTupleKind(lexer::TokenKind open, lexer::TokenKind close)
{
    if ( open == lexer::TokenKind::OpenParen ) {
        if ( close == lexer::TokenKind::CloseParen )
            return TupleKind::Open;
        else if ( close == lexer::TokenKind::CloseBracket )
            return TupleKind::OpenLeft;
    }
    else if ( open == lexer::TokenKind::OpenBracket ) {
        if ( close == lexer::TokenKind::CloseParen )
            return TupleKind::OpenRight;
        else if ( close == lexer::TokenKind::CloseBracket )
            return TupleKind::Closed;
    }
    else if ( open == lexer::TokenKind::OpenAngle
           && close == lexer::TokenKind::CloseAngle )
    {
        return TupleKind::Symbol;
    }

    throw std::runtime_error("invalid tuple expression syntax");
}

const char* to_string(TupleKind kind)
{
    switch (kind) {
#define X(a) case TupleKind::a: return #a;
        TUPLE_KINDS(X)
#undef X
    }

    throw std::runtime_error("invalid tuple kind");
}

TupleExpression::TupleExpression(TupleKind kind,
                                 std::vector<std::unique_ptr<Expression>>&& expressions)
    : Expression(Expression::Kind::Tuple)
    , myKind(kind)
    , myExpressions(std::move(expressions))
{
}

TupleExpression::~TupleExpression() = default;

void TupleExpression::io(IStream& stream) const
{
    stream.openArray(to_string(myKind));
    for ( auto const& e : myExpressions )
        e->io(stream);
    stream.closeArray();
}

void TupleExpression::resolveSymbols(Diagnostics& dgn, IResolver& resolver)
{
    for ( auto i = begin(myExpressions); i != end(myExpressions); ) {
        auto& e = *i;
        e->resolveSymbols(dgn, resolver);
        if ( e->kind() == Expression::Kind::Tuple ) {
            auto tuple = static_cast<TupleExpression*>(e.get());
            if ( tuple->kind() == TupleKind::Open ) {
                move(begin(tuple->expressions()), end(tuple->expressions()),
                     std::inserter(myExpressions, i));
                goto L_removeItem;
            }
        }

        ++i;
        continue;

    L_removeItem:
        i = myExpressions.erase(i);
    }

    if ( myKind == TupleKind::Apply ) {
        // TODO: type match
    }
}

TupleKind TupleExpression::kind() const
{
    return myKind;
}

std::vector<std::unique_ptr<Expression>> const& TupleExpression::expressions() const
{
    return myExpressions;
}

std::vector<std::unique_ptr<Expression>>& TupleExpression::expressions()
{
    return myExpressions;
}

//
// ConstraintExpression

ConstraintExpression::ConstraintExpression(std::unique_ptr<Expression> subject,
                                           std::unique_ptr<Expression> constraint)
    : Expression(Expression::Kind::Constraint)
    , mySubject(std::move(subject))
    , myConstraint(std::move(constraint))
{
    if ( !mySubject )
        throw std::runtime_error("constrain expression must have a subject");

    if ( !myConstraint )
        throw std::runtime_error("constrain expression must have a constraint");
}

ConstraintExpression::~ConstraintExpression() = default;

void ConstraintExpression::io(IStream& stream) const
{
    stream.next("subject", mySubject);
    stream.next("constraint", myConstraint);
}

void ConstraintExpression::resolveSymbols(Diagnostics& dgn, IResolver& resolver)
{
    mySubject->resolveSymbols(dgn, resolver);
    myConstraint->resolveSymbols(dgn, resolver);
}

Expression const* ConstraintExpression::subject() const
{
    return mySubject.get();
}

Expression const* ConstraintExpression::constraint() const
{
    return myConstraint.get();
}

    } // namespace ast
} // namespace kyfoo
