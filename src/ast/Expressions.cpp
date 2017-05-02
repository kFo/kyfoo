#include <kyfoo/ast/Expressions.hpp>

namespace kyfoo {
    namespace ast {

//
// PrimaryExpression

PrimaryExpression::PrimaryExpression(lexer::Token const& token)
    : myToken(token)
{
}

PrimaryExpression::~PrimaryExpression() = default;

void PrimaryExpression::io(IStream& stream) const
{
    stream.next("primary", myToken);
}

void PrimaryExpression::resolveSymbols(Diagnostics& dgn, Resolver& resolver)
{
    (void)dgn;
    (void)resolver;
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
    : myKind(kind)
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

void TupleExpression::resolveSymbols(Diagnostics& dgn, Resolver& resolver)
{
    (void)dgn;
    (void)resolver;
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

Expression* ConstraintExpression::subject()
{
    return mySubject.get();
}

Expression* ConstraintExpression::constraint()
{
    return myConstraint.get();
}

    } // namespace ast
} // namespace kyfoo
