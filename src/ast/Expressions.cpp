#include <tuple>

#include <kyfoo/lexer/Scanner.hpp>
#include <kyfoo/lexer/Token.hpp>

#include <kyfoo/Error.hpp>
#include <kyfoo/ast/Expressions.hpp>

namespace kyfoo {
    namespace ast {

//
// Expression

Expression::Expression() = default;
Expression::~Expression() = default;

void Expression::io(IStream& stream)
{
    std::string exprkind = typeid(*this).name();
    stream.next("exprkind", exprkind);
}

//
// PrimaryExpression

PrimaryExpression::PrimaryExpression(lexer::Token token)
    : myToken(token)
{
}

void PrimaryExpression::io(IStream& stream)
{
    Expression::io(stream);

    stream.next("primary", myToken);
}

lexer::Token PrimaryExpression::token() const
{
    return myToken;
}

//
// TupleExpression

TupleKind toTupleKind(lexer::TokenKind open, lexer::TokenKind close)
{
    if ( open == lexer::TokenKind::OpenParen ) {
        if ( close == lexer::TokenKind::CloseParen )
            return Open;
        else if ( close == lexer::TokenKind::CloseBracket )
            return HalfOpenLeft;
    }
    else if ( open == lexer::TokenKind::OpenBracket ) {
        if ( close == lexer::TokenKind::CloseParen )
            return HalfOpenRight;
        else if ( close == lexer::TokenKind::CloseBracket )
            return Closed;
    }

    throw std::runtime_error("invalid tuple expression syntax");
}

std::string to_string(TupleKind kind)
{
    switch (kind) {
    case Open: return "Open";
    case HalfOpenRight: return "HalfOpenRight";
    case HalfOpenLeft: return "HalfOpenLeft";
    case Closed: return "Closed";
    }

    throw std::runtime_error("invalid tuple kind");
}

TupleExpression::TupleExpression(std::vector<std::unique_ptr<Expression>> expressions)
    : myKind(Open)
    , myExpressions(std::move(expressions))
{
}

TupleExpression::TupleExpression(lexer::Token open,
                                 lexer::Token close,
                                 std::vector<std::unique_ptr<Expression>> expressions)
    : TupleExpression(std::move(expressions))
{

}

void TupleExpression::io(IStream& stream)
{
    Expression::io(stream);

    auto kind = to_string(myKind);
    stream.next("kind", kind);
    stream.openArray("expressions");
    for ( auto&& e : myExpressions )
        stream.next("expression", e);
    stream.closeArray();
}

//
// ApplyExpression

ApplyExpression::ApplyExpression(lexer::Token subject,
                                 std::unique_ptr<TupleExpression> arguments)
    : mySubject(subject)
    , myArguments(std::move(arguments))
{
}

void ApplyExpression::io(IStream& stream)
{
    Expression::io(stream);

    stream.next("subject", mySubject);
    stream.next("arguments", myArguments);
}

    } // namespace parser
} // namespace kyfoo
