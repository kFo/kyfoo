#include <tuple>

#include <kyfoo/lexer/Scanner.hpp>
#include <kyfoo/lexer/Token.hpp>

#include <kyfoo/Error.hpp>
#include <kyfoo/ast/ValueExpressions.hpp>
#include <kyfoo/ast/Scopes.hpp>
#include <kyfoo/ast/Semantics.hpp>

namespace kyfoo {
    namespace ast {

//
// ValueExpression

ValueExpression::ValueExpression() = default;
ValueExpression::~ValueExpression() = default;

void ValueExpression::io(IStream& stream)
{
    std::string exprkind = typeid(*this).name();
    stream.next("exprkind", exprkind);
}

//
// PrimaryExpression

PrimaryExpression::PrimaryExpression(lexer::Token const& token)
    : myToken(token)
{
}

void PrimaryExpression::io(IStream& stream)
{
    ValueExpression::io(stream);

    stream.next("primary", myToken);
}

void PrimaryExpression::resolveSymbols(Diagnostics&)
{

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
            return TupleKind::Open;
        else if ( close == lexer::TokenKind::CloseBracket )
            return TupleKind::HalfOpenLeft;
    }
    else if ( open == lexer::TokenKind::OpenBracket ) {
        if ( close == lexer::TokenKind::CloseParen )
            return TupleKind::HalfOpenRight;
        else if ( close == lexer::TokenKind::CloseBracket )
            return TupleKind::Closed;
    }

    throw std::runtime_error("invalid tuple expression syntax");
}

std::string to_string(TupleKind kind)
{
    switch (kind) {
    case TupleKind::Open: return "Open";
    case TupleKind::HalfOpenRight: return "HalfOpenRight";
    case TupleKind::HalfOpenLeft: return "HalfOpenLeft";
    case TupleKind::Closed: return "Closed";
    }

    throw std::runtime_error("invalid tuple kind");
}

TupleExpression::TupleExpression(std::vector<std::unique_ptr<ValueExpression>> expressions)
    : myKind(TupleKind::Open)
    , myExpressions(std::move(expressions))
{
}

TupleExpression::TupleExpression(lexer::Token open,
                                 lexer::Token close,
                                 std::vector<std::unique_ptr<ValueExpression>> expressions)
    : TupleExpression(std::move(expressions))
{

}

void TupleExpression::io(IStream& stream)
{
    ValueExpression::io(stream);

    auto kind = to_string(myKind);
    stream.next("kind", kind);
    stream.openArray("expressions");
    for ( auto&& e : myExpressions )
        stream.next("expression", e);
    stream.closeArray();
}

void TupleExpression::resolveSymbols(Diagnostics&)
{

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
    ValueExpression::io(stream);

    stream.next("subject", mySubject);
    stream.next("arguments", myArguments);
}

void ApplyExpression::resolveSymbols(Diagnostics&)
{

}

    } // namespace parser
} // namespace kyfoo
