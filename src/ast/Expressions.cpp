#include <tuple>

#include <kyfoo/lexer/Scanner.hpp>
#include <kyfoo/lexer/Token.hpp>

#include <kyfoo/Error.hpp>
#include <kyfoo/ast/Expressions.hpp>
#include <kyfoo/ast/Scopes.hpp>
#include <kyfoo/ast/Semantics.hpp>

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

void PrimaryExpression::resolveSymbols(Semantics& semantics)
{
    if ( token().kind() == lexer::TokenKind::Identifier )
    {
        auto decl = semantics.scope()->lookup(token().lexeme());
        if ( !decl )
            throw Error(token()) << "undefined identifier";

        switch (decl->kind()) {
        case DeclKind::Symbol:
            return;

        default:
            throw Error(token()) << "does not refer to a symbol or variable declaration";
        }
    }
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

TupleExpression::TupleExpression(std::vector<std::unique_ptr<Expression>> expressions)
    : myKind(TupleKind::Open)
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

void TupleExpression::resolveSymbols(Semantics& semantics)
{
    for ( auto&& e : myExpressions )
        e->resolveSymbols(semantics);
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

void ApplyExpression::resolveSymbols(Semantics& semantics)
{
    if ( mySubject.kind() != lexer::TokenKind::Identifier )
        throw std::logic_error("ApplyExpression subject must be an identifier");

    auto decl = semantics.scope()->lookup(mySubject.lexeme());
    if ( !decl )
        throw Error(mySubject) << "undefined identifier";

    if ( decl->kind() != DeclKind::Procedure )
        throw Error(mySubject) << "does not refer to a procedure";

    myArguments->resolveSymbols(semantics);
}

    } // namespace parser
} // namespace kyfoo
