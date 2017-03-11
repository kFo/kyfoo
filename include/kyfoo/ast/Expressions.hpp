#pragma once

#include <memory>

#include <kyfoo/lexer/Scanner.hpp>
#include <kyfoo/lexer/Token.hpp>
#include <kyfoo/ast/Types.hpp>

namespace kyfoo {
    namespace ast {

class ProceduralScope;

class Expression
{
public:
    Expression();
    ~Expression();

private:
    std::unique_ptr<Type> myType;
};

class PrimaryExpression : public Expression
{
public:
    explicit PrimaryExpression(lexer::Token token)
        : myToken(token)
    {
    }

public:
    lexer::Token token() const { return myToken; }

private:
    lexer::Token myToken;
};

enum TupleKind
{
    Open,
    HalfOpenRight,
    HalfOpenLeft,
    Closed
};

inline TupleKind toTupleKind(lexer::TokenKind open, lexer::TokenKind close)
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

class TupleExpression : public Expression
{
public:
    explicit TupleExpression(std::vector<std::unique_ptr<Expression>> expressions)
        : myKind(Open)
        , myExpressions(std::move(expressions))
    {
    }

    TupleExpression(lexer::Token open,
                    lexer::Token close,
                    std::vector<std::unique_ptr<Expression>> expressions)
        : TupleExpression(std::move(expressions))
    {

    }

private:
    TupleKind myKind;
    std::vector<std::unique_ptr<Expression>> myExpressions;
};

class ApplyExpression : public Expression
{
public:
    explicit ApplyExpression(lexer::Token subject,
                             std::unique_ptr<TupleExpression> arguments)
        : mySubject(subject)
        , myArguments(std::move(arguments))
    {
    }

private:
    lexer::Token mySubject;
    std::unique_ptr<TupleExpression> myArguments;
};

std::unique_ptr<Expression> parseExpression(lexer::Scanner& scanner);

    } // namespace ast
} // namespace kyfoo
