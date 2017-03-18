#pragma once

#include <memory>

#include <kyfoo/lexer/Scanner.hpp>
#include <kyfoo/lexer/Token.hpp>
#include <kyfoo/ast/Types.hpp>

namespace kyfoo {
    namespace ast {

class ProceduralScope;

class Expression : public INode
{
public:
    Expression();
    ~Expression();

public:
    void io(IStream& stream) override;

private:
    std::unique_ptr<Type> myType;
};

class PrimaryExpression : public Expression
{
public:
    explicit PrimaryExpression(lexer::Token token);

public:
    void io(IStream& stream) override;

public:
    lexer::Token token() const;

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

TupleKind toTupleKind(lexer::TokenKind open, lexer::TokenKind close);
std::string to_string(TupleKind kind);

class TupleExpression : public Expression
{
public:
    explicit TupleExpression(std::vector<std::unique_ptr<Expression>> expressions);

    TupleExpression(lexer::Token open,
                    lexer::Token close,
                    std::vector<std::unique_ptr<Expression>> expressions);

public:
    void io(IStream& stream) override;

private:
    TupleKind myKind;
    std::vector<std::unique_ptr<Expression>> myExpressions;
};

class ApplyExpression : public Expression
{
public:
    explicit ApplyExpression(lexer::Token subject,
                             std::unique_ptr<TupleExpression> arguments);

public:
    void io(IStream& stream) override;

private:
    lexer::Token mySubject;
    std::unique_ptr<TupleExpression> myArguments;
};

    } // namespace ast
} // namespace kyfoo
