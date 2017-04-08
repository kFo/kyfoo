#pragma once

#include <memory>

#include <kyfoo/lexer/Scanner.hpp>
#include <kyfoo/lexer/Token.hpp>
#include <kyfoo/ast/Node.hpp>
#include <kyfoo/ast/Tuples.hpp>
#include <kyfoo/ast/Types.hpp>

namespace kyfoo {
    namespace ast {

class ProceduralScope;
class Semantics;
class TypeExpression;

class Expression : public INode
{
public:
    Expression();
    ~Expression();

    // IIO
public:
    void io(IStream& stream) override;

public:
    virtual void resolveSymbols(Semantics& semantics) = 0;
};

class PrimaryExpression : public Expression
{
public:
    explicit PrimaryExpression(lexer::Token token);

    // IIO
public:
    void io(IStream& stream) override;

public:
    void resolveSymbols(Semantics& semantics);

public:
    lexer::Token token() const;

private:
    lexer::Token myToken;
};

class TupleExpression : public Expression
{
public:
    explicit TupleExpression(std::vector<std::unique_ptr<Expression>> expressions);

    TupleExpression(lexer::Token open,
                    lexer::Token close,
                    std::vector<std::unique_ptr<Expression>> expressions);

    // IIO
public:
    void io(IStream& stream) override;

public:
    void resolveSymbols(Semantics& semantics) override;

private:
    TupleKind myKind;
    std::vector<std::unique_ptr<Expression>> myExpressions;
};

class ApplyExpression : public Expression
{
public:
    explicit ApplyExpression(lexer::Token subject,
                             std::unique_ptr<TupleExpression> arguments);

    // IIO
public:
    void io(IStream& stream) override;

public:
    void resolveSymbols(Semantics& semantics) override;

private:
    lexer::Token mySubject;
    std::unique_ptr<TupleExpression> myArguments;
};

    } // namespace ast
} // namespace kyfoo
