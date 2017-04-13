#pragma once

#include <memory>

#include <kyfoo/lexer/Scanner.hpp>
#include <kyfoo/lexer/Token.hpp>
#include <kyfoo/ast/Node.hpp>
#include <kyfoo/ast/Tuples.hpp>
#include <kyfoo/ast/TypeExpressions.hpp>

namespace kyfoo {
    namespace ast {

class ProceduralScope;
class Semantics;
class TypeExpression;

class ValueExpression : public INode
{
public:
    ValueExpression();
    ~ValueExpression();

    // IIO
public:
    void io(IStream& stream) override;

public:
    virtual void resolveSymbols(Diagnostics& dgn) override = 0;
};

class PrimaryExpression : public ValueExpression
{
public:
    explicit PrimaryExpression(lexer::Token const& token);

    // IIO
public:
    void io(IStream& stream) override;

public:
    void resolveSymbols(Diagnostics& dgn) override;

public:
    lexer::Token token() const;

private:
    lexer::Token myToken;
};

class TupleExpression : public ValueExpression
{
public:
    explicit TupleExpression(std::vector<std::unique_ptr<ValueExpression>> expressions);

    TupleExpression(lexer::Token open,
                    lexer::Token close,
                    std::vector<std::unique_ptr<ValueExpression>> expressions);

    // IIO
public:
    void io(IStream& stream) override;

public:
    void resolveSymbols(Diagnostics& dgn) override;

private:
    TupleKind myKind;
    std::vector<std::unique_ptr<ValueExpression>> myExpressions;
};

class ApplyExpression : public ValueExpression
{
public:
    explicit ApplyExpression(lexer::Token subject,
                             std::unique_ptr<TupleExpression> arguments);

    // IIO
public:
    void io(IStream& stream) override;

public:
    void resolveSymbols(Diagnostics& dgn) override;

private:
    lexer::Token mySubject;
    std::unique_ptr<TupleExpression> myArguments;
};

    } // namespace ast
} // namespace kyfoo
