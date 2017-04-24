#pragma once

#include <memory>

#include <kyfoo/lexer/Scanner.hpp>
#include <kyfoo/lexer/Token.hpp>
#include <kyfoo/ast/Node.hpp>
#include <kyfoo/ast/Tuples.hpp>
#include <kyfoo/ast/TypeExpressions.hpp>

namespace kyfoo {
    class Diagnostics;

    namespace ast {

class ProceduralScope;
class TypeExpression;
class Resolver;

class ValueExpression : public INode
{
public:
    ValueExpression();
    ~ValueExpression();

    // IIO
public:
    void io(IStream& stream) const override;

public:
    virtual void resolveSymbols(Diagnostics& dgn, Resolver& resolver) = 0;
};

class PrimaryExpression : public ValueExpression
{
public:
    explicit PrimaryExpression(lexer::Token const& token);

    // IIO
public:
    void io(IStream& stream) const override;

    // ValueExpression
public:
    void resolveSymbols(Diagnostics& dgn, Resolver& resolver) override;

public:
    lexer::Token token() const;

private:
    lexer::Token myToken;
};

class TupleExpression : public ValueExpression
{
public:
    explicit TupleExpression(std::vector<std::unique_ptr<ValueExpression>> expressions);

    TupleExpression(lexer::Token const& open,
                    lexer::Token const& close,
                    std::vector<std::unique_ptr<ValueExpression>> expressions);

    TupleExpression(TupleKind kind, std::vector<std::unique_ptr<ValueExpression>> expressions);

    // IIO
public:
    void io(IStream& stream) const override;

    // ValueExpression
public:
    void resolveSymbols(Diagnostics& dgn, Resolver& resolver) override;

private:
    TupleKind myKind;
    std::vector<std::unique_ptr<ValueExpression>> myExpressions;
};

class ApplyExpression : public ValueExpression
{
public:
    ApplyExpression(std::unique_ptr<ValueExpression> subject,
                    std::unique_ptr<TupleExpression> arguments);

    // IIO
public:
    void io(IStream& stream) const override;

    // ValueExpression
public:
    void resolveSymbols(Diagnostics& dgn, Resolver& resolver) override;

private:
    std::unique_ptr<ValueExpression> mySubject;
    std::unique_ptr<TupleExpression> myArguments;
};

    } // namespace ast
} // namespace kyfoo
