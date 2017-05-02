#pragma once

#include <kyfoo/ast/Node.hpp>
#include <kyfoo/ast/Tuples.hpp>

namespace kyfoo {
    
    class Diagnostics;

    namespace ast {

class Resolver;

class Expression : public INode
{
    // IIO
public:
    void io(IStream& stream) const override = 0;

public:
    virtual void resolveSymbols(Diagnostics& dgn, Resolver& resolver) = 0;
};

class PrimaryExpression : public Expression
{
public:
    explicit PrimaryExpression(lexer::Token const& token);
    ~PrimaryExpression();

    // IIO
public:
    void io(IStream& stream) const override;

public:
    void resolveSymbols(Diagnostics& dgn, Resolver& resolver) override;

private:
    lexer::Token myToken;
};

class TupleExpression : public Expression
{
public:
    TupleExpression(TupleKind kind,
                    std::vector<std::unique_ptr<Expression>>&& expressions);
    ~TupleExpression();

    // IIO
public:
    void io(IStream& stream) const override;

public:
    void resolveSymbols(Diagnostics& dgn, Resolver& resolver) override;

public:
    TupleKind kind() const;
    std::vector<std::unique_ptr<Expression>> const& expressions() const;
    std::vector<std::unique_ptr<Expression>>& expressions();

private:
    TupleKind myKind;
    std::vector<std::unique_ptr<Expression>> myExpressions;
};

class ConstraintExpression : public Expression
{
public:
    ConstraintExpression(std::unique_ptr<Expression> subject,
                        std::unique_ptr<Expression> constraint);
    ~ConstraintExpression();

    // IIO
public:
    void io(IStream& stream) const override;

public:
    void resolveSymbols(Diagnostics& dgn, IResolver& resolver) override;

public:
    Expression* subject();
    Expression* constraint();

private:
    std::unique_ptr<Expression> mySubject;
    std::unique_ptr<Expression> myConstraint;
};

    } // namespace ast
} // namespace kyfoo
