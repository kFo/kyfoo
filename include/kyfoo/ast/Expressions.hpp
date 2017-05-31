#pragma once

#include <kyfoo/ast/Node.hpp>
#include <kyfoo/ast/Tuples.hpp>

namespace kyfoo {
    
    class Diagnostics;

    namespace ast {

class IResolver;
class Expression;
class Declaration;

#define EXPRESSION_KINDS(X) \
    X(Primary   , PrimaryExpression) \
    X(Tuple     , TupleExpression) \
    X(Constraint, ConstraintExpression)

class Expression : public INode
{
public:
    enum class Kind
    {
#define X(a, b) a,
        EXPRESSION_KINDS(X)
#undef X
    };

protected:
    explicit Expression(Kind kind);

    // IIO
public:
    void io(IStream& stream) const override = 0;

public:
    virtual void resolveSymbols(Diagnostics& dgn, IResolver& resolver) = 0;

public:
    Kind kind() const;

    template <typename T> T* as() = delete;
    template <typename T> T const* as() const = delete;

private:
    Kind myKind;
};

class PrimaryExpression : public Expression
{
public:
    explicit PrimaryExpression(lexer::Token const& token);
    ~PrimaryExpression();

    // IIO
public:
    void io(IStream& stream) const override;

    // Expression
public:
    void resolveSymbols(Diagnostics& dgn, IResolver& resolver) override;

public:
    lexer::Token const& token() const;
    Declaration const* declaration() const;

private:
    lexer::Token myToken;
    Declaration const* myDeclaration = nullptr;
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
    void resolveSymbols(Diagnostics& dgn, IResolver& resolver) override;

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
    Expression const* subject() const;
    Expression const* constraint() const;

private:
    std::unique_ptr<Expression> mySubject;
    std::unique_ptr<Expression> myConstraint;
};

#define X(a, b) template <> inline b* Expression::as<b>() { return myKind == Expression::Kind::a ? static_cast<b*>(this) : nullptr; }
EXPRESSION_KINDS(X)
#undef X

#define X(a, b) template <> inline b const* Expression::as<b>() const { return myKind == Expression::Kind::a ? static_cast<b const*>(this) : nullptr; }
EXPRESSION_KINDS(X)
#undef X

    } // namespace ast
} // namespace kyfoo
