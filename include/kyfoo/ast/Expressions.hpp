#pragma once

#include <kyfoo/Slice.hpp>
#include <kyfoo/ast/Node.hpp>
#include <kyfoo/ast/Tuples.hpp>

namespace kyfoo {
    
    class Diagnostics;
    class Error;

    namespace ast {

class IResolver;
class Expression;
class Declaration;
class ProcedureDeclaration;
class SymbolReference;

#define EXPRESSION_KINDS(X) \
    X(Primary   , PrimaryExpression) \
    X(Tuple     , TupleExpression) \
    X(Apply     , ApplyExpression) \
    X(Symbol    , SymbolExpression) \
    X(Constraint, ConstraintExpression)

class Context
{
public:
    Context(Diagnostics& dgn, IResolver& resolver);
    ~Context();

public:
    Error& error(lexer::Token const& token);
    std::size_t errorCount() const;

    Declaration const* lookup(SymbolReference const& sym) const;
    Declaration const* match(SymbolReference const& sym) const;
    Declaration const* matchProcedure(SymbolReference const& sym) const;

    void rewrite(std::unique_ptr<Expression> expr);

    void resolveExpression(std::unique_ptr<Expression>& expression);
    void resolveExpressions(std::vector<std::unique_ptr<Expression>>& expressions);

private:
    Diagnostics* myDiagnostics;
    IResolver* myResolver;
    std::unique_ptr<Expression> myRewrite;
};

class Expression : public INode
{
public:
    friend Context;

    enum class Kind
    {
#define X(a, b) a,
        EXPRESSION_KINDS(X)
#undef X
    };

    ~Expression();

protected:
    explicit Expression(Kind kind);
    Expression(Expression const& rhs);
    Expression& operator = (Expression const& rhs) = delete;

    void swap(Expression& rhs);

    // IIO
public:
    void io(IStream& stream) const override = 0;

public:
    virtual std::unique_ptr<Expression> clone() const = 0;

protected:
    virtual void resolveSymbols(Context& ctx) = 0;

public:
    Kind kind() const;

    template <typename T> T* as() = delete;
    template <typename T> T const* as() const = delete;

private:
    Kind myKind;
};

template <typename T>
class CloneableMixin : public Expression
{
public:
    using base_t = CloneableMixin<T>;

public:
    using Expression::Expression;

    std::unique_ptr<Expression> clone() const override
    {
        return std::unique_ptr<Expression>(new T(*static_cast<T const*>(this)));
    }
};

class PrimaryExpression : public CloneableMixin<PrimaryExpression>
{
public:
    explicit PrimaryExpression(lexer::Token const& token);
    PrimaryExpression(PrimaryExpression const& rhs);
    PrimaryExpression& operator = (PrimaryExpression const& rhs);
    ~PrimaryExpression();

    void swap(PrimaryExpression& rhs);

    // IIO
public:
    void io(IStream& stream) const override;

    // Expression
protected:
    void resolveSymbols(Context& ctx) override;

public:
    lexer::Token const& token() const;
    Declaration const* declaration() const;

private:
    lexer::Token myToken;
    Declaration const* myDeclaration = nullptr;
};

class TupleExpression : public CloneableMixin<TupleExpression>
{
public:
    TupleExpression(TupleKind kind,
                    std::vector<std::unique_ptr<Expression>>&& expressions);
    TupleExpression(TupleExpression const& rhs);
    TupleExpression& operator = (TupleExpression const& rhs);
    ~TupleExpression();

    void swap(TupleExpression& rhs);

    // IIO
public:
    void io(IStream& stream) const override;

    // Expression
protected:
    void resolveSymbols(Context& ctx) override;

public:
    TupleKind kind() const;
    Slice<Expression*> expressions() const;
    Slice<Expression*> expressions();

private:
    void flattenOpenTuples();

private:
    TupleKind myKind;
    std::vector<std::unique_ptr<Expression>> myExpressions;
};

class ApplyExpression : public CloneableMixin<ApplyExpression>
{
public:
    ApplyExpression(std::vector<std::unique_ptr<Expression>>&& expressions);
    ApplyExpression(ApplyExpression const& rhs);
    ApplyExpression& operator = (ApplyExpression const& rhs);
    ~ApplyExpression();

    void swap(ApplyExpression& rhs);

    // IIO
public:
    void io(IStream& stream) const override;

    // Expression
protected:
    void resolveSymbols(Context& ctx) override;

public:
    void flatten();

public:
    Slice<Expression*> expressions() const;

private:
    std::vector<std::unique_ptr<Expression>> myExpressions;
    ProcedureDeclaration* myDeclaration = nullptr;
};

class SymbolExpression : public CloneableMixin<SymbolExpression>
{
public:
    SymbolExpression(lexer::Token const& identifier,
                     std::vector<std::unique_ptr<Expression>>&& expressions);
    SymbolExpression(std::vector<std::unique_ptr<Expression>>&& expressions);
    SymbolExpression(SymbolExpression const& rhs);
    SymbolExpression& operator = (SymbolExpression const& rhs);
    ~SymbolExpression();

    void swap(SymbolExpression& rhs);

    // IIO
public:
    void io(IStream& stream) const override;

    // Expression
protected:
    void resolveSymbols(Context& ctx) override;

public:
    Slice<Expression*> expressions();
    Slice<Expression*> expressions() const;

    std::vector<std::unique_ptr<Expression>>& internalExpressions();

private:
    lexer::Token myIdentifier;
    std::vector<std::unique_ptr<Expression>> myExpressions;
    Declaration const* myDeclaration = nullptr; // todo: variant?
};

class ConstraintExpression : public CloneableMixin<ConstraintExpression>
{
public:
    ConstraintExpression(std::unique_ptr<Expression> subject,
                         std::unique_ptr<Expression> constraint);
    ConstraintExpression(ConstraintExpression const& rhs);
    ConstraintExpression& operator = (ConstraintExpression const& rhs);
    ~ConstraintExpression();

    void swap(ConstraintExpression& rhs);

    // IIO
public:
    void io(IStream& stream) const override;

    // Expression
protected:
    void resolveSymbols(Context& ctx) override;

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
