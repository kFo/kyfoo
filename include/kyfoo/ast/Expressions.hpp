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
class LookupHit;

#define EXPRESSION_KINDS(X) \
    X(Primary   , PrimaryExpression) \
    X(Tuple     , TupleExpression) \
    X(Apply     , ApplyExpression) \
    X(Symbol    , SymbolExpression) \
    X(Constraint, ConstraintExpression)

class Expression : public INode
{
public:
    friend class Context;

    enum class Kind
    {
#define X(a, b) a,
        EXPRESSION_KINDS(X)
#undef X
    };

    ~Expression();

protected:
    explicit Expression(Kind kind);
    Expression(Kind kind, Declaration const* decl);
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
    Declaration const* declaration() const;

    template <typename T> T* as() = delete;
    template <typename T> T const* as() const = delete;

private:
    Kind myKind;

protected:
    // Semantic state
    Declaration const* myDeclaration = nullptr;
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

    void setFreeVariable(Declaration const* decl);

private:
    lexer::Token myToken;
};

class TupleExpression : public CloneableMixin<TupleExpression>
{
public:
    TupleExpression(TupleKind kind,
                    std::vector<std::unique_ptr<Expression>>&& expressions);
    TupleExpression(lexer::Token const& open,
                    lexer::Token const& close,
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
    lexer::Token const& openToken() const;
    lexer::Token const& closeToken() const;

    Slice<Expression*> expressions() const;
    Slice<Expression*> expressions();

private:
    void flattenOpenTuples();

private:
    // AST state
    TupleKind myKind;
    std::vector<std::unique_ptr<Expression>> myExpressions;

    lexer::Token myOpenToken;
    lexer::Token myCloseToken;
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

    ProcedureDeclaration const* declaration() const;

private:
    // AST state
    std::vector<std::unique_ptr<Expression>> myExpressions;
};

class SymbolExpression : public CloneableMixin<SymbolExpression>
{
public:
    SymbolExpression(lexer::Token const& identifier,
                     std::vector<std::unique_ptr<Expression>>&& expressions);
    SymbolExpression(std::vector<std::unique_ptr<Expression>>&& expressions);
    SymbolExpression(lexer::Token const& open,
                     lexer::Token const& close,
                     std::vector<std::unique_ptr<Expression>>&& expressions);
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
    lexer::Token const& identifier() const;
    Slice<Expression*> expressions();
    Slice<Expression*> expressions() const;

    lexer::Token const& openToken() const;
    lexer::Token const& closeToken() const;

    std::vector<std::unique_ptr<Expression>>& internalExpressions();

private:
    // AST state
    lexer::Token myIdentifier;
    std::vector<std::unique_ptr<Expression>> myExpressions;

    lexer::Token myOpenToken;
    lexer::Token myCloseToken;
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
    Expression* subject();
    Expression const* subject() const;

    Expression* constraint();
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

lexer::Token const& front(Expression const& expr);
std::ostream& print(std::ostream& stream, Expression const& expr);
void enforceResolution(Context& ctx, Expression const& expr);

    } // namespace ast
} // namespace kyfoo
