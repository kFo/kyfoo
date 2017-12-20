#pragma once

#include <kyfoo/Slice.hpp>
#include <kyfoo/ast/Node.hpp>
#include <kyfoo/ast/Tuples.hpp>

namespace kyfoo {
    
    class Diagnostics;
    class Error;

    namespace codegen {
        struct CustomData;
    }

    namespace ast {

class Context;
class IResolver;
class Expression;
class Declaration;
class ProcedureDeclaration;
class ProcedureScope;
class SymbolReference;
class LookupHit;
class VariableDeclaration;

#define EXPRESSION_KINDS(X)           \
    X(Primary  , PrimaryExpression  ) \
    X(Reference, ReferenceExpression) \
    X(Tuple    , TupleExpression    ) \
    X(Apply    , ApplyExpression    ) \
    X(Symbol   , SymbolExpression   ) \
    X(Dot      , DotExpression      ) \
    X(Var      , VarExpression      ) \
    X(Lambda   , LambdaExpression   )

class SymRes
{
public:
    enum Resolution
    {
        Success,
        Fail,
        NeedsSubstitution,
        Rewrite,
    };

private:
    Resolution myRes;

public:
    SymRes() : myRes(Success) {}
    /*implicit*/ SymRes(Resolution rhs) : myRes(rhs) {}

    explicit operator bool() const { return myRes == Success; }

    bool operator ==(SymRes const& rhs) const { return myRes == rhs.myRes; }
    bool operator !=(SymRes const& rhs) const { return !operator==(rhs); }

    SymRes& operator |=(SymRes const& rhs)
    {
        if ( rhs.myRes == Fail )
            myRes = Fail;
        else if ( myRes == Success && rhs.myRes == NeedsSubstitution )
            myRes = NeedsSubstitution;

        return *this;
    }
};

inline SymRes operator |(SymRes lhs, SymRes const& rhs)
{
    return lhs |= rhs;
}

class Expression : public INode
{
public:
    friend class Context;
    friend class DotExpression;

    enum class Kind
    {
#define X(a, b) a,
        EXPRESSION_KINDS(X)
#undef X
    };

protected:
    explicit Expression(Kind kind);
    Expression(Kind kind, Declaration const* decl);
    Expression(Expression const& rhs);
    Expression& operator = (Expression const& rhs) = delete;

public:
    ~Expression();

protected:
    void swap(Expression& rhs);

    // IIO
public:
    void io(IStream& stream) const override = 0;

public:
    virtual Expression* clone(clone_map_t& map) const = 0;
    virtual void cloneChildren(Expression& c, clone_map_t& map) const;
    virtual void remapReferences(clone_map_t const& map);

protected:
    virtual SymRes resolveSymbols(Context& ctx) = 0;

public:
    void addConstraint(std::unique_ptr<Expression> expr);

public:
    Kind kind() const;
    Declaration const* declaration() const;
    void setDeclaration(Declaration const& decl);
    void clearDeclaration();

    Slice<Expression*> constraints();
    const Slice<Expression*> constraints() const;

    template <typename T> T* as() = delete;
    template <typename T> T const* as() const = delete;

private:
    Kind myKind;

protected:
    std::vector<std::unique_ptr<Expression>> myConstraints;
    Declaration const* myDeclaration = nullptr;
};

class PrimaryExpression : public Expression
{
public:
    explicit PrimaryExpression(lexer::Token const& token);

protected:
    PrimaryExpression(Kind kind, lexer::Token const& token);

    PrimaryExpression(PrimaryExpression const& rhs);
    PrimaryExpression& operator = (PrimaryExpression const& rhs);

public:
    ~PrimaryExpression();

    void swap(PrimaryExpression& rhs);

    // IIO
public:
    void io(IStream& stream) const override;

    // Expression
    DECL_CLONE_ALL(Expression)
protected:
    SymRes resolveSymbols(Context& ctx) override;

public:
    lexer::Token const& token() const;

    void setMetaVariable(Declaration const* decl);

private:
    lexer::Token myToken;
};

class ReferenceExpression : public Expression
{
public:
    explicit ReferenceExpression(std::unique_ptr<Expression> expression);

protected:
    ReferenceExpression(ReferenceExpression const& rhs);
    ReferenceExpression& operator = (ReferenceExpression const& rhs);

public:
    ~ReferenceExpression();

    void swap(ReferenceExpression& rhs);

    // IIO
public:
    void io(IStream& stream) const override;

    // Expression
    DECL_CLONE_ALL(Expression)
protected:
    SymRes resolveSymbols(Context& ctx) override;

public:
    Expression const& expression() const;
    Expression& expression();

private:
    std::unique_ptr<Expression> myExpression;
};

class TupleExpression : public Expression
{
public:
    friend class ApplyExpression;

public:
    TupleExpression(TupleKind kind,
                    std::vector<std::unique_ptr<Expression>>&& expressions);
    TupleExpression(lexer::Token const& open,
                    lexer::Token const& close,
                    std::vector<std::unique_ptr<Expression>>&& expressions);

protected:
    TupleExpression(TupleExpression const& rhs);
    TupleExpression& operator = (TupleExpression const& rhs);

public:
    ~TupleExpression();

    void swap(TupleExpression& rhs);

    // IIO
public:
    void io(IStream& stream) const override;

    // Expression
    DECL_CLONE_ALL(Expression)
protected:
    SymRes resolveSymbols(Context& ctx) override;

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

class ApplyExpression : public Expression
{
public:
    ApplyExpression(std::vector<std::unique_ptr<Expression>>&& expressions);

protected:
    ApplyExpression(ApplyExpression const& rhs);
    ApplyExpression& operator = (ApplyExpression const& rhs);

public:
    ~ApplyExpression();

    void swap(ApplyExpression& rhs);

    // IIO
public:
    void io(IStream& stream) const override;

    // Expression
    DECL_CLONE_ALL(Expression)
protected:
    SymRes resolveSymbols(Context& ctx) override;

public:
    void flatten();
    void flatten(std::vector<std::unique_ptr<Expression>>::iterator first);

public:
    Slice<Expression*> expressions() const;

    ProcedureDeclaration const* declaration() const;

private:
    // AST state
    std::vector<std::unique_ptr<Expression>> myExpressions;
};

class SymbolExpression : public Expression
{
public:
    SymbolExpression(lexer::Token const& identifier,
                     std::vector<std::unique_ptr<Expression>>&& expressions);
    SymbolExpression(std::vector<std::unique_ptr<Expression>>&& expressions);
    SymbolExpression(lexer::Token const& open,
                     lexer::Token const& close,
                     std::vector<std::unique_ptr<Expression>>&& expressions);

protected:
    SymbolExpression(SymbolExpression const& rhs);
    SymbolExpression& operator = (SymbolExpression const& rhs);

public:
    ~SymbolExpression();

    void swap(SymbolExpression& rhs);

    // IIO
public:
    void io(IStream& stream) const override;

    // Expression
    DECL_CLONE_ALL(Expression)
protected:
    SymRes resolveSymbols(Context& ctx) override;

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

class DotExpression : public Expression
{
public:
    DotExpression(bool global,
                  std::vector<std::unique_ptr<Expression>>&& exprs);

protected:
    DotExpression(DotExpression const& rhs);
    DotExpression& operator = (DotExpression const& rhs);

public:
    ~DotExpression();

    void swap(DotExpression& rhs);

    // IIO
public:
    void io(IStream& stream) const override;

    // Expression
    DECL_CLONE_ALL(Expression)
protected:
    SymRes resolveSymbols(Context& ctx) override;

public:
    Slice<Expression*> expressions();
    Slice<Expression*> expressions() const;

    Expression& top();
    Expression const& top() const;

    bool isModuleScope() const;

private:
    std::vector<std::unique_ptr<Expression>> myExpressions;
    bool myGlobal = false;
};

class VarExpression : public Expression
{
public:
    VarExpression(std::unique_ptr<PrimaryExpression> id,
                  std::unique_ptr<Expression> expression);
    VarExpression(VariableDeclaration const& var,
                  std::unique_ptr<Expression> expression);

protected:
    VarExpression(VarExpression const& rhs);
    VarExpression& operator = (VarExpression const& rhs);

public:
    ~VarExpression();

    void swap(VarExpression& rhs);

    // IIO
public:
    void io(IStream& stream) const override;

    // Expression
    DECL_CLONE_ALL(Expression)
protected:
    SymRes resolveSymbols(Context& ctx) override;

public:
    PrimaryExpression const& identity() const;
    PrimaryExpression& identity();
    Expression const& expression() const;
    Expression& expression();

private:
    std::unique_ptr<PrimaryExpression> myIdentity;
    std::unique_ptr<Expression> myExpression;
};

class LambdaExpression : public Expression
{
public:
    LambdaExpression(std::unique_ptr<Expression> params,
                     std::unique_ptr<Expression> returnType,
                     std::unique_ptr<Expression> body);

protected:
    LambdaExpression(LambdaExpression const& rhs);
    LambdaExpression& operator = (LambdaExpression const& rhs);

public:
    ~LambdaExpression();

    void swap(LambdaExpression& rhs);

    // IIO
public:
    void io(IStream& stream) const override;

    // Expression
    DECL_CLONE_ALL(Expression)
protected:
    SymRes resolveSymbols(Context& ctx) override;

public:
    Expression const& parameters() const;
    Expression const& returnType() const;
    Expression const& body() const;

    Expression& parameters();
    Expression& returnType();
    Expression& body();

private:
    std::unique_ptr<Expression> myParams;
    std::unique_ptr<Expression> myReturnType;
    std::unique_ptr<Expression> myBody;
};

#define X(a, b) template <> inline b* Expression::as<b>() { return myKind == Expression::Kind::a ? static_cast<b*>(this) : nullptr; }
EXPRESSION_KINDS(X)
#undef X

#define X(a, b) template <> inline b const* Expression::as<b>() const { return myKind == Expression::Kind::a ? static_cast<b const*>(this) : nullptr; }
EXPRESSION_KINDS(X)
#undef X

bool allResolved(Slice<Expression*> const& exprs);

    } // namespace ast
} // namespace kyfoo
