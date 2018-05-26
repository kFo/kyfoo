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
class Resolver;
class Expression;
class Declaration;
class LookupHit;
class ProcedureDeclaration;
class ProcedureScope;
class SymbolReference;
class TupleExpression;
class Type;
class UniverseExpression;
class VariableDeclaration;

#define EXPRESSION_KINDS(X)             \
    X(Literal   , LiteralExpression   ) \
    X(Identifier, IdentifierExpression) \
    X(Tuple     , TupleExpression     ) \
    X(Apply     , ApplyExpression     ) \
    X(Symbol    , SymbolExpression    ) \
    X(Dot       , DotExpression       ) \
    X(Assign    , AssignExpression    ) \
    X(Lambda    , LambdaExpression    ) \
    X(Arrow     , ArrowExpression     ) \
    X(Universe  , UniverseExpression  )

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
        else if ( rhs.myRes == Rewrite )
            myRes = Rewrite;
        else if ( myRes == Success && rhs.myRes == NeedsSubstitution )
            myRes = NeedsSubstitution;

        return *this;
    }
};

inline SymRes operator |(SymRes lhs, SymRes const& rhs)
{
    return lhs |= rhs;
}

class Strata
{
public:
    Strata();
    ~Strata();

    Strata(Strata const&) = delete;
    Strata(Strata&&) = delete;

public:
    Expression const* getType(Expression const& expr);
    UniverseExpression const& universe(std::size_t level);
    TupleExpression const& tuple(Slice<Expression const*> exprs);

private:
    std::vector<std::unique_ptr<UniverseExpression>> myUniverses;
    std::vector<std::unique_ptr<TupleExpression>> myTuples;
};

class Expression : public INode
{
public:
    friend class Context;
    friend class DotExpression;
    friend std::vector<std::unique_ptr<Expression>> flattenConstraints(std::unique_ptr<Expression> expr);

    enum class Kind
    {
#define X(a, b) a,
        EXPRESSION_KINDS(X)
#undef X
    };

    static UniverseExpression const& universe(std::size_t level);
    static TupleExpression const& tuple(Slice<Expression const*> exprs);

protected:
    explicit Expression(Kind kind);
    Expression(Kind kind, Expression const* type);
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
    void addConstraints(std::vector<std::unique_ptr<Expression>>&& exprs);

public:
    Kind kind() const;
    Expression const* type() const;
    void setType(Expression const* type);
    void setType(std::unique_ptr<Expression> type);
    void setType(Declaration const& decl);
    void clearType();

    Slice<Expression*> constraints();
    Slice<Expression const*> constraints() const;

    std::vector<std::unique_ptr<Expression>>&& takeConstraints();

    template <typename T> T* as() = delete;
    template <typename T> T const* as() const = delete;

private:
    Kind myKind;

protected:
    std::vector<std::unique_ptr<Expression>> myConstraints;

    static Strata g_strata;
    mutable Expression const* myType = nullptr;
};

class LiteralExpression : public Expression
{
public:
    explicit LiteralExpression(lexer::Token const& token);

protected:
    LiteralExpression(Kind kind, lexer::Token const& token);

    LiteralExpression(LiteralExpression const& rhs);
    LiteralExpression& operator = (LiteralExpression const& rhs);

public:
    ~LiteralExpression();

    void swap(LiteralExpression& rhs);

    // IIO
public:
    void io(IStream& stream) const override;

    // Expression
    DECL_CLONE_ALL(Expression)
protected:
    SymRes resolveSymbols(Context& ctx) override;

public:
    lexer::Token const& token() const;

private:
    lexer::Token myToken;
};

class IdentifierExpression : public Expression
{
public:
    explicit IdentifierExpression(lexer::Token const& token);
    IdentifierExpression(lexer::Token const& token, Declaration const& decl);

protected:
    IdentifierExpression(Kind kind,
                         lexer::Token const& token,
                         Declaration const* decl);

    IdentifierExpression(IdentifierExpression const& rhs);
    IdentifierExpression& operator = (IdentifierExpression const& rhs);

public:
    ~IdentifierExpression();

    void swap(IdentifierExpression& rhs);

    // IIO
public:
    void io(IStream& stream) const override;

    // Expression
    DECL_CLONE_ALL(Expression)
protected:
    SymRes resolveSymbols(Context& ctx) override;

public:
    lexer::Token const& token() const;
    Declaration const* declaration() const;

    SymRes tryLowerTemplateToProc(Context& ctx);

    void setDeclaration(Declaration const& decl);
    void clearDeclaration();

protected:
    // todo: removeme
    void setToken(lexer::Token const& token);

private:
    lexer::Token myToken;
    Declaration const* myDeclaration = nullptr;
};

class ExpressionArray
{
public:
    struct iterator_end
    {
        std::size_t i;
    };

    class iterator
    {
        Slice<Expression const*> const* s;
        std::size_t ei = 0;
        std::size_t ai = 0;

    public:
        explicit iterator(Slice<Expression const*> const& s)
            : s(&s)
        {
        }

    public:
        void operator++()
        {
            ++ei;
            if ( ei == s->size() ) {
                ei = 0;
                ++ai;
            }
        }

        Expression const* operator*() const { return (*s)[ei]; }

        bool operator==(iterator_end rhs)
        {
            return ai == rhs.i;
        }

        bool operator!=(iterator_end rhs)
        {
            return !operator==(rhs);
        }
    };

public:
    ExpressionArray(Slice<Expression const*> exprs, std::size_t n)
        : exprs(exprs)
        , n(exprs.empty() ? 0 : n)
    {
    }

    /*implicit*/ ExpressionArray(Slice<Expression const*> exprs)
        : ExpressionArray(exprs, 1)
    {
    }

public:
    iterator begin() const { return iterator(exprs); }
    iterator_end end() const { return iterator_end{n}; }

    std::size_t size() const { return exprs.size() * n; }
    bool empty() const { return size() == 0; }

    Expression const* front() const { return exprs.front(); }
    Expression const* back() const { return exprs.back(); }

    Expression const* operator[](std::size_t i) const { return exprs[i % exprs.size()]; }

private:
    Slice<Expression const*> exprs;
    std::size_t n = 1;
};

inline ExpressionArray::iterator begin(ExpressionArray const& rhs) { return rhs.begin(); }
inline ExpressionArray::iterator_end end(ExpressionArray const& rhs) { return rhs.end(); }

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

    TupleExpression(std::vector<std::unique_ptr<Expression>>&& expressions,
                    std::unique_ptr<Expression> cardExpression);

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

    Slice<Expression*> expressions();
    Slice<Expression const*> expressions() const;

    ExpressionArray elements() const;
    std::size_t elementsCount() const;

private:
    void flattenOpenTuples();

private:
    // AST state
    TupleKind myKind;
    std::vector<std::unique_ptr<Expression>> myExpressions;
    std::unique_ptr<Expression> myCardExpression;
    std::size_t myCard = 0;

    lexer::Token myOpenToken;
    lexer::Token myCloseToken;
};

class ApplyExpression : public Expression
{
public:
    friend class ProcedureDeclaration;

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
    SymRes lowerToApplicable(Context& ctx);
    SymRes lowerToStaticCall(Context& ctx);
    SymRes lowerToConstruction(Context& ctx);
    SymRes elaborateTuple(Context& ctx);
    void flatten();
    void flatten(std::vector<std::unique_ptr<Expression>>::iterator first);

public:
    Slice<Expression*> expressions();
    Slice<Expression const*> expressions() const;

    Expression* subject();
    Expression const* subject() const;

    Slice<Expression*> arguments();
    Slice<Expression const*> arguments() const;

    ProcedureDeclaration const* procedure() const;

private:
    // AST state
    std::vector<std::unique_ptr<Expression>> myExpressions;
    ProcedureDeclaration const* myProc = nullptr;
};

// todo: removeme
class SymbolExpression : public IdentifierExpression
{
public:
    SymbolExpression(lexer::Token const& token,
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
    Slice<Expression*> expressions();
    Slice<Expression const*> expressions() const;

    lexer::Token const& openToken() const;
    lexer::Token const& closeToken() const;

    std::vector<std::unique_ptr<Expression>>& internalExpressions();

private:
    // AST state
    std::vector<std::unique_ptr<Expression>> myExpressions;

    lexer::Token myOpenToken;
    lexer::Token myCloseToken;
};

class DotExpression : public Expression
{
public:
    friend ApplyExpression;

public:
    DotExpression(bool modScope,
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
    Slice<Expression const*> expressions() const;

    Expression const* top(std::size_t index = 0) const;

    bool isModuleScope() const;

protected:
    std::unique_ptr<Expression> takeTop(std::size_t index = 0);

private:
    std::vector<std::unique_ptr<Expression>> myExpressions;
    bool myModScope = false;
};

class AssignExpression : public Expression
{
public:
    AssignExpression(std::unique_ptr<Expression> lhs,
                     std::unique_ptr<Expression> rhs);
    AssignExpression(VariableDeclaration const& var,
                     std::unique_ptr<Expression> expression);

protected:
    AssignExpression(AssignExpression const& rhs);
    AssignExpression& operator = (AssignExpression const& rhs);

public:
    ~AssignExpression();

    void swap(AssignExpression& rhs);

    // IIO
public:
    void io(IStream& stream) const override;

    // Expression
    DECL_CLONE_ALL(Expression)
protected:
    SymRes resolveSymbols(Context& ctx) override;

public:
    Expression const& left() const;
    Expression& left();
    Expression const& right() const;
    Expression& right();

private:
    std::unique_ptr<Expression> myLeft;
    std::unique_ptr<Expression> myRight;
};

class LambdaExpression : public Expression
{
public:
    LambdaExpression(lexer::Token const& yieldToken,
                     ProcedureDeclaration* proc);

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
    lexer::Token const& yieldToken() const;

    ProcedureDeclaration const& procedure() const;
    ProcedureDeclaration& procedure();

private:
    lexer::Token myYieldToken;
    ProcedureDeclaration* myProc = nullptr;
};

class ArrowExpression : public Expression
{
public:
    ArrowExpression(std::unique_ptr<Expression> from,
                    std::unique_ptr<Expression> to);

protected:
    ArrowExpression(ArrowExpression const& rhs);
    ArrowExpression& operator = (ArrowExpression const& rhs);

public:
    ~ArrowExpression();

    void swap(ArrowExpression& rhs);

    // IIO
public:
    void io(IStream& stream) const override;

    // Expression
    DECL_CLONE_ALL(Expression)
protected:
    SymRes resolveSymbols(Context& ctx) override;

public:
    Expression const& from() const;
    Expression& from();

    Slice<Expression const*> sliceFrom() const;

    Expression const& to() const;
    Expression& to();

    Slice<Expression const*> sliceTo() const;

private:
    std::unique_ptr<Expression> myFrom;
    std::unique_ptr<Expression> myTo;
};

class UniverseExpression : public Expression
{
public:
    friend class Strata;
    using natural_t = std::size_t; // todo

private:
    explicit UniverseExpression(natural_t level);

protected:
    UniverseExpression(UniverseExpression const& rhs);
    UniverseExpression& operator = (UniverseExpression const& rhs);

public:
    ~UniverseExpression();

    void swap(UniverseExpression& rhs);

    // IIO
public:
    void io(IStream& stream) const override;

    // Expression
    DECL_CLONE_ALL(Expression)
protected:
    SymRes resolveSymbols(Context& ctx) override;

public:
    natural_t level() const;

private:
    natural_t myLevel = 0;
};

#define X(a, b) template <> inline b* Expression::as<b>() { return myKind == Expression::Kind::a ? static_cast<b*>(this) : nullptr; }
EXPRESSION_KINDS(X)
#undef X

#define X(a, b) template <> inline b const* Expression::as<b>() const { return myKind == Expression::Kind::a ? static_cast<b const*>(this) : nullptr; }
EXPRESSION_KINDS(X)
#undef X

std::ostream& operator << (std::ostream& stream, Expression const& expr);
std::ostream& operator << (std::ostream& stream, Slice<Expression const*> exprs);
std::ostream& operator << (std::ostream& stream, Slice<Expression*> exprs);

struct get_types {
    Slice<Expression const*> exprs;

    explicit get_types(Slice<Expression const*> rhs) : exprs(rhs) {}
    explicit get_types(Slice<Expression*> rhs) : exprs(rhs) {}
};

std::ostream& operator << (std::ostream& stream, get_types&& types);

Expression const* createInferredType(Expression& expr, Declaration const& decl);
IdentifierExpression* identify(Expression& expr);
IdentifierExpression const* identify(Expression const& expr);
bool hasDeclaration(Expression const& expr);
Declaration const* getDeclaration(Expression const& expr);
Declaration const* getDeclaration(Expression const* expr);
std::vector<std::unique_ptr<Expression>> flattenConstraints(std::unique_ptr<Expression> expr);
std::tuple<Declaration const*, Expression const*> getRef(Expression const& expr);
Expression const* getRefType(Expression const& expr);
ProcedureDeclaration const* getProcedure(Expression const& expr);

    } // namespace ast
} // namespace kyfoo
