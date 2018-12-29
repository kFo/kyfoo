#pragma once

#include <optional>

#include <kyfoo/Dollar.hpp>
#include <kyfoo/Slice.hpp>
#include <kyfoo/Utilities.hpp>

#include <kyfoo/lexer/Token.hpp>

#include <kyfoo/ast/Clone.hpp>
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
class Lookup;
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
    X(Symbol    , SymbolExpression    ) \
    X(Dot       , DotExpression       ) \
    X(Apply     , ApplyExpression     ) \
    X(Lambda    , LambdaExpression    ) \
    X(Assign    , AssignExpression    ) \
    X(Tuple     , TupleExpression     ) \
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
    Resolution myRes = Success;

public:
    SymRes() = default;
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

    bool error() const
    {
        return myRes == Fail;
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
    UniverseExpression const& universe(uz level);
    TupleExpression const& tuple(Slice<Expression const*> exprs);

private:
    std::vector<Box<UniverseExpression>> myUniverses;
    std::vector<Box<TupleExpression>> myTuples;
};

class Expression
{
public:
    friend class Context;
    friend class DotExpression;
    friend std::vector<Box<Expression>> flattenConstraints(Box<Expression> expr);

    enum class Kind
    {
#define X(a, b) a,
        EXPRESSION_KINDS(X)
#undef X
    };

    static UniverseExpression const& universe(uz level);
    static TupleExpression const& tuple(Slice<Expression const*> exprs);

protected:
    explicit Expression(Kind kind);
    Expression(Kind kind, Expression const* type);
    Expression(Expression const& rhs);
    Expression& operator = (Expression const& rhs) = delete;

public:
    KYFOO_DEBUG_VIRTUAL ~Expression();

protected:
    void swap(Expression& rhs) noexcept;

public:
    DECL_CLONE_ALL_NOBASE(Expression);

public:
    void addConstraint(Box<Expression> expr);
    void addConstraints(std::vector<Box<Expression>>&& exprs);

public:
    Kind kind() const;
    Expression const* type() const;
    void setType(Expression const* type);
    void setType(Box<Expression> type);
    void setType(Declaration const& decl);
    void clearType();

    Slice<Expression*> constraints();
    Slice<Expression const*> constraints() const;

    std::vector<Box<Expression>>&& takeConstraints();

    template <typename T> T* as() = delete;
    template <typename T> T const* as() const = delete;

private:
    Kind myKind;

protected:
    std::vector<Box<Expression>> myConstraints;

    static Strata g_strata;
    mutable Expression const* myType = nullptr;
};

class LiteralExpression : public Expression
{
public:
    friend class Context;

public:
    explicit LiteralExpression(lexer::Token token);

protected:
    LiteralExpression(Kind kind, lexer::Token token);

    LiteralExpression(LiteralExpression const& rhs);
    LiteralExpression& operator = (LiteralExpression const& rhs);

public:
    ~LiteralExpression() KYFOO_DEBUG_OVERRIDE;

    void swap(LiteralExpression& rhs) noexcept;

    // Expression
    DECL_CLONE_ALL(Expression)
protected:
    SymRes resolveSymbols(Context& ctx);

public:
    lexer::Token const& token() const;

private:
    lexer::Token myToken;
};

class IdentifierExpression : public Expression
{
public:
    friend class Context;

public:
    explicit IdentifierExpression(lexer::Token token);
    IdentifierExpression(lexer::Token token, Declaration const& decl);

protected:
    IdentifierExpression(Kind kind,
                         lexer::Token token,
                         Declaration const* decl);

    IdentifierExpression(IdentifierExpression const& rhs);
    IdentifierExpression& operator = (IdentifierExpression const& rhs);

public:
    ~IdentifierExpression() KYFOO_DEBUG_OVERRIDE;

    void swap(IdentifierExpression& rhs) noexcept;

    // Expression
    DECL_CLONE_ALL(Expression)
protected:
    SymRes resolveSymbols(Context& ctx);

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

// todo: removeme
class SymbolExpression : public IdentifierExpression
{
public:
    friend class Context;

public:
    SymbolExpression(lexer::Token token,
                     std::vector<Box<Expression>>&& expressions);
    SymbolExpression(std::vector<Box<Expression>>&& expressions);
    SymbolExpression(lexer::Token open,
                     lexer::Token close,
                     std::vector<Box<Expression>>&& expressions);

protected:
    SymbolExpression(SymbolExpression const& rhs);
    SymbolExpression& operator = (SymbolExpression const& rhs);

public:
    ~SymbolExpression() KYFOO_DEBUG_OVERRIDE;

    void swap(SymbolExpression& rhs) noexcept;

    // Expression
    DECL_CLONE_ALL(Expression)
protected:
    SymRes resolveSymbols(Context& ctx);

public:
    SymRes resolveSubExpressions(Context& ctx);

    Slice<Expression*> expressions();
    Slice<Expression const*> expressions() const;

    lexer::Token const& openToken() const;
    lexer::Token const& closeToken() const;

    std::vector<Box<Expression>>& internalExpressions();

private:
    // AST state
    std::vector<Box<Expression>> myExpressions;

    lexer::Token myOpenToken;
    lexer::Token myCloseToken;
};

class DotExpression : public Expression
{
public:
    friend class Context;
    friend class ApplyExpression;

public:
    DotExpression(bool modScope,
                  std::vector<Box<Expression>>&& exprs);

protected:
    DotExpression(DotExpression const& rhs);
    DotExpression& operator = (DotExpression const& rhs);

public:
    ~DotExpression() KYFOO_DEBUG_OVERRIDE;

    void swap(DotExpression& rhs) noexcept;

    // Expression
    DECL_CLONE_ALL(Expression)
protected:
    SymRes resolveSymbols(Context& ctx);

public:
    SymRes resolveSymbols(Context& ctx, uz subExpressionLimit);

    Slice<Expression*> expressions();
    Slice<Expression const*> expressions() const;

    Expression* top(uz index = 0);
    Expression const* top(uz index = 0) const;

    bool isModuleScope() const;

protected:
    Box<Expression> takeTop(uz index = 0);

private:
    std::vector<Box<Expression>> myExpressions;
    bool myModScope = false;
};

class ApplyExpression : public Expression
{
public:
    friend class Context;
    friend class ProcedureDeclaration;

public:
    ApplyExpression(std::vector<Box<Expression>>&& expressions);

protected:
    ApplyExpression(ApplyExpression const& rhs);
    ApplyExpression& operator = (ApplyExpression const& rhs);

public:
    ~ApplyExpression() KYFOO_DEBUG_OVERRIDE;

    void swap(ApplyExpression& rhs) noexcept;

    // Expression
    DECL_CLONE_ALL(Expression)
protected:
    SymRes resolveSymbols(Context& ctx);

public:
    Declaration const* resolveSubjectAsUFCSMethod(Context& ctx, IdentifierExpression& id);
    SymRes lowerToApplicable(Context& ctx);
    SymRes lowerToStaticCall(Context& ctx);
    SymRes elaborateTuple(Context& ctx);
    void flatten();
    void flatten(std::vector<Box<Expression>>::iterator first);

public:
    Slice<Expression*> expressions();
    Slice<Expression const*> expressions() const;

    Expression* subject();
    Expression const* subject() const;

    Slice<Expression*> arguments();
    Slice<Expression const*> arguments() const;

    ProcedureDeclaration const* procedure() const;

protected:
    Slice<Box<Expression>> mutableArgs();

private:
    // AST state
    std::vector<Box<Expression>> myExpressions;
    ProcedureDeclaration const* myProc = nullptr;
};

class LambdaExpression : public Expression
{
public:
    friend class Context;

public:
    explicit LambdaExpression(ProcedureDeclaration& proc);

protected:
    LambdaExpression(LambdaExpression const& rhs);
    LambdaExpression& operator = (LambdaExpression const& rhs);

public:
    ~LambdaExpression() KYFOO_DEBUG_OVERRIDE;

    void swap(LambdaExpression& rhs) noexcept;

    // Expression
    DECL_CLONE_ALL(Expression)
protected:
    SymRes resolveSymbols(Context& ctx);

public:
    ProcedureDeclaration const& procedure() const;
    ProcedureDeclaration& procedure();

private:
    ProcedureDeclaration* myProc = nullptr;
};

// todo: change to statement only
class AssignExpression : public Expression
{
public:
    friend class Context;

public:
    AssignExpression(Box<Expression> lhs,
                     Box<Expression> rhs);
    AssignExpression(VariableDeclaration const& var,
                     Box<Expression> expression);

protected:
    AssignExpression(AssignExpression const& rhs);
    AssignExpression& operator = (AssignExpression const& rhs);

public:
    ~AssignExpression() KYFOO_DEBUG_OVERRIDE;

    void swap(AssignExpression& rhs) noexcept;

    // Expression
    DECL_CLONE_ALL(Expression)
protected:
    SymRes resolveSymbols(Context& ctx);

public:
    Expression const& left() const;
    Expression& left();
    Expression const& right() const;
    Expression& right();

    Box<Expression> takeLeft();
    Box<Expression> takeRight();

private:
    Box<Expression> myLeft;
    Box<Expression> myRight;
};

class ExpressionArray
{
public:
    struct iterator_end
    {
        uz i;
    };

    class iterator
    {
        Slice<Expression const*> const* s;
        uz ei = 0;
        uz ai = 0;

    public:
        explicit iterator(Slice<Expression const*> const& s)
            : s(&s)
        {
        }

    public:
        void operator++()
        {
            ++ei;
            if ( ei == s->card() ) {
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
    ExpressionArray(Slice<Expression const*> exprs, uz n)
        : exprs(exprs)
        , n(!exprs ? 0 : n)
    {
    }

    /*implicit*/ ExpressionArray(Slice<Expression const*> exprs)
        : ExpressionArray(exprs, 1)
    {
    }

public:
    iterator begin() const { return iterator(exprs); }
    iterator_end end() const { return iterator_end{n}; }

    uz card() const { return exprs.card() * n; }
    bool empty() const { return card() == 0; }

    Expression const* front() const { return exprs.front(); }
    Expression const* back() const { return exprs.back(); }

    Expression const* operator[](uz i) const { return exprs[i % $]; }

private:
    Slice<Expression const*> exprs;
    uz n = 1;
};

inline ExpressionArray::iterator begin(ExpressionArray const& rhs) { return rhs.begin(); }
inline ExpressionArray::iterator_end end(ExpressionArray const& rhs) { return rhs.end(); }

class TupleExpression : public Expression
{
public:
    friend class Context;
    friend class ApplyExpression;

public:
    TupleExpression(TupleKind kind,
                    std::vector<Box<Expression>>&& expressions);
    TupleExpression(lexer::Token const& open,
                    lexer::Token const& close,
                    std::vector<Box<Expression>>&& expressions);

    TupleExpression(std::vector<Box<Expression>>&& expressions,
                    Box<Expression> cardExpression);

protected:
    TupleExpression(TupleExpression const& rhs);
    TupleExpression& operator = (TupleExpression const& rhs);

public:
    ~TupleExpression() KYFOO_DEBUG_OVERRIDE;

    void swap(TupleExpression& rhs) noexcept;

    // Expression
    DECL_CLONE_ALL(Expression)
protected:
    SymRes resolveSymbols(Context& ctx);

public:
    TupleKind kind() const;
    lexer::Token const& openToken() const;
    lexer::Token const& closeToken() const;

    Slice<Expression*> expressions();
    Slice<Expression const*> expressions() const;

    ExpressionArray elements() const;
    uz elementsCount() const;

public:
    static std::optional<std::vector<Box<Expression>>::iterator>
    tryExpandTuple(std::vector<Box<Expression>>& exprs,
                   std::vector<Box<Expression>>::iterator i);

    static std::vector<Box<Expression>>::iterator
    expandTuple(std::vector<Box<Expression>>& exprs,
                std::vector<Box<Expression>>::iterator i);

    static std::vector<Box<Expression>>::iterator
    expandIntoList(TupleExpression& tup,
                   std::vector<Box<Expression>>& exprs,
                   std::vector<Box<Expression>>::iterator i);

    static void flattenOpenTuples(std::vector<Box<Expression>>& exprs);

private:
    // AST state
    TupleKind myKind;
    std::vector<Box<Expression>> myExpressions;
    Box<Expression> myCardExpression;
    uz myCard = 0;

    lexer::Token myOpenToken;
    lexer::Token myCloseToken;
};

class ArrowExpression : public Expression
{
public:
    friend class Context;

public:
    ArrowExpression(Box<Expression> from,
                    Box<Expression> to);

protected:
    ArrowExpression(ArrowExpression const& rhs);
    ArrowExpression& operator = (ArrowExpression const& rhs);

public:
    ~ArrowExpression() KYFOO_DEBUG_OVERRIDE;

    void swap(ArrowExpression& rhs) noexcept;

    // Expression
    DECL_CLONE_ALL(Expression)
protected:
    SymRes resolveSymbols(Context& ctx);

public:
    Expression const& from() const;
    Expression& from();

    Slice<Expression const*> sliceFrom() const;

    Expression const& to() const;
    Expression& to();

    Slice<Expression const*> sliceTo() const;

    Box<Expression> takeFrom();
    Box<Expression> takeTo();

private:
    Box<Expression> myFrom;
    Box<Expression> myTo;
};

class UniverseExpression : public Expression
{
public:
    friend class Context;
    friend class Strata;
    using natural_t = uz; // todo

private:
    explicit UniverseExpression(natural_t level);

protected:
    UniverseExpression(UniverseExpression const& rhs);
    UniverseExpression& operator = (UniverseExpression const& rhs);

public:
    ~UniverseExpression() KYFOO_DEBUG_OVERRIDE;

    void swap(UniverseExpression& rhs) noexcept;

    // Expression
    DECL_CLONE_ALL(Expression)
protected:
    SymRes resolveSymbols(Context& ctx);

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

inline Box<Expression> beginClone(Expression const& expr, clone_map_t& map)
{
    switch (expr.kind()) {
#define X(a,b) case Expression::Kind::a: return static_cast<b const&>(expr).beginClone(map);
    EXPRESSION_KINDS(X)
#undef X
    }

    ENFORCEU("invalid expression type");
}

inline void remap(Expression& expr, clone_map_t const& map)
{
    switch (expr.kind()) {
#define X(a,b) case Expression::Kind::a: return static_cast<b&>(expr).remapReferences(map);
    EXPRESSION_KINDS(X)
#undef X
    }

    ENFORCEU("invalid expression type");
}

std::ostream& operator << (std::ostream& stream, Expression        const& expr );
std::ostream& operator << (std::ostream& stream, Slice<Expression const*> exprs);
std::ostream& operator << (std::ostream& stream, Slice<Expression      *> exprs);

struct get_types {
    Slice<Expression const*> exprs;

    explicit get_types(Slice<Expression const*> rhs) : exprs(rhs) {}
    explicit get_types(Slice<Expression*> rhs) : exprs(rhs) {}
};

std::ostream& operator << (std::ostream& stream, get_types&& types);

lexer::SourceLocation getSourceLocation(Expression const& expr);
Expression const* createInferredType(Expression& expr, Declaration const& decl);
IdentifierExpression* identify(Expression& expr);
IdentifierExpression const* identify(Expression const& expr);
bool hasDeclaration(Expression const& expr);
Declaration const* getDeclaration(Expression const& expr);
Declaration const* getDeclaration(Expression const* expr);

template <typename T>
T const* as(Expression const& expr)
{
    if ( auto decl = getDeclaration(expr) )
        return decl->as<T>();

    return nullptr;
}

template <typename T>
T const* as(Expression const* expr)
{
    if ( expr )
        return as<T>(*expr);

    return nullptr;
}

std::vector<Box<Expression>> flattenConstraints(Box<Expression> expr);
bool isUnit(Expression const& expr);

struct DeclRef
{
    Declaration const* decl;
    Expression const* type;
};

DeclRef getRef(Expression const& expr);
Expression const* getRefType(Expression const& expr);
ProcedureDeclaration const* getProcedure(Expression const& expr);

    } // namespace ast
} // namespace kyfoo
