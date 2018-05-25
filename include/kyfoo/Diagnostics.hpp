#pragma once

#include <chrono>
#include <memory>
#include <sstream>
#include <vector>

#include <kyfoo/Slice.hpp>

#include <kyfoo/lexer/Token.hpp>

namespace kyfoo {
    namespace ast {
        class Module;
        class DeclarationScope;
        class Declaration;
        class Expression;
        class Statement;
        class Junction;
    }

class StopWatch
{
public:
    StopWatch()
        : myStart(std::chrono::system_clock::now())
    {
    }

    std::chrono::duration<double> elapsed()
    {
        return std::chrono::system_clock::now() - myStart;
    }

    std::chrono::duration<double> reset()
    {
        auto now = std::chrono::system_clock::now();
        auto elapsed = now - myStart;
        myStart = now;
        return elapsed;
    }

private:
    std::chrono::system_clock::time_point myStart;
};

class ContextReference
{
public:
    enum Kind {
        SeeDeclaration,
        SeeExpression,
        MismatchExpected,
        MismatchReceived,
    };

    struct ExpressionContextBase {
        enum Kind {
            Value,
            Type,
        };

        Kind kind = Value;
        ast::DeclarationScope const* scope = nullptr;

        ExpressionContextBase() = default;

        ExpressionContextBase(Kind kind, ast::DeclarationScope const& scope)
            : kind(kind)
            , scope(&scope)
        {
        }

        explicit operator bool() const { return scope != nullptr; }

        bool isValue() const { return kind == Value; }
    };

    struct SingleExpressionContext : ExpressionContextBase {
        ast::Expression const* expr = nullptr;

        SingleExpressionContext() = default;

        SingleExpressionContext(Kind kind,
                          ast::DeclarationScope const& scope,
                          ast::Expression const& expr)
            : ExpressionContextBase(kind, scope)
            , expr(&expr)
        {
        }
    };

    struct ManyExpressionContext : ExpressionContextBase {
        Slice<ast::Expression const*> exprs;

        ManyExpressionContext() = default;

        ManyExpressionContext(Kind kind,
                           ast::DeclarationScope const& scope,
                           Slice<ast::Expression const*> exprs)
            : ExpressionContextBase(kind, scope)
            , exprs(exprs)
        {
        }
    };

public:
    explicit ContextReference(ast::Declaration const& decl)
        : myKind(SeeDeclaration)
        , myContext(&decl)
    {
    }

    ContextReference(ExpressionContextBase::Kind kind,
                     ast::DeclarationScope const& scope,
                     ast::Expression const& expr)
        : myKind(SeeExpression)
        , myContext(kind, scope, expr)
    {
    }

    ContextReference(Kind kind,
                     ExpressionContextBase::Kind exprKind,
                     ast::DeclarationScope const& scope,
                     Slice<ast::Expression const*> exprs)
        : myKind(kind)
        , myContext(exprKind, scope, exprs)
    {
    }

    ContextReference(ContextReference const& rhs)
        : myKind(rhs.myKind)
    {
        std::memcpy(&myContext, &rhs.myContext, sizeof(rhs.myContext));
    }

public:
    ast::Declaration const* seeDecl() const
    {
        return myKind == SeeDeclaration ? myContext.decl : nullptr;
    }

    SingleExpressionContext seeExpr() const
    {
        return myKind == SeeExpression ? myContext.exprSingle : SingleExpressionContext();
    }

    ManyExpressionContext expected() const
    {
        return myKind == MismatchExpected ? myContext.exprMany : ManyExpressionContext();
    }

    ManyExpressionContext received() const
    {
        return myKind == MismatchReceived ? myContext.exprMany : ManyExpressionContext();
    }

private:
    Kind myKind;
    union Ctx {
        Ctx(){}

        Ctx(ast::Declaration const* decl) : decl(decl) {}
        ast::Declaration const* decl;

        Ctx(ExpressionContextBase::Kind kind,
            ast::DeclarationScope const& scope,
            ast::Expression const& expr) : exprSingle(kind, scope, expr) {}
        SingleExpressionContext exprSingle;

        Ctx(ExpressionContextBase::Kind kind,
            ast::DeclarationScope const& scope,
            Slice<ast::Expression const*> exprs) : exprMany(kind, scope, exprs) {}
        ManyExpressionContext exprMany;
    } myContext;
};

class Error
{
public:
    enum Code
    {
        General,
        Undeclared,
    };

public:
    explicit Error(ast::Module const& module);
    Error(ast::Module const& module, lexer::Token const& token);
    Error(ast::Module const& module, lexer::Token const& token, Code code);
    Error(ast::Module const& module, ast::Expression const& expr, Code code);
    Error(Error& rhs) = delete;

public:
    ast::Module const& module() const;
    std::string what() const;
    ast::Expression const* expression() const;
    lexer::Token const& token() const;
    Code code() const;
    Slice<ContextReference const> references() const;
    Error& see(ast::Declaration const& declaration);
    Error& see(ast::DeclarationScope const& scope, ast::Expression const& expression);
    Error& expected(ast::DeclarationScope const& scope, Slice<ast::Expression const*> exprs);
    Error& expectedTypes(ast::DeclarationScope const& scope, Slice<ast::Expression const*> exprs);
    Error& received(ast::DeclarationScope const& scope, Slice<ast::Expression const*> exprs);
    Error& receivedTypes(ast::DeclarationScope const& scope, Slice<ast::Expression const*> exprs);

public:
    std::ostream& stream();
    Error& operator << (lexer::Token const& token);

    template <typename T>
    friend Error& operator << (Error&, T&&);

private:
    ast::Module const* myModule = nullptr;
    ast::Expression const* myExpression = nullptr;
    lexer::Token myToken;
    Code myCode;
    std::ostringstream myInfo;
    std::vector<ContextReference> myReferences;
};

template <typename T>
Error& operator << (Error& err, T&& rhs)
{
    err.myInfo << std::forward<T>(rhs);
    return err;
}

std::ostream& operator << (std::ostream& sink, Error const& err);

class Diagnostics
{
public:
    void die();

    Error& error(ast::Module const& module);
    Error& error(ast::Module const& module, lexer::Token const& token);
    Error& error(ast::Module const& module, ast::Expression const& expr);
    Error& error(ast::Module const& module, ast::Statement const& stmt);
    Error& error(ast::Module const& module, ast::Junction const& junc);
    Error& error(ast::Module const& module, ast::Declaration const& decl);
    Error& undeclared(ast::Module const& module, lexer::Token const& token);

    void dumpErrors(std::ostream& stream);

    std::size_t errorCount() const;

    void bunkExpression(std::unique_ptr<ast::Expression> expr);

private:
    std::vector<std::unique_ptr<Error>> myErrors;
    std::vector<std::unique_ptr<ast::Expression>> myBunkedExpressions;
};

} // namespace kyfoo
