#pragma once

#include <chrono>
#include <sstream>
#include <vector>

#include <kyfoo/Slice.hpp>
#include <kyfoo/Types.hpp>

#include <kyfoo/lexer/Token.hpp>

namespace kyfoo::ast {
    class Expression;
    class Declaration;
    class Scope;
    class Junction;
    class Lookup;
    class Module;
    class Statement;
}

namespace kyfoo {

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
        SeeLookup,
        MismatchExpected,
        MismatchReceived,
    };

    struct ExpressionContextBase {
        enum Kind {
            Value,
            Type,
        };

        Kind kind = Value;
        ast::Scope const* scope = nullptr;

        ExpressionContextBase() = default;

        ExpressionContextBase(Kind kind, ast::Scope const& scope)
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
                          ast::Scope const& scope,
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
                           ast::Scope const& scope,
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
                     ast::Scope const& scope,
                     ast::Expression const& expr)
        : myKind(SeeExpression)
        , myContext(kind, scope, expr)
    {
    }

    ContextReference(Kind kind,
                     ExpressionContextBase::Kind exprKind,
                     ast::Scope const& scope,
                     Slice<ast::Expression const*> exprs)
        : myKind(kind)
        , myContext(exprKind, scope, exprs)
    {
    }

    ContextReference(ast::Lookup&& miss)
        : myKind(SeeLookup)
        , myContext(std::move(miss))
    {
    }

    ContextReference(ContextReference const& rhs)
        : myKind(rhs.myKind)
    {
        std::memcpy(&myContext, &rhs.myContext, sizeof(rhs.myContext));
    }

    ~ContextReference();

public:
    ast::Declaration const* seeDecl() const
    {
        return myKind == SeeDeclaration ? myContext.decl : nullptr;
    }

    SingleExpressionContext seeExpr() const
    {
        return myKind == SeeExpression ? myContext.exprSingle : SingleExpressionContext();
    }

    ast::Lookup const* seeLookup() const
    {
        return myKind == SeeLookup ? myContext.miss : nullptr;
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
        Ctx() {};

        Ctx(ast::Declaration const* decl) : decl(decl) {}
        ast::Declaration const* decl;

        Ctx(ExpressionContextBase::Kind kind,
            ast::Scope const& scope,
            ast::Expression const& expr) : exprSingle(kind, scope, expr) {}
        SingleExpressionContext exprSingle;

        Ctx(ExpressionContextBase::Kind kind,
            ast::Scope const& scope,
            Slice<ast::Expression const*> exprs) : exprMany(kind, scope, exprs) {}
        ManyExpressionContext exprMany;

        Ctx(ast::Lookup&& miss);
        ast::Lookup* miss;
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
    Error& see(ast::Scope const& scope, ast::Expression const& expression);
    Error& see(ast::Lookup&& miss);
    Error& expected(ast::Scope const& scope, Slice<ast::Expression const*> exprs);
    Error& expectedTypes(ast::Scope const& scope, Slice<ast::Expression const*> exprs);
    Error& received(ast::Scope const& scope, Slice<ast::Expression const*> exprs);
    Error& receivedTypes(ast::Scope const& scope, Slice<ast::Expression const*> exprs);

public:
    std::ostream& stream();
    Error& operator << (lexer::Token const& token);

    template <typename T>
    friend Error& operator << (Error& err, T&& rhs);

private:
    ast::Module const* myModule = nullptr;
    ast::Expression const* myExpression = nullptr;
    lexer::Token myToken;
    Code myCode = General;
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

    Error& error(ast::Module const& mod);
    Error& error(ast::Module const& mod, lexer::Token const& token);
    Error& error(ast::Module const& mod, ast::Expression const& expr);
    Error& error(ast::Module const& mod, ast::Statement const& stmt);
    Error& error(ast::Module const& mod, ast::Junction const& junc);
    Error& error(ast::Module const& mod, ast::Declaration const& decl);

    void dumpErrors(std::ostream& stream);

    uz errorCount() const;

    void bunkExpression(Box<ast::Expression> expr);

private:
    std::vector<Box<Error>> myErrors;
    std::vector<Box<ast::Expression>> myBunkedExpressions;
};

class DiagnosticsContext
{
public:
    DiagnosticsContext(Diagnostics& dgn, ast::Module const& mod);

public:
    Error& error();
    Error& error(lexer::Token const& token);
    Error& error(ast::Expression const& expr);
    Error& error(ast::Statement const& stmt);
    Error& error(ast::Junction const& junc);
    Error& error(ast::Declaration const& decl);

public:
    Diagnostics& diagnostics();
    ast::Module const& module();

private:
    Diagnostics* myDiagnostics = nullptr;
    ast::Module const* myModule = nullptr;
};

std::ostream& operator << (std::ostream& sink, ast::Module const& mod);
std::ostream& operator << (std::ostream& sink, lexer::SourceLocation loc);

} // namespace kyfoo
