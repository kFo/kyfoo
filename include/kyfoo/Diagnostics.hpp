#pragma once

#include <chrono>
#include <memory>
#include <sstream>
#include <vector>

#include <kyfoo/lexer/Token.hpp>

namespace kyfoo {
    namespace ast {
        class Module;
        class Declaration;
        class Expression;
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
    std::vector<ast::Declaration const*> const& references() const;

public:
    void see(ast::Declaration const* declaration);

public:
    Error& operator << (lexer::Token const& token);

    template <typename T>
    friend Error& operator << (Error&, T&&);

private:
    ast::Module const* myModule = nullptr;
    ast::Expression const* myExpression = nullptr;
    lexer::Token myToken;
    Code myCode;
    std::ostringstream myInfo;
    std::vector<ast::Declaration const*> myReferences;
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
    Error& undeclared(ast::Module const& module, lexer::Token const& token);

    void dumpErrors(std::ostream& stream);

    std::size_t errorCount() const;

private:
    std::vector<std::unique_ptr<Error>> myErrors;
};

} // namespace kyfoo
