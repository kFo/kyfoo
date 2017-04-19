#pragma once

#include <memory>
#include <sstream>
#include <vector>

#include <kyfoo/lexer/Token.hpp>

namespace kyfoo {
    namespace ast {
        class Declaration;
    }

class Error
{
public:
    enum Code
    {
        General,
        Undeclared,
    };

public:
    explicit Error(lexer::Token const& token);
    explicit Error(lexer::Token const& token, Code code);
    Error(Error& rhs) = delete;

public:
    std::string what() const;
    lexer::Token const& token() const;
    std::vector<ast::Declaration const*> const& references() const;

public:
    void see(ast::Declaration const* declaration);

public:
    Error& operator << (lexer::Token const& token);
    Error& operator << (std::string const& rhs);

private:
    lexer::Token myToken;
    Code myCode;
    std::ostringstream myInfo;
    std::vector<ast::Declaration const*> myReferences;
};

std::ostream& operator << (std::ostream& sink, Error const& err);

class Diagnostics
{
public:
    void die();

    Error& error(lexer::Token const& token);
    Error& undeclared(lexer::Token const& token);

    void dumpErrors(std::ostream& stream);

    std::size_t errorCount() const;

private:
    std::vector<std::unique_ptr<Error>> myErrors;
};

} // namespace kyfoo
