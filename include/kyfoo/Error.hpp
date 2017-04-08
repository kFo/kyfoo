#pragma once

#include <sstream>

#include <kyfoo/lexer/Token.hpp>

namespace kyfoo {

class Error
{
public:
    explicit Error(lexer::Token const& token)
        : myToken(token)
    {
    }

    Error(Error& rhs)
        : myToken(rhs.myToken)
    {
        //myInfo << rhs.myInfo.rdbuf();
        std::swap(myInfo, rhs.myInfo);
    }

public:
    std::string what() const
    {
        return myInfo.str();
    }

    lexer::Token const& token() const
    {
        return myToken;
    }

public:
    Error& operator << (lexer::Token const& token)
    {
        myInfo << '\'' << token.lexeme() << '\'';
        return *this;
    }

    Error& operator << (std::string const& rhs)
    {
        myInfo << rhs;
        return *this;
    }

private:
    lexer::Token myToken;
    std::ostringstream myInfo;
};

inline std::ostream& operator << (std::ostream& sink, Error const& err)
{
    sink << '(' << err.token().line() << ", " << err.token().column() << "): error: ";
    if ( !err.token().lexeme().empty() )
        sink << "'" << err.token().lexeme() << "' ";

    sink << err.what() << std::endl;

    return sink;
}

} // namespace kyfoo
