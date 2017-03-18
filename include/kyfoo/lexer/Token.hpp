#pragma once

#include <string>

#include "TokenKind.hpp"

namespace kyfoo {
    namespace lexer {

using line_index_t = std::size_t;
using column_index_t = std::size_t;

class Token
{
    TokenKind myKind = TokenKind::EndOfFile;
    line_index_t myLine = 0;
    column_index_t myColumn = 0;
    std::string myLexeme;

public:
    explicit Token();
    Token(TokenKind kind,
          line_index_t line,
          column_index_t column,
          std::string const& lexeme);

public:
    Token(Token const&);
    Token(Token const&&);
    Token& operator = (Token const&);
    void swap(Token&);

public:
    bool operator < (Token const&) const;

public:
    TokenKind kind() const;
    line_index_t line() const;
    column_index_t column() const;
    std::string const& lexeme() const;
};

    } // namespace lexer
} // namespace kyfoo
