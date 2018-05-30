#pragma once

#include <string>

#include <kyfoo/Types.hpp>
#include <kyfoo/lexer/TokenKind.hpp>

namespace kyfoo::lexer {

using line_index_t = uz;
using column_index_t = uz;

class Token
{
    TokenKind myKind = TokenKind::Undefined;
    std::string myLexeme;
    line_index_t myLine = 0;
    column_index_t myColumn = 0;

public:
    explicit Token();
    Token(TokenKind kind,
          line_index_t line,
          column_index_t column,
          std::string lexeme);

public:
    Token(Token const&);
    Token& operator = (Token const&);

    Token(Token&&);
    Token& operator = (Token&&);

    void swap(Token&);

public:
    bool operator < (Token const&) const;

    explicit operator bool () const;

public:
    TokenKind kind() const;
    line_index_t line() const;
    column_index_t column() const;
    std::string_view lexeme() const;
};

} // namespace kyfoo::lexer
