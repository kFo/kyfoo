#pragma once

#include <string>

#include <kyfoo/Types.hpp>
#include <kyfoo/lexer/TokenKind.hpp>

namespace kyfoo::lexer {

using line_index_t = u32;
using column_index_t = u32;

struct SourceLocation
{
    line_index_t line = 0;
    column_index_t column = 0;

    SourceLocation() = default;
    SourceLocation(line_index_t line, column_index_t col)
        : line(line)
        , column(col)
    {
    }

    SourceLocation(SourceLocation const&) = default;
    SourceLocation& operator = (SourceLocation const&) = default;

    SourceLocation(SourceLocation&&) = default;
    SourceLocation& operator = (SourceLocation&&) = default;

    ~SourceLocation() = default;

    void swap(SourceLocation& rhs)
    {
        using kyfoo::swap;
        swap(line, rhs.line);
        swap(column, rhs.column);
    }
};

class Token
{
    TokenKind myKind = TokenKind::Undefined;
    std::string myLexeme;
    SourceLocation myLoc = { 0, 0 };

public:
    explicit Token();
    Token(TokenKind kind,
          std::string lexeme,
          SourceLocation loc);

public:
    Token(Token const&);
    Token& operator = (Token const&);

    Token(Token&&);
    Token& operator = (Token&&);

    void swap(Token&) noexcept;

public:
    bool operator < (Token const&) const;

    explicit operator bool () const;

public:
    TokenKind kind() const;
    std::string_view lexeme() const;
    SourceLocation location() const;
    line_index_t line() const;
    column_index_t column() const;
};

} // namespace kyfoo::lexer
