#pragma once

#include <kyfoo/String.hpp>
#include <kyfoo/Types.hpp>
#include <kyfoo/lexer/TokenKind.hpp>

namespace kyfoo::lexer {

using line_index_t = u32;
using column_index_t = u32;

struct SourceLocation
{
    line_index_t line = 0;
    column_index_t column = 0;

    constexpr SourceLocation() noexcept = default;
    constexpr SourceLocation(line_index_t line, column_index_t col) noexcept
        : line(line)
        , column(col)
    {
    }

    void swap(SourceLocation& rhs)
    {
        using kyfoo::swap;
        swap(line, rhs.line);
        swap(column, rhs.column);
    }
};

class Token
{
public:
    constexpr explicit Token() noexcept = default;

    constexpr Token(TokenKind kind, stringv lexeme, SourceLocation loc) noexcept
        : myKind(kind)
        , myLexeme(lexeme)
        , myLoc(loc)
    {
    }

    constexpr Token(TokenKind kind, SourceLocation loc) noexcept
        : myKind(kind)
        , myLoc(loc)
    {
    }

    void swap(Token& rhs) noexcept
    {
        using kyfoo::swap;

        swap(myKind, rhs.myKind);
        swap(myLexeme, rhs.myLexeme);
        swap(myLoc, rhs.myLoc);
    }

public:
    constexpr bool operator < (Token const& rhs) const noexcept
    {
        return myKind < rhs.myKind;
    }

    constexpr explicit operator bool () const noexcept
    {
        return myKind != TokenKind::Undefined;
    }

public:
    constexpr TokenKind      kind    () const noexcept { return myKind      ; }
    constexpr stringv        lexeme  () const noexcept { return myLexeme    ; }
    constexpr SourceLocation location() const noexcept { return myLoc       ; }
    constexpr line_index_t   line    () const noexcept { return myLoc.line  ; }
    constexpr column_index_t column  () const noexcept { return myLoc.column; }

private:
    TokenKind myKind = TokenKind::Undefined;
    stringv myLexeme;
    SourceLocation myLoc;
};

} // namespace kyfoo::lexer
