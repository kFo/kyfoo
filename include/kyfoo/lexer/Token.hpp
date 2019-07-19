#pragma once

#include <kyfoo/String.hpp>
#include <kyfoo/Types.hpp>
#include <kyfoo/lexer/TokenKind.hpp>

namespace kyfoo::lexer {

using LineIndex = u32;
using ColumnIndex = u32;

struct SourceLocation
{
    LineIndex line = 0;
    ColumnIndex column = 0;

    constexpr SourceLocation() noexcept = default;
    constexpr SourceLocation(LineIndex line, ColumnIndex col) noexcept
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
    constexpr LineIndex   line    () const noexcept { return myLoc.line  ; }
    constexpr ColumnIndex column  () const noexcept { return myLoc.column; }

private:
    TokenKind myKind = TokenKind::Undefined;
    stringv myLexeme;
    SourceLocation myLoc;
};

} // namespace kyfoo::lexer

namespace kyfoo::ascii {
    template <typename Sink>
    void write(Sink& sink, lexer::SourceLocation loc)
    {
        write(sink, loc.line);
        write(sink, ':');
        write(sink, loc.column);
    }
}
