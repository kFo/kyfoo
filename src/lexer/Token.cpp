#include <kyfoo/lexer/Token.hpp>

#include <kyfoo/Utilities.hpp>

namespace kyfoo::lexer {

Token::Token() = default;

Token::Token(TokenKind kind,
             std::string lexeme,
             SourceLocation loc)
    : myKind(kind)
    , myLexeme(std::move(lexeme))
    , myLoc(loc)
{
}

Token::Token(Token const& rhs)
    : myKind(rhs.myKind)
    , myLexeme(rhs.myLexeme)
    , myLoc(rhs.myLoc)
{
}

Token& Token::operator = (Token const& rhs)
{
    Token(rhs).swap(*this);
    return *this;
}

Token::Token(Token&& rhs)
    : myKind(rhs.myKind)
    , myLexeme(rhs.myLexeme)
    , myLoc(rhs.myLoc)
{
}

Token& Token::operator = (Token&& rhs)
{
    this->~Token();
    new (this) Token(std::move(rhs));
    return *this;
}

void Token::swap(Token& rhs) noexcept
{
    using kyfoo::swap;

    swap(myKind, rhs.myKind);
    swap(myLexeme, rhs.myLexeme);
    swap(myLoc, rhs.myLoc);
}

bool Token::operator < (Token const& rhs) const
{
    return myKind < rhs.myKind;
}

Token::operator bool () const
{
    return myKind != TokenKind::Undefined;
}

TokenKind Token::kind() const
{
    return myKind;
}

std::string_view Token::lexeme() const
{
    return myLexeme;
}

SourceLocation Token::location() const
{
    return myLoc;
}

line_index_t Token::line() const
{
    return myLoc.line;
}

column_index_t Token::column() const
{
    return myLoc.column;
}

} // namespace kyfoo::lexer
