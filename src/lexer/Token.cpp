#include <kyfoo/lexer/Token.hpp>

namespace kyfoo {
    namespace lexer {

Token::Token()
{
}

Token::Token(TokenKind kind,
             line_index_t line,
             column_index_t column,
             std::string const& lexeme)
    : myKind(kind)
    , myLine(line)
    , myColumn(column)
    , myLexeme(lexeme)
{
}

Token::Token(Token const& rhs)
    : myKind(rhs.myKind)
    , myLine(rhs.myLine)
    , myColumn(rhs.myColumn)
    , myLexeme(rhs.myLexeme)
{
}

Token::Token(Token const&& rhs)
    : myKind(rhs.myKind)
    , myLine(rhs.myLine)
    , myColumn(rhs.myColumn)
    , myLexeme(rhs.myLexeme)
{
}

Token& Token::operator = (Token const& rhs)
{
    if ( &rhs != this )
        Token(rhs).swap(*this);
    
    return *this;
}

void Token::swap(Token& rhs)
{
    using std::swap;

    swap(myKind, rhs.myKind);
    swap(myLine, rhs.myLine);
    swap(myColumn, rhs.myColumn);
    swap(myLexeme, rhs.myLexeme);
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

line_index_t Token::line() const
{
    return myLine;
}

column_index_t Token::column() const
{
    return myColumn;
}

std::string const& Token::lexeme() const
{
    return myLexeme;
}

    } // namespace lexer
} // namespace kyfoo
