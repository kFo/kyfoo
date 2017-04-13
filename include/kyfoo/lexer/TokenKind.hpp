#pragma once

namespace kyfoo {
    namespace lexer {

#define TOKEN_DEFINITIONS(X) \
    X(EndOfFile, "EOF") \
    X(Indent, "indent") \
    X(LineBreak, "EOL") \
    X(Comment, "comment") \
    X(Identifier, "identifier") \
    \
    X(Integer, "integer") \
    X(String, "string") \
    X(Decimal, "decimal") \
    \
    X(Equal, "equal") \
    X(OpenParen, "openParen") \
    X(CloseParen, "closeParen") \
    X(OpenBracket, "openBracket") \
    X(CloseBracket, "closeBracket") \
    X(OpenAngle, "openAngle") \
    X(CloseAngle, "closeAngle") \
    X(OpenBrace, "openBrace") \
    X(CloseBrace, "closeBrace") \
    \
    X(Range, "range") \
    X(Dot, "dot") \
    X(Colon, "colon") \
    X(Pipe, "pipe") \
    X(Comma, "comma") \
    \
    X(Plus, "plus") \
    X(Minus, "minus") \
    X(Star, "star") \
    X(Slash, "slash") \
    X(Yield, "yield") \
    \
    X(_keywordStart, "keyword start") \
    X(_if, "if") \
    X(_var, "var") \
    X(_type, "type") \
    X(_else, "else") \
    X(_is, "is") \
    X(_in, "in") \
    X(_import, "import") \
    X(_keywordEnd, "keyword end")

#define X(A,B) A,
enum class TokenKind
{
TOKEN_DEFINITIONS(X)
};
#undef X

const char* toString(TokenKind kind);

inline bool isParen(TokenKind kind)
{
    return kind == lexer::TokenKind::OpenParen
        || kind == lexer::TokenKind::CloseParen;
}

inline bool isBracket(TokenKind kind)
{
    return kind == lexer::TokenKind::OpenBracket
        || kind == lexer::TokenKind::CloseBracket;
}

inline bool isBreak(TokenKind kind)
{
    return kind == TokenKind::LineBreak
        || kind == TokenKind::EndOfFile;
}

    } // namespace lexer
} // namespace kyfoo
