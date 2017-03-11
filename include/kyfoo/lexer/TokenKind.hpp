#pragma once

namespace kyfoo {
    namespace lexer {

#define TOKEN_DEFINITIONS \
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
    X(OpenParen, "OpenParen") \
    X(CloseParen, "CloseParen") \
    X(OpenBracket, "OpenBracket") \
    X(CloseBracket, "CloseBracket") \
    X(OpenAngle, "OpenAngle") \
    X(CloseAngle, "CloseAngle") \
    X(OpenBrace, "OpenBrace") \
    X(CloseBrace, "CloseBrace") \
    \
    X(Range, "Range") \
    X(Dot, "Dot") \
    X(Colon, "Colon") \
    X(Pipe, "Pipe") \
    X(Comma, "Comma") \
    \
    X(Plus, "plus") \
    X(Minus, "minus") \
    X(Star, "star") \
    X(Slash, "Slash") \
    X(Yield, "yield") \
    \
    X(_if, "if") \
    X(_var, "var") \
    X(_type, "type") \
    X(_else, "else") \
    X(_is, "is") \
    X(_in, "in")

#define X(A,B) A,
enum class TokenKind
{
TOKEN_DEFINITIONS
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
