#pragma once

namespace kyfoo {
    namespace lexer {

#define TOKEN_DEFINITIONS(X) \
    X(Undefined, "undefined") \
    X(EndOfFile, "EOF"      ) \
    \
    X(IndentLT   , "indent(<)"  ) \
    X(IndentEQ   , "indent(=)"  ) \
    X(IndentGT   , "indent(>)"  ) \
    X(IndentError, "indent(err)") \
    \
    X(Identifier  , "identifier"  ) \
    X(FreeVariable, "freeVariable") \
    X(Integer     , "integer"     ) \
    X(Rational    , "rational"    ) \
    X(String      , "string"      ) \
    \
    X(OpenParen   , "openParen"   ) \
    X(CloseParen  , "closeParen"  ) \
    X(OpenBracket , "openBracket" ) \
    X(CloseBracket, "closeBracket") \
    X(OpenAngle   , "openAngle"   ) \
    X(CloseAngle  , "closeAngle"  ) \
    X(OpenBrace   , "openBrace"   ) \
    X(CloseBrace  , "closeBrace"  ) \
    \
    X(Equal         , "equal"         ) \
    X(Range         , "range"         ) \
    X(Dot           , "dot"           ) \
    X(Colon         , "colon"         ) \
    X(Semicolon     , "semicolon"     ) \
    X(Pipe          , "pipe"          ) \
    X(Comma         , "comma"         ) \
    X(ColonPipe     , "colonPipe"     ) \
    X(ColonAmpersand, "colonAmpersand") \
    X(ColonEqual    , "colonEqual"    ) \
    X(ColonQuestion , "colonQuestion" ) \
    X(ColonSlash    , "colonSlash"    ) \
    \
    X(Yield, "yield") \
    X(Map  , "map"  ) \
    \
    X(At        , "at"         ) \
    X(MinusMinus, "minus minus") \
    \
    X(_keywordStart, "keyword start") \
    X(_import      , "import"       ) \
    X(_return      , "return"       ) \
    X(_keywordEnd  , "keyword end"  )

#define X(A,B) A,
enum class TokenKind
{
TOKEN_DEFINITIONS(X)
};
#undef X

const char* to_string(TokenKind kind);

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

inline bool isAngle(TokenKind kind)
{
    return kind == lexer::TokenKind::OpenAngle
        || kind == lexer::TokenKind::CloseAngle;
}

inline bool isBrace(TokenKind kind)
{
    return kind == lexer::TokenKind::OpenBrace
        || kind == lexer::TokenKind::CloseBrace;
}

inline bool isIndent(TokenKind kind)
{
    switch (kind) {
    case TokenKind::IndentLT:
    case TokenKind::IndentEQ:
    case TokenKind::IndentGT:
        return true;
    }

    return false;
}

inline bool isBreak(TokenKind kind)
{
    return isIndent(kind) || kind == TokenKind::EndOfFile;
}

inline bool isIdentifier(TokenKind kind)
{
    switch (kind) {
    case TokenKind::Identifier:
    case TokenKind::FreeVariable:
        return true;
    }

    return false;
}

inline bool isLiteral(TokenKind kind)
{
    return !isIdentifier(kind);
}

    } // namespace lexer
} // namespace kyfoo
