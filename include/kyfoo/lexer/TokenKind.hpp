#pragma once

namespace kyfoo::lexer {

#define TOKEN_DEFINITIONS(X) \
    X(Undefined, "undefined") \
    X(EndOfFile, "EOF"      ) \
    \
    X(IndentLT   , "indent(<)"  ) \
    X(IndentEQ   , "indent(=)"  ) \
    X(IndentGT   , "indent(>)"  ) \
    X(IndentError, "indent(err)") \
    \
    X(Vacuum     , "vacuum") \
    X(Hyphen     , "hyphen") \
    \
    X(Identifier  , "identifier"  ) \
    X(MetaVariable, "metaVariable") \
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
    X(DotDot        , "range"         ) \
    X(Dot           , "dot"           ) \
    X(Colon         , "colon"         ) \
    X(Semicolon     , "semicolon"     ) \
    X(Pipe          , "pipe"          ) \
    X(Comma         , "comma"         ) \
    \
    X(ColonPipe         , "colonPipe"         ) \
    X(ColonAmpersand    , "colonAmpersand"    ) \
    X(ColonEqual        , "colonEqual"        ) \
    X(ColonStar         , "colonStar"         ) \
    X(ColonQuestion     , "colonQuestion"     ) \
    X(ColonSlash        , "colonSlash"        ) \
    X(ColonOpenAngle    , "colonOpenAngle"    ) \
    X(ColonStarAngle    , "colonStarAngle"    ) \
    X(ColonQuestionAngle, "colonQuestionAngle") \
    X(ColonSlashAngle   , "colonSlashAngle"   ) \
    X(ColonPlus         , "colonPlus"         ) \
    X(ColonMinus        , "colonMinus"        ) \
    X(ColonDot          , "colonDot"          ) \
    \
    X(Yield, "yield") \
    X(Arrow, "arrow") \
    \
    X(At        , "at"         ) \
    X(MinusMinus, "minus minus") \
    \
    X(_keywordStart, "keyword start") \
    X(_import      , "import"       ) \
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
    case TokenKind::MetaVariable:
        return true;
    }

    return false;
}

inline bool isLiteral(TokenKind kind)
{
    switch ( kind ) {
    case TokenKind::Integer:
    case TokenKind::Rational:
    case TokenKind::String:
        return true;
    }

    return false;
}

} // namespace kyfoo::lexer
