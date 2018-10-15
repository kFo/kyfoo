#include <kyfoo/lexer/TokenKind.hpp>

namespace kyfoo::lexer {

stringv to_string(TokenKind kind)
{
    #define X(A,B) B,
    static stringv tokenKindStringTable[] =
    {
    TOKEN_DEFINITIONS(X)
    };
    #undef X

    return tokenKindStringTable[static_cast<int>(kind)];
}

} // namespace kyfoo::lexer
