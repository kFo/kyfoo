#pragma once

#include <kyfoo/lexer/TokenKind.hpp>

namespace kyfoo {
    namespace lexer {

#define X(A,B) #A,
const char* g_tokenKindStringTable[] =
{
TOKEN_DEFINITIONS
};
#undef X

const char* toString(TokenKind kind)
{
    return g_tokenKindStringTable[static_cast<int>(kind)];
}

    } // namespace lexer
} // namespace kyfoo
