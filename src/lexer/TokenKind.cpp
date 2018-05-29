#pragma once

#include <kyfoo/lexer/TokenKind.hpp>

namespace kyfoo::lexer {

#define X(A,B) B,
const char* g_tokenKindStringTable[] =
{
TOKEN_DEFINITIONS(X)
};
#undef X

const char* to_string(TokenKind kind)
{
    return g_tokenKindStringTable[static_cast<int>(kind)];
}

} // namespace kyfoo::lexer
