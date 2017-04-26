#pragma once

#include <string>

#include <kyfoo/lexer/TokenKind.hpp>

namespace kyfoo {
    namespace ast {

enum class TupleKind
{
    Open,
    HalfOpenRight,
    HalfOpenLeft,
    Closed
};

TupleKind toTupleKind(lexer::TokenKind open, lexer::TokenKind close);
const char* to_string(TupleKind kind);

    } // namesapce ast
} // namespace kyfoo
