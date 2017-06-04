#pragma once

#include <string>

#include <kyfoo/lexer/TokenKind.hpp>

namespace kyfoo {
    namespace ast {

#define TUPLE_KINDS(X) \
    X(Open) \
    X(OpenRight) \
    X(OpenLeft) \
    X(Closed)

enum class TupleKind
{
#define X(a) a,
    TUPLE_KINDS(X)
#undef X
};

TupleKind toTupleKind(lexer::TokenKind open, lexer::TokenKind close);
const char* to_string(TupleKind kind);

    } // namesapce ast
} // namespace kyfoo
