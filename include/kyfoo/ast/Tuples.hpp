#pragma once

#include <kyfoo/String.hpp>
#include <kyfoo/lexer/TokenKind.hpp>

namespace kyfoo::ast {

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

const char* presentTupleOpen(TupleKind kind);
const char* presentTupleClose(TupleKind kind);
const char* presentTupleWeave(TupleKind kind);

} // namespace kyfoo::ast
