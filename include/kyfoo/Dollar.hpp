#pragma once

namespace kyfoo {

struct Identity
{
    template <typename T>
    auto operator()(T&& rhs) const noexcept { return rhs; }
};

template <typename Op, typename L, typename R>
struct BindLeft {
    L l;
    R r;

    template <typename T>
    auto operator()(T&& rhs) noexcept { return Op::op(l, r(rhs)); }
};

template <typename Op, typename L, typename R>
struct BindRight {
    L l;
    R r;

    template <typename T>
    auto operator()(T&& lhs) noexcept { return Op::op(l(lhs), r); }
};

#define DOLLAR_DEFINE_OPS(X) \
    X(Add, +) \
    X(Sub, -) \
    X(Mul, *) \
    X(Div, /) \
    X(Rem, %) \
    X(Eq, ==) \
    X(Ne, !=) \
    X(Lt, <)  \
    X(Le, <=) \
    X(Gt, >)  \
    X(Ge, >=)

#define X(a,b)                                                                                                                \
    struct a { template <typename L, typename R> static auto op(L const& lhs, R const& rhs) noexcept { return lhs b rhs; } };          \
    template <typename L> BindLeft <a, L       , Identity> operator b(L&& lhs     , Identity rhs) noexcept { return {lhs, rhs}; } \
    template <typename R> BindRight<a, Identity, R       > operator b(Identity lhs, R&& rhs     ) noexcept { return {lhs, rhs}; }

DOLLAR_DEFINE_OPS(X)
#undef X

inline Identity $;

} // namespace kyfoo
