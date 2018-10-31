#pragma once

#include <type_traits>
#include <utility>

#define DEFINE_PLACEHOLDER_UNARY_OPS(X) \
    X(Plus  , + ) \
    X(Minus , - ) \
    X(Deref , * ) \
    X(BitNot, ~ ) \
    X(LogNot, ! ) \
    X(PreInc, ++) \
    X(PreDec, --)

#define DEFINE_PLACEHOLDER_UNARY_POST_OPS(X) \
    X(PostInc, ++) \
    X(PostDec, --)

#define DEFINE_PLACEHOLDER_BINARY_OPS(X) \
    X(Add   , + ) \
    X(Sub   , - ) \
    X(Mul   , * ) \
    X(Div   , / ) \
    X(Rem   , % ) \
    X(BitXor, ^ ) \
    X(BitAnd, & ) \
    X(BitOr , | ) \
    X(Shl   , <<) \
    X(Shr   , >>) \
    X( Eq   , ==) \
    X( Ne   , !=) \
    X( Lt   , < ) \
    X( Le   , <=) \
    X( Gt   , > ) \
    X( Ge   , >=)

#define DEFINE_PLACEHOLDER_BINARY_ASSIGN_OPS(X) \
    X(Assign      , =  ) \
    X(AddAssign   , += ) \
    X(SubAssign   , -= ) \
    X(MulAssign   , *= ) \
    X(DivAssign   , /= ) \
    X(RemAssign   , %= ) \
    X(BitXorAssign, ^= ) \
    X(BitAndAssign, &= ) \
    X(BitOrAssign , |= ) \
    X(ShlAssign   , <<=) \
    X(ShrAssign   , >>=)

namespace kyfoo {

    namespace placeholders {

#define X(a,b) struct Unary_##a { template <typename T> static decltype(auto) op(T&& rhs) noexcept { return b rhs; } };
DEFINE_PLACEHOLDER_UNARY_OPS(X)
#undef X

#define X(a,b) struct Unary_##a { template <typename T> static decltype(auto) op(T&& rhs) noexcept { return rhs b; } };
DEFINE_PLACEHOLDER_UNARY_POST_OPS(X)
#undef X

#define X(a,b) struct Binary_##a { template <typename L, typename R> static decltype(auto) op(L&& lhs, R&& rhs) noexcept { return lhs b rhs; } };
DEFINE_PLACEHOLDER_BINARY_OPS(X)
#undef X

#define X(a,b) struct Binary_##a { template <typename L, typename R> static decltype(auto) op(L& lhs, R&& rhs) noexcept { return lhs b rhs; } };
DEFINE_PLACEHOLDER_BINARY_ASSIGN_OPS(X)
#undef X

struct Binary_Index { template <typename L, typename R> static decltype(auto) op(L&& lhs, R&& rhs) noexcept { return lhs[rhs]; } };

template <unsigned N>                          struct Placeholder;
template <typename T>                          struct Capture;
template <typename Op, typename T>             struct Unary;
template <typename Op, typename L, typename R> struct Binary;

template <typename T> struct is_placeholder                        : public std::false_type {};
template <unsigned N> struct is_placeholder<Placeholder<N>       > : public std::true_type  {};
template <unsigned N> struct is_placeholder<Placeholder<N> const > : public std::true_type  {};
template <unsigned N> struct is_placeholder<Placeholder<N>&      > : public std::true_type  {};
template <unsigned N> struct is_placeholder<Placeholder<N> const&> : public std::true_type  {};
template <typename T> constexpr bool is_placeholder_v = is_placeholder<T>::value;

template <typename T> struct is_capture                    : public std::false_type {};
template <typename T> struct is_capture<Capture<T>       > : public std::true_type  {};
template <typename T> struct is_capture<Capture<T> const > : public std::true_type  {};
template <typename T> struct is_capture<Capture<T>&      > : public std::true_type  {};
template <typename T> struct is_capture<Capture<T> const&> : public std::true_type  {};
template <typename T> constexpr bool is_capture_v = is_capture<T>::value;

template <typename T>              struct is_unary                     : public std::false_type {};
template <typename Op, typename T> struct is_unary<Unary<Op,T>>        : public std::true_type  {};
template <typename Op, typename T> struct is_unary<Unary<Op,T> const>  : public std::true_type  {};
template <typename Op, typename T> struct is_unary<Unary<Op,T>&>       : public std::true_type  {};
template <typename Op, typename T> struct is_unary<Unary<Op,T> const&> : public std::true_type  {};
template <typename T> constexpr bool is_unary_v = is_unary<T>::value;

template <typename T>                          struct is_binary                        : public std::false_type {};
template <typename Op, typename L, typename R> struct is_binary<Binary<Op,L,R>>        : public std::true_type  {};
template <typename Op, typename L, typename R> struct is_binary<Binary<Op,L,R> const>  : public std::true_type  {};
template <typename Op, typename L, typename R> struct is_binary<Binary<Op,L,R>&>       : public std::true_type  {};
template <typename Op, typename L, typename R> struct is_binary<Binary<Op,L,R> const&> : public std::true_type  {};
template <typename T> constexpr bool is_binary_v = is_binary<T>::value;

template <typename T> constexpr bool is_placeholder_kind_v =
       is_placeholder_v<T>
    || is_capture_v<T>
    || is_unary_v<T>
    || is_binary_v<T>;

template <typename L, typename R> constexpr bool needs_capture_left_v  = !is_placeholder_kind_v<L> &&  is_placeholder_kind_v<R>;
template <typename L, typename R> constexpr bool needs_capture_right_v =  is_placeholder_kind_v<L> && !is_placeholder_kind_v<R>;

template <typename Derived>
struct Operators
{
#define X(a,b) \
    Unary<Unary_##a,       Derived> operator b ()      ; \
    Unary<Unary_##a, const Derived> operator b () const;
DEFINE_PLACEHOLDER_UNARY_OPS(X)
#undef X

#define X(a,b) \
    Unary<Unary_##a,       Derived> operator b (int)      ; \
    Unary<Unary_##a, const Derived> operator b (int) const;
DEFINE_PLACEHOLDER_UNARY_POST_OPS(X)
#undef X

#define X(a,b) \
    template <typename T> std::enable_if_t<!is_placeholder_kind_v<T>, Binary<Binary_##a, Derived, Capture<T>>> operator b (T&& rhs); \
    template <typename T> std::enable_if_t< is_placeholder_kind_v<T>, Binary<Binary_##a, Derived, T         >> operator b (T&& rhs);
DEFINE_PLACEHOLDER_BINARY_ASSIGN_OPS(X)
#undef X

    template <typename T> std::enable_if_t<!is_placeholder_kind_v<T>, Binary<Binary_Index, Derived, Capture<T>>> operator[](T&& rhs);
    template <typename T> std::enable_if_t< is_placeholder_kind_v<T>, Binary<Binary_Index, Derived, T         >> operator[](T&& rhs);

    template <typename T> std::enable_if_t<!is_placeholder_kind_v<T>, Binary<Binary_Index, const Derived, Capture<T>>> operator[](T&& rhs) const;
    template <typename T> std::enable_if_t< is_placeholder_kind_v<T>, Binary<Binary_Index, const Derived, T         >> operator[](T&& rhs) const;

    Derived*       dthis()       { return static_cast<Derived      *>(this); }
    Derived const* dthis() const { return static_cast<Derived const*>(this); }
};

template <unsigned N, typename Head, typename... Tail>
decltype(auto) choose(Head&& head, Tail&&... tail)
{
    if constexpr(N == 0)
        return std::forward<Head>(head);
    else
        return choose<N-1>(std::forward<Tail>(tail)...);
}

template <unsigned N>
struct Placeholder : public Operators<Placeholder<N>>
{
    using Base = Operators<Placeholder<N>>;

    using Base::operator =;

    template <typename... Args>
    decltype(auto) operator()(Args&&... args) const noexcept
    {
        return choose<N>(std::forward<Args>(args)...);
    }

    template <typename... Args>
    decltype(auto) operator()(Args&&... args) noexcept
    {
        return choose<N>(std::forward<Args>(args)...);
    }
};

template <typename T>
struct Capture
{
    static_assert(!is_placeholder_kind_v<T>);
    T capture;

    template <typename... Args>
    T operator()(Args&&...) const noexcept
    {
        return capture;
    }

    template <typename... Args>
    T operator()(Args&&...) noexcept
    {
        return capture;
    }
};

template <typename Op, typename T>
struct Unary : public Operators<Unary<Op, T>>
{
    using Base = Operators<Unary<Op, T>>;

    using Base::operator =;

    T expr;

    template <typename U>
    /*implicit*/ Unary(U&& rhs)
        : expr(std::forward<U>(rhs))
    {
    }

    template <typename... Args>
    decltype(auto) operator()(Args&&... args) noexcept
    {
        return Op::op(expr(std::forward<Args>(args)...));
    }

    template <typename... Args>
    decltype(auto) operator()(Args&&... args) const noexcept
    {
        return Op::op(expr(std::forward<Args>(args)...));
    }
};

template <typename Op, typename L, typename R>
struct Binary : public Operators<Binary<Op, L, R>>
{
    using Base = Operators<Binary<Op, L, R>>;

    using Base::operator =;

    L lhs;
    R rhs;

    template <typename T, typename U>
    /*implicit*/ Binary(T&& l, U&& r)
        : lhs(std::forward<T>(l))
        , rhs(std::forward<U>(r))
    {
    }

    template <typename... Args>
    decltype(auto) operator()(Args&&... args) noexcept
    {
        return Op::op(lhs(std::forward<Args>(args)...),
                      rhs(std::forward<Args>(args)...));
    }

    template <typename... Args>
    decltype(auto) operator()(Args&&... args) const noexcept
    {
        return Op::op(lhs(std::forward<Args>(args)...),
                      rhs(std::forward<Args>(args)...));
    }
};

#define X(a,b) \
    template <typename Derived> Unary<Unary_##a,       Derived> Operators<Derived>::operator b ()       { return { *dthis() }; } \
    template <typename Derived> Unary<Unary_##a, const Derived> Operators<Derived>::operator b () const { return { *dthis() }; }
DEFINE_PLACEHOLDER_UNARY_OPS(X)
#undef X

#define X(a,b) \
    template <typename Derived> Unary<Unary_##a,       Derived> Operators<Derived>::operator b (int)       { return { *dthis() }; } \
    template <typename Derived> Unary<Unary_##a, const Derived> Operators<Derived>::operator b (int) const { return { *dthis() }; }
DEFINE_PLACEHOLDER_UNARY_POST_OPS(X)
#undef X

#define X(a,b) \
    template <typename Derived> template <typename T> std::enable_if_t<!is_placeholder_kind_v<T>, Binary<Binary_##a, Derived, Capture<T>>> Operators<Derived>::operator b (T&& rhs) { return { *dthis(), Capture<T>{rhs} }; } \
    template <typename Derived> template <typename T> std::enable_if_t< is_placeholder_kind_v<T>, Binary<Binary_##a, Derived, T         >> Operators<Derived>::operator b (T&& rhs) { return { *dthis(), rhs             }; }
DEFINE_PLACEHOLDER_BINARY_ASSIGN_OPS(X)
#undef X

template <typename Derived> template <typename T> std::enable_if_t<!is_placeholder_kind_v<T>, Binary<Binary_Index, Derived, Capture<T>>> Operators<Derived>::operator[](T&& rhs) { return { *dthis(), Capture<T>{rhs} }; }
template <typename Derived> template <typename T> std::enable_if_t< is_placeholder_kind_v<T>, Binary<Binary_Index, Derived, T         >> Operators<Derived>::operator[](T&& rhs) { return { *dthis(), rhs             }; }

template <typename Derived> template <typename T> std::enable_if_t<!is_placeholder_kind_v<T>, Binary<Binary_Index, const Derived, Capture<T>>> Operators<Derived>::operator[](T&& rhs) const { return { *dthis(), Capture<T>{rhs} }; }
template <typename Derived> template <typename T> std::enable_if_t< is_placeholder_kind_v<T>, Binary<Binary_Index, const Derived, T         >> Operators<Derived>::operator[](T&& rhs) const { return { *dthis(), rhs             }; }

#define X(a,b) \
    template <typename L, typename R> std::enable_if_t<is_placeholder_kind_v<L> && is_placeholder_kind_v<R>, placeholders::Binary<placeholders::Binary_##a, L, R>> operator b (L&& lhs, R&& rhs) noexcept { return { std::forward<L>(lhs), std::forward<R>(rhs) }; } \
    template <typename L, typename R> std::enable_if_t<needs_capture_left_v<L,R>,  Binary<Binary_##a, Capture<L>, R>> operator b (L&& lhs, R&& rhs) noexcept { return { Capture<L>{lhs}, std::forward<R>(rhs) }; }                                                   \
    template <typename L, typename R> std::enable_if_t<needs_capture_right_v<L,R>, Binary<Binary_##a, L, Capture<R>>> operator b (L&& lhs, R&& rhs) noexcept { return { std::forward<L>(lhs), Capture<R>{rhs} }; }
DEFINE_PLACEHOLDER_BINARY_OPS(X)
#undef X

    } // namespace placeholders

constexpr placeholders::Placeholder<0> $;
constexpr placeholders::Placeholder<0> $a;
constexpr placeholders::Placeholder<1> $b;
constexpr placeholders::Placeholder<2> $c;

inline static placeholders::Placeholder<0> $$;
inline static placeholders::Placeholder<0> $$a;
inline static placeholders::Placeholder<1> $$b;
inline static placeholders::Placeholder<2> $$c;

} // namespace kyfoo
