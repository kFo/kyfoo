#pragma once

#include <type_traits>

#include <kyfoo/Intrinsics.hpp>
#include <kyfoo/Types.hpp>

namespace kyfoo {

template <typename T>
constexpr std::enable_if_t<std::is_signed_v<T>,
T> abs(T rhs) noexcept
{
    return rhs < 0 ? -rhs : rhs;
}

template <typename T>
constexpr std::enable_if_t<std::is_unsigned_v<T>,
T> abs(T rhs) noexcept
{
    return rhs;
}

template <typename T>
constexpr std::enable_if_t<std::is_integral_v<T>,
T> gcd_euclid(T x, T y) noexcept
{
    while ( y ) {
        auto const t = y;
        y = x % y;
        x = t;
    }

    return x;
}

template <typename T>
constexpr decltype(intrinsics::ctz(std::declval<T>())) gcd_stein(T x, T y)
{
    if ( !x ) return y;
    if ( !y ) return x;

    unsigned const pow2 = intrinsics::ctz(x | y);
    x >>= intrinsics::ctz(x);
    do {
        y >>= intrinsics::ctz(y);
        if ( x > y )
            swap(x, y);
        y -= x;
    } while ( y );
    return x << pow2;
}

template <typename T>
constexpr std::enable_if_t<intrinsics::has_ctz_v<T, T>,
T> gcd(T x, T y) noexcept
{
    return gcd_stein(x, y);
}

template <typename T>
constexpr std::enable_if_t<!intrinsics::has_ctz_v<T, T>,
T> gcd(T x, T y) noexcept
{
    return gcd_euclid(x, y);
}

template <typename T>
constexpr T lcm(T x, T y) noexcept
{
    auto const xx = abs(x);
    auto const yy = abs(y);
    return (xx / gcd(xx, yy)) * yy;
}

template <typename T>
constexpr std::enable_if_t<std::is_unsigned_v<T> && !intrinsics::has_clz_v<T, T>,
T> log2(T n) noexcept
{
    T ret = 0;
    while ( n >>= 1 )
        ++ret;
    return ret;
}

template <typename T>
constexpr std::enable_if_t<std::is_unsigned_v<T> && intrinsics::has_clz_v<T, T>,
T> log2(T x) noexcept
{
    return (sizeof(T)*8 - 1) - intrinsics::clz(x);
}

template <typename T>
constexpr std::enable_if_t<std::is_unsigned_v<T> && intrinsics::has_clz_v<T, T>,
T> roundUpToPow2(T x) noexcept
{
    return (T(1) << (sizeof(T) * 8 - 1)) >> (intrinsics::clz(x - 1) - 1);
}

template <typename T>
constexpr std::enable_if_t<std::is_integral_v<T>,
T> roundUpToMultiple(T n, T m) noexcept
{
    auto const rem = n % m;
    return rem ? n + m - rem : n;
}

template <typename T>
constexpr std::enable_if_t<std::is_integral_v<T>,
T> roundDownToMultiple(T n, T m) noexcept
{
    return n - n % m;
}

} // namespace kyfoo
