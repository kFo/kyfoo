#pragma once

#include <kyfoo/Range.hpp>

namespace kyfoo {

template <typename T>
constexpr T succ(T rhs) noexcept { return ++rhs; }

template <typename T>
constexpr T succ(T rhs, unsigned n) noexcept { return rhs + n; }

template <typename T>
constexpr T pred(T rhs) noexcept { return --rhs; }

template <typename T>
constexpr T pred(T rhs, unsigned n) noexcept { return rhs - n; }

template <typename T>
constexpr void inc(T&& rhs) noexcept { ++rhs; }

template <typename T>
constexpr void inc(T&& rhs, unsigned n) { rhs += n; }

template <typename T>
constexpr void dec(T&& rhs) noexcept { --rhs; }

template <typename T>
constexpr void dec(T&& rhs, unsigned n) noexcept { rhs += n; }

template <typename T>
constexpr uz card(T const* begin, T const* end) noexcept { return end - begin; }

//
// fold

template <typename InputRange, typename Seed, typename Fn>
std::enable_if_t<is_input_range<InputRange>,
Seed> fold(InputRange r, Seed seed, Fn&& fn)
{
    for ( ; r; r.popFront() )
        seed = fn(seed, r.front());

    return seed;
}

/**
 * \pre r.card() > 0
 */
template <typename InputRange, typename Fn>
std::enable_if_t<is_input_range<InputRange>,
typename InputRange::Element> fold(InputRange r, Fn&& fn)
{
    InputRange::Element seed = r.front();
    r.popFront();
    for ( ; r; r.popFront() )
        seed = fn(seed, r.front());

    return seed;
}

//
// foldr

template <typename InputRange, typename Fn>
std::enable_if_t<is_input_range<InputRange>,
typename InputRange::Element> foldr(InputRange r, typename InputRange::Element seed, Fn&& fn)
{
    for ( ; r; r.popBack() )
        seed = fn(seed, r.back());

    return seed;
}

/**
 * \pre r.card() > 0
 */
template <typename InputRange, typename Fn>
std::enable_if_t<is_input_range<InputRange>,
typename InputRange::Element> foldr(InputRange r, Fn&& fn)
{
    typename InputRange::Element seed = r.back();
    r.popBack();
    for ( ; r; r.popBack() )
        seed = fn(seed, r.back());

    return seed;
}

//
// equal

template <typename R1, typename R2, typename Fn>
std::enable_if_t<
    is_input_range<R1>
 && is_input_range<R2>
 && std::is_invocable_v<Fn, typename R1::Element, typename R2::Element>,
bool> equal(R1 lhs, R2 rhs, Fn&& fn)
{
    for ( ; lhs && rhs; lhs.popFront(), rhs.popFront() )
        if ( !fn(lhs.front(), rhs.front()) )
            return false;

    return true;
}

template <typename R1, typename R2>
std::enable_if_t<is_input_range<R1> && is_input_range<R2>,
bool> equal(R1 lhs, R2 rhs)
{
    return equal(lhs, rhs, [](auto l, auto r) { return l == r; });
}

//
// scan

template <typename InputRange, typename Fn>
std::enable_if_t<
    is_input_range<InputRange>
 && std::is_invocable_v<Fn, typename InputRange::ConstReference>,
InputRange> scan(InputRange&& r, Fn&& fn)
{
    for ( ; r; r.popFront() ) {
        if ( fn(r.front()) )
            break;
    }

    return r;
}

template <typename Container, typename Fn>
std::enable_if_t<
    !is_input_range<Container>
 && std::is_invocable_v<Fn, typename Container::View::ConstReference>,
typename Container::View> scan(Container& c, Fn&& fn)
{
    return scan(view(c), std::forward<Fn>(fn));
}

template <typename InputRange>
std::enable_if_t<
    is_input_range<InputRange>,
InputRange> scan(InputRange r, typename InputRange::ConstReference target)
{
    return scan(r, [&target](auto t) { return t == target; });
}

template <typename Container>
std::enable_if_t<
    !is_input_range<Container>,
typename Container::View> scan(Container& c, typename Container::View::ConstReference target)
{
    return scan(view(c), [&target](auto t) { return t == target; });
}

//
// indexOf

template <typename InputRange, typename Fn>
std::enable_if_t<
    is_input_range<InputRange>
 && std::is_invocable_v<Fn, typename InputRange::Element>,
uz> indexOf(InputRange r, Fn&& fn)
{
    uz i = 0;
    for ( ; r; r.popFront(), ++i ) {
        if ( fn(r.front()) )
            break;
    }

    return i;
}

template <typename InputRange>
std::enable_if_t<is_input_range<InputRange>,
uz> indexOf(InputRange r, typename InputRange::Element target)
{
    return indexOf(r, [&target](auto t) { return t == target; });
}

} // namespace kyfoo
