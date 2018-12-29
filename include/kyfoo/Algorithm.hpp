#pragma once

#include <kyfoo/Range.hpp>

namespace kyfoo {

//
// fold

template <typename InputRange, typename Fn>
std::enable_if_t<is_input_range<InputRange>,
typename InputRange::value_type> fold(InputRange r, typename InputRange::value_type seed, Fn&& fn)
{
    for ( ; r; r.popFront() )
        seed = fn(seed, r.front());

    return seed;
}

/**
 * \pre r.size() > 0
 */
template <typename InputRange, typename Fn>
std::enable_if_t<is_input_range<InputRange>,
typename InputRange::value_type> fold(InputRange r, Fn&& fn)
{
    typename InputRange::value_type seed = r.front();
    r.popFront();
    for ( ; r; r.popFront() )
        seed = fn(seed, r.front());

    return seed;
}

//
// foldr

template <typename InputRange, typename Fn>
std::enable_if_t<is_input_range<InputRange>,
typename InputRange::value_type> foldr(InputRange r, typename InputRange::value_type seed, Fn&& fn)
{
    for ( ; r; r.popBack() )
        seed = fn(seed, r.back());

    return seed;
}

/**
 * \pre r.size() > 0
 */
template <typename InputRange, typename Fn>
std::enable_if_t<is_input_range<InputRange>,
typename InputRange::value_type> foldr(InputRange r, Fn&& fn)
{
    typename InputRange::value_type seed = r.back();
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
 && std::is_invocable_v<Fn, typename R1::value_type, typename R2::value_type>,
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
std::enable_if_t<is_input_range<InputRange>,
InputRange> scan(InputRange r, Fn&& fn)
{
    for ( ; r; r.popFront() ) {
        if ( fn(r.front()) )
            break;
    }

    return r;
}

template <typename InputRange>
std::enable_if_t<is_input_range<InputRange>,
InputRange> scan(InputRange r, typename InputRange::value_type target)
{
    return scan(r, [&target](auto t) { return t == target; });
}

//
// indexOf

template <typename InputRange, typename Fn>
std::enable_if_t<
    is_input_range<InputRange>
 && std::is_invocable_v<Fn, typename InputRange::value_type>,
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
uz> indexOf(InputRange r, typename InputRange::value_type target)
{
    return indexOf(r, [&target](auto t) { return t == target; });
}

} // namespace kyfoo
