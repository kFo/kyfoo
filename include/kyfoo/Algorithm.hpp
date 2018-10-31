#pragma once

#include <kyfoo/Range.hpp>

namespace kyfoo {

//
// fold

template <typename Fn, typename InputRange>
std::enable_if_t<is_input_range<InputRange>,
typename InputRange::value_type> fold(Fn&& fn, typename InputRange::value_type seed, InputRange r)
{
    for ( ; r; r.popFront() )
        seed = fn(seed, r.front());

    return seed;
}

/**
 * \pre r.size() > 0
 */
template <typename Fn, typename InputRange>
std::enable_if_t<is_input_range<InputRange>,
typename InputRange::value_type> fold(Fn&& fn, InputRange r)
{
    typename InputRange::value_type seed = r.front();
    r.popFront();
    for ( ; r; r.popFront() )
        seed = fn(seed, r.front());

    return seed;
}

//
// foldr

template <typename Fn, typename InputRange>
std::enable_if_t<is_input_range<InputRange>,
typename InputRange::value_type> foldr(Fn&& fn, typename InputRange::value_type seed, InputRange r)
{
    for ( ; r; r.popBack() )
        seed = fn(seed, r.back());

    return seed;
}

/**
 * \pre r.size() > 0
 */
template <typename Fn, typename InputRange>
std::enable_if_t<is_input_range<InputRange>,
typename InputRange::value_type> foldr(Fn&& fn, InputRange r)
{
    typename InputRange::value_type seed = r.back();
    r.popBack();
    for ( ; r; r.popBack() )
        seed = fn(seed, r.back());

    return seed;
}

//
// equal

template <typename InputRange>
std::enable_if_t<is_input_range<InputRange>,
bool> equal(InputRange lhs, InputRange rhs)
{
    for ( ; lhs && rhs; lhs.popFront(), rhs.popFront() )
        if ( !(lhs.front() == rhs.front()) )
            return false;

    return true;
}

} // namespace kyfoo
