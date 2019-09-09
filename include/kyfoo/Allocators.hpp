/**
 * Allocators library from Dlang
 * https://dlang.org/phobos/std_experimental_allocator.html
 */

#pragma once

#include <type_traits>

#include <kyfoo/Math.hpp>
#include <kyfoo/Meta.hpp>
#include <kyfoo/Slice.hpp>
#include <kyfoo/Tribool.hpp>
#include <kyfoo/Types.hpp>

namespace kyfoo {

constexpr uz PlatformAlignment = std::alignment_of_v<std::max_align_t>;

uz pageSize() noexcept;
uz allocationGranularity() noexcept;

template <typename T>
constexpr T* alignedUp(T* p, uz a) noexcept
{
    return reinterpret_cast<T*>(roundUpToMultiple(reinterpret_cast<uz>(p), a));
}

template <typename T>
constexpr T* alignedDown(T* p, uz a) noexcept
{
    return reinterpret_cast<T*>(roundDownToMultiple(reinterpret_cast<uz>(p), a));
}

using mems = Slice<void>;
using memv = Slice<void const>;

namespace allocators {

DEFINE_HAS_METHOD_SIG(alignment             , uz                         );
DEFINE_HAS_METHOD_SIG(goodAllocSize         , uz, uz                     );
DEFINE_HAS_METHOD_SIG(empty                 , Tribool                    );
DEFINE_HAS_METHOD_SIG(owns                  , Tribool, memv              );
DEFINE_HAS_METHOD_SIG(resolveInternalPointer, Tribool, void const*, mems&);

DEFINE_HAS_METHOD_SIG(allocate       , mems, uz    );
DEFINE_HAS_METHOD_SIG(alignedAllocate, mems, uz, uz);
DEFINE_HAS_METHOD_SIG(allocateZeroed , mems, uz    );
DEFINE_HAS_METHOD_SIG(allocateAll    , mems        );

DEFINE_HAS_METHOD_SIG(expand           , bool, mems&, uz    );
DEFINE_HAS_METHOD_SIG(reallocate       , bool, mems&, uz    );
DEFINE_HAS_METHOD_SIG(alignedReallocate, bool, mems&, uz, uz);

DEFINE_HAS_METHOD_SIG(deallocate       , bool, mems);
DEFINE_HAS_METHOD_SIG(alignedDeallocate, bool, mems);
DEFINE_HAS_METHOD_SIG(deallocateAll    , bool      );

} // namespace allocators

template <typename T, typename A> uz goodAllocSize(A&& alloc, uz n) noexcept { return alloc.goodAllocSize(n * sizeof(T)); }
template <typename T, typename A> Slice<T> allocate(A&& alloc, uz n) { return alloc.allocate(n * sizeof(T)).template cast<T>(); }
template <typename T, typename A> Slice<T> alignedAllocate(A&& alloc, uz n) { return alloc.alignedAllocate(n * sizeof(T)).template cast<T>(); }
template <typename T, typename A> Slice<T> allocateZeroed(A&& alloc, uz n) { return alloc.allocateZeroed(n * sizeof(T)).template cast<T>(); }

template <typename T, typename A>
std::enable_if_t<!std::is_const_v<T>,
bool> expand(A&& alloc, Slice<T>& m, uz n)
{
    auto block = type_erase(m);
    if ( alloc.expand(block, n * sizeof(T)) ) {
        m = block.cast<T>();
        return true;
    }

    return false;
}

template <typename T, typename A>
std::enable_if_t<!std::is_const_v<T>,
bool> reallocate(A&& alloc, Slice<T>& m, uz n)
{
    auto block = type_erase(m);
    if ( alloc.reallocate(block, n * sizeof(T)) ) {
        m = block.cast<T>();
        return true;
    }

    return false;
}

template <typename T, typename A>
std::enable_if_t<!std::is_const_v<T>,
bool> alignedReallocate(A&& alloc, Slice<T>& m, uz n)
{
    auto block = type_erase(m);
    if ( alloc.alignedReallocate(block, n * sizeof(T)) ) {
        m = block.cast<T>();
        return true;
    }

    return false;
}

template <typename T, typename A> bool deallocate(A&& alloc, Slice<T> m) { return alloc.deallocate(m); }
template <typename T, typename A> bool alignedDeallocate(A&& alloc, Slice<T> m) { return alloc.alignedDeallocate(m); }

template <typename T, typename A>           std::enable_if_t< allocators::has_method_expand<std::remove_reference_t<A>>, bool> tryExpand(A&& alloc, Slice<T> m, uz delta) noexcept { return expand<T>(std::forward<A>(alloc), m, delta); }
template <typename T, typename A> constexpr std::enable_if_t<!allocators::has_method_expand<std::remove_reference_t<A>>, bool> tryExpand(A&&      , Slice<T>  , uz      ) noexcept { return false; }

} // namespace kyfoo
