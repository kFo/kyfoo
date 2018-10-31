/**
 * Allocators library from Dlang
 * https://dlang.org/phobos/std_experimental_allocator.html
 */

#pragma once

#include <type_traits>

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

DEFINE_HAS_METHOD(alignment             , uz                         );
DEFINE_HAS_METHOD(goodAllocSize         , uz, uz                     );
DEFINE_HAS_METHOD(empty                 , Tribool                    );
DEFINE_HAS_METHOD(owns                  , Tribool, memv              );
DEFINE_HAS_METHOD(resolveInternalPointer, Tribool, void const*, mems&);

DEFINE_HAS_METHOD(allocate       , mems, uz    );
DEFINE_HAS_METHOD(alignedAllocate, mems, uz, uz);
DEFINE_HAS_METHOD(allocateZeroed , mems, uz    );
DEFINE_HAS_METHOD(allocateAll    , mems,       );

DEFINE_HAS_METHOD(expand           , bool, mems&, uz    );
DEFINE_HAS_METHOD(reallocate       , bool, mems&, uz    );
DEFINE_HAS_METHOD(alignedReallocate, bool, mems&, uz, uz);

DEFINE_HAS_METHOD(deallocate       , bool, mems);
DEFINE_HAS_METHOD(alignedDeallocate, bool, mems);
DEFINE_HAS_METHOD(deallocateAll    , bool      );

} // namespace allocators

template <typename T, typename A> uz goodAllocSize(A&& alloc, uz n) noexcept { return alloc.goodAllocSize(n * sizeof(T)); }
template <typename T, typename A> Slice<T> allocate(A&& alloc, uz n) { return alloc.allocate(n * sizeof(T)).template cast<T>(); }
template <typename T, typename A> Slice<T> alignedAllocate(A&& alloc, uz n) { return alloc.alignedAllocate(n * sizeof(T)).template cast<T>(); }
template <typename T, typename A> Slice<T> allocateZeroed(A&& alloc, uz n) { return alloc.allocateZeroed(n * sizeof(T)).template cast<T>(); }

template <typename T, typename A>
bool expand(A&& alloc, Slice<T>& m, uz n)
{
    mems block(m.begin(), m.end());
    if ( alloc.expand(block, n * sizeof(T)) ) {
        m = block.cast<T>();
        return true;
    }

    return false;
}

template <typename T, typename A>
bool reallocate(A&& alloc, Slice<T>& m, uz n)
{
    mems block(m);
    if ( alloc.reallocate(block, n * sizeof(T)) ) {
        m = block.cast<T>();
        return true;
    }

    return false;
}

template <typename T, typename A>
bool alignedReallocate(A&& alloc, Slice<T>& m, uz n)
{
    mems block(m);
    if ( alloc.alignedReallocate(block, n * sizeof(T)) ) {
        m = block.cast<T>();
        return true;
    }

    return false;
}

template <typename T, typename A> bool deallocate(A&& alloc, Slice<T> m) { return alloc.deallocate(m); }
template <typename T, typename A> bool alignedDeallocate(A&& alloc, Slice<T> m) { return alloc.alignedDeallocate(m); }

template <typename T, typename A>           std::enable_if_t< allocators::has_method_expand_v<std::remove_reference_t<A>>, bool> tryExpand(A&& alloc, Slice<T> m, uz delta) noexcept { return expand<T>(std::forward<A>(alloc), m, delta); }
template <typename T, typename A> constexpr std::enable_if_t<!allocators::has_method_expand_v<std::remove_reference_t<A>>, bool> tryExpand(A&&      , Slice<T>  , uz      ) noexcept { return false; }

} // namespace kyfoo
