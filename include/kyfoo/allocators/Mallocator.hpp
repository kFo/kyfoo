#pragma once

#include <kyfoo/Allocators.hpp>

namespace kyfoo::allocators {

class Mallocator
{
public:
    constexpr static uz alignment() { return PlatformAlignment; }

    // uz      goodAllocSize         (uz bytes)           const noexcept
    // Tribool empty                 ()                   const noexcept
    // Tribool owns                  (memv)               const noexcept
    // Tribool resolveInternalPointer(const void*, mems&) const noexcept

public:
    mems allocate(uz bytes) const noexcept;
    mems alignedAllocate(uz bytes, uz a) const noexcept;
    mems allocateZeroed(uz bytes) const noexcept;
    // mems allocateAll() const noexcept;

public:
    //bool expand(mems& m, uz delta) const noexcept;
    bool reallocate(mems& m, uz bytes) const noexcept;
    bool alignedReallocate(mems& m, uz bytes, uz a) const noexcept;

public:
    bool deallocate(mems m) const noexcept;
    bool alignedDeallocate(mems m) const noexcept;
    // bool deallocateAll() const noexcept
};

class AlignedMallocator : private Mallocator
{
public:
    using Mallocator::alignment;

    // uz      goodAllocSize         (uz bytes)           const noexcept
    // Tribool empty                 ()                   const noexcept
    // Tribool owns                  (memv)               const noexcept
    // Tribool resolveInternalPointer(const void*, mems&) const noexcept

public:
    mems allocate(uz bytes) const noexcept
    {
        return alignedAllocate(bytes, alignment());
    }

    using Mallocator::alignedAllocate;

    using Mallocator::allocateZeroed;

    // using Mallocator::allocateAll()

public:
    // using Mallocator::expand

    bool reallocate(mems& m, uz bytes) const noexcept
    {
        return alignedReallocate(m, bytes, alignment());
    }

    using Mallocator::alignedReallocate;

public:
    bool deallocate(mems m) const noexcept
    {
        return alignedDeallocate(m);
    }

    using Mallocator::alignedDeallocate;

    // using Mallocator::deallocateAll
};

} // namespace kyfoo::allocators
