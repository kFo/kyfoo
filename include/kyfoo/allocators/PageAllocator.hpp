#pragma once

#include <kyfoo/Allocators.hpp>

namespace kyfoo::allocators {

class PageAllocator
{
public:
    static uz alignment() { return pageSize(); }

    uz goodAllocSize(uz bytes) const noexcept { return roundUpToMultiple(bytes, alignment()); }
    // Tribool empty                 ()                   const noexcept
    // Tribool owns                  (memv)               const noexcept
    // Tribool resolveInternalPointer(const void*, mems&) const noexcept

public:
    mems allocate(uz bytes) const noexcept;
    // mems alignedAllocate(uz bytes, uz a) const noexcept
    mems allocateZeroed(uz bytes) const noexcept;
    // mems allocateAll() const noexcept

public:
    bool deallocate(mems m) const noexcept;
    // bool alignedDeallocate(mems m, uz a) const noexcept
    // bool deallocateAll() const noexcept
};

} // namespace kyfoo::allocators
