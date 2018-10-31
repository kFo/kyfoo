#pragma once

#include <kyfoo/Allocators.hpp>

namespace kyfoo::allocators {

class NullAllocator
{
public:
    static uz alignment() noexcept { return allocationGranularity(); }

    uz      goodAllocSize         (uz)                 const noexcept { return 0; }
    Tribool empty                 ()                   const noexcept { return Tribool::True; }
    Tribool owns                  (memv)               const noexcept { return Tribool::False; }
    Tribool resolveInternalPointer(const void*, mems&) const noexcept { return Tribool::False; }

public:
    mems allocate       (uz)     const noexcept { return {}; }
    mems alignedAllocate(uz, uz) const noexcept { return {}; }
    mems allocateZeroed (uz)     const noexcept { return {}; }
    mems allocateAll    ()       const noexcept { return {}; }

public:
    bool expand           (mems&, uz bytes) const noexcept { return bytes == 0; }
    bool reallocate       (mems&, uz)       const noexcept { return false; }
    bool alignedReallocate(mems&, uz, uz)   const noexcept { return false; }

public:
    bool deallocate       (mems m) const noexcept { return m.data() == nullptr; }
    bool alignedDeallocate(mems m) const noexcept { return m.data() == nullptr; }
    bool deallocateAll    ()       const noexcept { return true; }
};

} // namespace kyfoo::allocators
