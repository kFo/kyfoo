#pragma once

#include <kyfoo/Allocators.hpp>

namespace kyfoo::allocators {

class AscendingPageAllocator
{
public:
    explicit AscendingPageAllocator(uz bytes) noexcept;
    ~AscendingPageAllocator() noexcept;

public:
    uz alignment() const noexcept { return pageSize(); }
    uz goodAllocSize(uz n) const noexcept { return roundUpToMultiple(n, alignment()); }

    Tribool empty() const noexcept { return myBegin == myEnd; }

    Tribool owns(memv m) const noexcept
    {
        return m
            && myBegin <= m.begin() && m.end() <= myLimit;
    }

    // Tribool resolveInternalPointer(const void*, mems&) const noexcept

public:
    mems allocate(uz bytes) noexcept;
    mems alignedAllocate(uz bytes, uz a) noexcept;
    // mems allocateZeroed(uz bytes) noexcept
    // mems allocateAll() noexcept

public:
    bool expand(mems& m, uz delta) noexcept;
    // bool reallocate(mems& m, uz delta) noexcept
    // bool alignedReallocate(mems& m, uz detal, uz a) noexcept

public:
    bool deallocate(mems m) noexcept;
    bool alignedDeallocate(mems m, uz a) noexcept;
    bool deallocateAll() noexcept;

private:
    constexpr uz size() const noexcept { return myEnd - myBegin; }
    constexpr uz reserved() const noexcept { return myLimit - myEnd; }
    constexpr uz capacity() const noexcept { return myLimit - myBegin; }

private:
    u8* myBegin = nullptr;
    u8* myEnd = nullptr;
    u8* myLimit = nullptr;
};

} // namespace kyfoo::allocators
