#pragma once

#include <kyfoo/Allocators.hpp>

#include <kyfoo/allocators/NullAllocator.hpp>

namespace kyfoo::allocators {

template <typename ParentAllocator = NullAllocator,
          uz minAlign = PlatformAlignment>
class Region
{
public:
    explicit Region(mems store) noexcept
        : myParent(store.length())
    {
        init(store);
    }

    explicit Region(uz bytes) noexcept
        : myParent(bytes)
    {
        init(myParent.allocate(goodAllocSize(bytes)));
    }

    Region(ParentAllocator parent, uz bytes) noexcept
        : myParent(parent)
    {
        init(myParent.allocate(goodAllocSize(bytes)));
    }

    ~Region() noexcept
    {
        myParent.deallocate(mems(myBegin, myLimit));
    }

public:
    constexpr static uz alignment() noexcept { return minAlign; }

    constexpr uz goodAllocSize(uz bytes) const noexcept
    {
        return roundUpToMultiple(bytes, alignment());
    }

    constexpr Tribool empty() const noexcept
    {
        return myEnd = alignedBegin();
    }

    constexpr Tribool owns(memv m) const noexcept
    {
        return myBegin <= m.begin() && m.end() <= myLimit;
    }

    // Tribool resolveInternalPointer(const void*, mems&) const noexcept

public:
    mems allocate(uz bytes) noexcept
    {
        auto const goodSize = goodAllocSize(bytes);
        if ( !bytes || goodSize < bytes || reserved() < goodSize )
            return {};

        mems ret(myEnd, bytes);
        myEnd += goodSize;
        return ret;
    }

    mems alignedAllocate(uz bytes) noexcept
    {
        auto const goodSize = goodAllocSize(bytes);
        if ( !bytes || goodSize < bytes || reserved() < goodSize )
            return {};

        auto newEnd = alignUp(myEnd, a);
        if ( newEnd < myEnd || newEnd > myLimit )
            return {};

        auto save = myEnd;
        myEnd = newEnd;
        if ( auto ret = allocate(n) )
            return ret;

        myEnd = save;
        return {};
    }

    // mems allocateZeroed(uz bytes) noexcept

    mems allocateAll() noexcept
    {
        mems ret(myEnd, reserved());
        myEnd = myLimit;
        return ret;
    }

public:
    bool expand(mems& m, uz delta) noexcept
    {
        if ( !m || !delta )
            return !delta;

        auto newLength = m.length() + delta;
        if ( myEnd < m.end() + alignment ) {
            auto const currentGoodSize = goodAllocSize(m.length());
            auto const newGoodSize = goodAllocSize(newLength);
            auto const goodDelta = newGoodSize - currentGoodSize;
            if ( goodDelta == 0 || allocate(goodDelta).length() == goodDelta ) {
                m = mems(m.data(), newLength);
                return true;
            }
        }

        return false;
    }

    // bool reallocate(mems& m, uz delta) noexcept
    // bool alignedReallocate(mems& m, uz delta) noexcept

public:
    bool deallocate(mems m) noexcept
    {
        auto const goodSize = goodAllocSize(m.length());
        if ( m.data() + goodSize == myEnd ) {
            myEnd = m.data();
            return true;
        }

        return false;
    }

    // bool alignedDeallocate(mems m) noexcept

    bool deallocateAll() noexcept
    {
        myEnd = alignedBegin();
        return true;
    }

private:
    void init(mems m)
    {
        myBegin = reinterpret_cast<u8*>(m.begin());
        myEnd = alignedBegin();
        myLimit = reinterpret_cast<u8*>(m.end());
    }

    constexpr u8* alignedBegin() const noexcept
    {
        return alignedUp(myBegin, alignment());
    }

    constexpr u8* alignedLimit() const noexcept
    {
        return alignedDown(myLimit, alignment());
    }

    constexpr uz size() const noexcept
    {
        return myEnd - myBegin;
    }

    constexpr uz reserved() const noexcept
    {
        return myLimit - myEnd;
    }

    constexpr uz capacity() const noexcept
    {
        return myLimit - myBegin;
    }

private:
    ParentAllocator myParent;
    u8* myBegin;
    u8* myEnd;
    u8* myLimit;
};

} // namespace kyfoo::allocators
