#pragma once

#include <kyfoo/Allocators.hpp>

namespace kyfoo::allocators {

template <typename Primary, typename Fallback>
class FallbackAllocator
{
public:
    static uz alignment() noexcept { return min(Primary::alignment(), Fallback::alignment()); }

    // uz goodAllocSize(uz) const noexcept

    Tribool empty() const noexcept
    {
        return myPrimary.empty() & myFallback.empty();
    }

    Tribool owns(memv m) const noexcept
    {
        return myPrimary.owns(m) | myFallback.owns(m);
    }

    Tribool resolveInternalPointer(void const* p, mems& result) const noexcept
    {
        auto const r = myPrimary.resolveInternalPointer(p, result);
        if ( r == Tribool::False )
            return myFallback.resolveInternalPointer(p, result);

        return r;
    }

public:
    mems allocate(uz bytes) noexcept
    {
        if ( auto ret = myPrimary.allocate(bytes) )
            return ret;

        return myFallback.allocate(bytes);
    }

    mems alignedAllocate(uz bytes, uz a) noexcept
    {
        if ( auto ret = myPrimary.alignedAllocate(bytes, a) )
            return ret;

        return myFallback.alignedAllocate(bytes, a);
    }

    mems allocateAll() noexcept
    {
        if ( auto ret = myPrimary.allocateAll() )
            return ret;

        return myFallback.allocateAll();
    }

public:
    bool expand(mems& m, uz delta) noexcept
    {
        if ( !delta )
            return true;

        if ( !m.data() )
            return false;

        if ( myPrimary.owns(m) )
            return myPrimary.expand(m, delta);

        return myFallback.expand(m, delta);
    }

    bool reallocate(mems& m, uz bytes) noexcept
    {
        auto crossAllocatorMove = [&](auto& from, auto& to) {
            auto tm = to.allocate(bytes);
            if ( tm.card() != bytes )
                return false;

            if ( m.card() < bytes )
                std::memcpy(tm.data(), m.data(), m.card());
            else
                tm = mems(m.data(), bytes);

            from.deallocate(m);
            m = tm;
            return true;
        };

        if ( !m || myPrimary.owns(m) )
            return myPrimary.reallocate(m, bytes)
                || crossAllocatorMove(myPrimary, myFallback);

        return myFallback.reallocate(m, bytes)
            || crossAllocatorMove(myFallback, myPrimary);
    }

    bool alignedReallocate(mems& m, uz bytes, uz a) noexcept
    {
        auto crossAllocatorMove = [&](auto& from, auto& to) {
            auto tm = to.alignedAllocate(bytes, a);
            if ( tm.card() != bytes )
                return false;

            if ( m.card() < bytes )
                std::memcpy(tm.data(), m.data(), m.card());
            else
                tm = mems(m.data(), bytes);

            from.alignedDeallocate(m, a);
            m = tm;
            return true;
        };

        if ( !m || myPrimary.owns(m) )
            return myPrimary.alignedReallocate(m, bytes, a)
                || crossAllocatorMove(myPrimary, myFallback);

        return myFallback.alignedReallocate(m, bytes, a)
            || crossAllocatorMove(myFallback, myPrimary);
    }

public:
    bool deallocate(mems m) noexcept
    {
        if ( myPrimary.owns(m) )
            return myPrimary.deallocate(m);

        return myFallback.deallocate(m);
    }

    bool alignedDeallocate(mems m, uz a) noexcept
    {
        if ( myPrimary.owns(m) )
            return myPrimary.alignedDeallocate(m, a);

        return myFallback.deallocate(m, a);
    }

public:
    Primary myPrimary;
    Fallback myFallback;
};

} // namespace kyfoo::allocators
