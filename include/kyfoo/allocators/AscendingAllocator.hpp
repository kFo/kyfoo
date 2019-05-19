#pragma once

#include <kyfoo/Array.hpp>
#include <kyfoo/Allocators.hpp>

namespace kyfoo::allocators {

template <typename T, typename General>
class AscendingAllocator
{
public:
    explicit AscendingAllocator(uz bytes = 0) noexcept
        : myReserve(bytes)
    {
        if ( myReserve )
            myAllocators.append(myReserve);
    }

    ~AscendingAllocator() noexcept
    {
        deallocateAll();
    }

public:
    static uz alignment() noexcept { return T::alignment(); }

    uz goodAllocSize(uz bytes) const noexcept
    {
        return myAllocators.back().goodAllocSize(bytes);
    }

    Tribool empty() const noexcept
    {
        return Tribool::False; // never surrender
    }

    Tribool owns(memv m) const noexcept
    {
        Tribool ret;
        for ( auto& e : myAllocators )
            if ( ret |= e.owns(m) )
                break;

        return ret;
    }

    Tribool resolveInternalPointer(const void* p, mems& m) const noexcept
    {
        Tribool ret;
        for ( auto& e : myAllocators )
            if ( ret |= e.resolveInternalPointer(p, m) )
                break;

        return ret;
    }

public:
    mems allocate(uz bytes) noexcept
    {
        if ( !myAllocators ) {
            myAllocators.append(myReserve);
            return myAllocators.back().allocate(bytes);
        }

        auto ret = myAllocators.back().allocate(bytes);
        if ( !ret ) {
            myAllocators.append(myReserve);
            return allocate(bytes);
        }

        return ret;
    }

    mems alignedAllocate(uz bytes, uz a) noexcept
    {
        if ( !myAllocators ) {
            myAllocators.append(myReserve);
            return myAllocators.back().alignedAllocate(bytes);
        }

        auto ret = myAllocators.back().alignedAllocate(bytes, a);
        if ( !ret ) {
            myAllocators.append(myReserve);
            return alignedAllocate(bytes, a);
        }

        return ret;
    }

    mems allocateZeroed (uz bytes) noexcept
    {
        auto ret = myAllocators.back().allocateZeroed(bytes);
        if ( !ret ) {
            myAllocators.append(myReserve);
            return allocateZeroed(bytes);
        }

        return ret;
    }

    // mems allocateAll() noexcept

public:
    bool expand(mems& m, uz delta) noexcept
    {
        for ( auto& e : myAllocators )
            if ( e.owns(m) )
                return e.expand(m, delta);

        return false;
    }

    bool reallocate(mems& m, uz bytes) noexcept
    {
        uz index = 0;
        for ( auto& e : myAllocators ) {
            if ( e.owns(m) ) {
                if ( e.reallocate(m, bytes) )
                    return true;

                index = &e - myAllocators.begin();
                break;
            }
        }

        auto tm = allocate(bytes);
        std::memcpy(tm.data(), m.data(), m.card());
        myAllocators[index].deallocate(m);
        m = tm;
        return true;
    }

    bool alignedReallocate(mems& m, uz bytes, uz a) noexcept
    {
        uz index = 0;
        for ( auto& e : myAllocators ) {
            if ( e.owns(m) ) {
                if ( e.alignedReallocate(m, bytes, a) )
                    return true;

                index = &e - myAllocators.begin();
                break;
            }
        }

        auto tm = allocate(bytes);
        std::memcpy(tm.data(), m.data(), m.card());
        myAllocators[index].alignedDeallocate(m);
        m = tm;
        return true;
    }

public:
    // bool deallocate(mems) noexcept

    // bool alignedDeallocate(mems) noexcept

    bool deallocateAll() noexcept
    {
        for ( auto& e : myAllocators )
            e.deallocateAll();

        return true;
    }

private:
    uz myReserve = 0;
    ArrayBuilder<T, General> myAllocators;
};

} // namespace kyfoo::allocators
