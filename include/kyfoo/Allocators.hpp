/**
 * Allocators library from Dlang
 * https://dlang.org/phobos/std_experimental_allocator.html
 */

#pragma once

#include <type_traits>

#include <kyfoo/Array.hpp>
#include <kyfoo/Meta.hpp>
#include <kyfoo/Slice.hpp>
#include <kyfoo/Tribool.hpp>
#include <kyfoo/Types.hpp>

namespace kyfoo {

constexpr uz PlatformAlignment = std::alignment_of_v<std::max_align_t>;

uz pageSize() noexcept;
uz allocationGranularity() noexcept;

constexpr uz roundUpToMultiple(uz n, uz m) noexcept
{
    auto const rem = n % m;
    return rem ? n + m - rem : n;
}

constexpr uz roundDownToMultiple(uz n, uz m) noexcept
{
    return n - n % m;
}

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

template <typename T, typename A>           std::enable_if_t< has_method_expand_v<std::remove_reference_t<A>>, bool> tryExpand(A&& alloc, Slice<T> m, uz delta) noexcept { return expand<T>(std::forward<A>(alloc), m, delta); }
template <typename T, typename A> constexpr std::enable_if_t<!has_method_expand_v<std::remove_reference_t<A>>, bool> tryExpand(A&&      , Slice<T>  , uz      ) noexcept { return false; }

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
            if ( tm.length() != bytes )
                return false;

            if ( m.length() < bytes )
                std::memcpy(tm.data(), m.data(), m.length());
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
            if ( tm.length() != bytes )
                return false;

            if ( m.length() < bytes )
                std::memcpy(tm.data(), m.data(), m.length());
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
            myAllocators.append(myReserved);
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
        std::memcpy(tm.data(), m.data(), m.size());
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
        std::memcpy(tm.data(), m.data(), m.size());
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

} // namespace kyfoo
