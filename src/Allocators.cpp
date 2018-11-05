#include <kyfoo/Allocators.hpp>

#include <kyfoo/allocators/AscendingPageAllocator.hpp>
#include <kyfoo/allocators/Mallocator.hpp>
#include <kyfoo/allocators/PageAllocator.hpp>

#include <malloc.h>

#define NOMINMAX
#define VC_EXTRALEAN
#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
#undef WIN32_LEAN_AND_MEAN
#undef VC_EXTRALEAN
#undef NOMINMAX

namespace kyfoo {

namespace win32 {
    ::SYSTEM_INFO g_systemInfo;

    struct static_init {
        static_init()
        {
            ::GetSystemInfo(&g_systemInfo);
        }
    } g_static_init;

    bool extendCommitLimit(void* begin, uz size) noexcept
    {
        return ::VirtualAlloc(begin, size, MEM_COMMIT, PAGE_READWRITE) != NULL;
    }

    bool extendCommitLimit(void* begin, void* end) noexcept
    {
        auto size = Slice(begin, end).card();
        return extendCommitLimit(begin, size);
    }
} // namespace win32

uz pageSize() noexcept
{
    return win32::g_systemInfo.dwPageSize;
}

uz allocationGranularity() noexcept
{
    return win32::g_systemInfo.dwAllocationGranularity;
}

    namespace allocators {

//
// Mallocator

mems Mallocator::allocate(uz bytes) const noexcept
{
    if ( bytes )
        if ( auto p = std::malloc(bytes) )
            return { p, bytes };

    return {};
}

mems Mallocator::alignedAllocate(uz bytes, uz a) const noexcept
{
    if ( auto p = _aligned_malloc(bytes, a) )
        return { p, bytes };

    return {};
}

mems Mallocator::allocateZeroed(uz bytes) const noexcept
{
    if ( !bytes )
        return {};

    if ( auto p = std::calloc(1, bytes) )
        return { p, bytes };

    return {};
}

bool Mallocator::reallocate(mems& m, uz bytes) const noexcept
{
    if ( !bytes ) {
        if ( deallocate(m) ) {
            m = {};
            return true;
        }

        return false;
    }

    if ( auto p = std::realloc(m.data(), bytes) ) {
        m = mems(p, bytes);
        return true;
    }

    return false;
}

bool Mallocator::alignedReallocate(mems& m, uz bytes, uz a) const noexcept
{
    if ( !bytes ) {
        if ( alignedDeallocate(m) ) {
            m = {};
            return true;
        }

        return false;
    }

    auto p = alignedAllocate(bytes, a);
    if ( !p )
        return false;

    auto const upTo = std::max(bytes, m.card());
    std::memcpy(p.data(), m.data(), upTo);
    alignedDeallocate(m);
    m = p;
    return true;
}

bool Mallocator::deallocate(mems m) const noexcept
{
    std::free(m.data());
    return true;
}

bool Mallocator::alignedDeallocate(mems m) const noexcept
{
    _aligned_free(m.data());
    return true;
}

//
// PageAllocator

mems PageAllocator::allocate(uz bytes) const noexcept
{
    if ( !bytes )
        return {};

    auto p = ::VirtualAlloc(NULL, bytes, MEM_COMMIT, PAGE_READWRITE);
    if ( p == NULL )
        return {};

    return { p, bytes };
}

mems PageAllocator::allocateZeroed(uz bytes) const noexcept
{
    return allocate(bytes);
}

bool PageAllocator::deallocate(mems m) const noexcept
{
    if ( !m.data() )
        return true;

    return ::VirtualFree(m.data(), 0, MEM_RELEASE) == TRUE;
}

//
// AscendingPageAllocator

AscendingPageAllocator::AscendingPageAllocator(uz bytes) noexcept
{
    auto const reserveBytes = !bytes
        ? allocationGranularity()
        : roundUpToMultiple(bytes, allocationGranularity());

    myBegin = static_cast<u8*>(::VirtualAlloc(NULL, reserveBytes, MEM_RESERVE, PAGE_NOACCESS));
    myEnd = myBegin;
    myLimit = myBegin + reserveBytes;
}

AscendingPageAllocator::~AscendingPageAllocator() noexcept
{
    if ( myBegin )
        deallocateAll();
}

mems AscendingPageAllocator::allocate(uz bytes) noexcept
{
    auto const goodSize = goodAllocSize(bytes);

    auto target = myEnd + goodSize;
    if ( !goodSize || myLimit < target )
        return {};

    if ( !win32::extendCommitLimit(myEnd, target) )
        return {};

    mems ret { myEnd, bytes };
    myEnd = target;

    return ret;
}

mems AscendingPageAllocator::alignedAllocate(uz bytes, uz a) noexcept
{
    auto const alignedEnd = alignedUp(myEnd, a);
    auto const goodSize = goodAllocSize(bytes);
    auto const target = alignedEnd + goodSize;
    if ( !goodSize || myLimit < target )
        return {};

    if ( !win32::extendCommitLimit(myEnd, target) )
        return {};

    mems ret { alignedEnd, bytes };
    myEnd = target;

    return ret;
}

bool AscendingPageAllocator::expand(mems& m, uz delta) noexcept
{
    if ( !delta )
        return true;

    if ( !m )
        return false;

    auto const PS = alignment();
    auto pageUp = alignedUp(static_cast<u8*>(m.end()), PS);
    auto target = static_cast<u8*>(m.end()) + delta;
    if ( target <= pageUp ) {
        m = mems(m.data(), m.card() + delta);
        return true;
    }

    if ( pageUp < myEnd || myLimit < target )
        return false;

    if ( !win32::extendCommitLimit(myEnd, pageUp) )
        return false;

    myEnd = pageUp;
    m = mems(m.data(), m.card() + delta);
    return true;
}

bool AscendingPageAllocator::deallocate(mems m) noexcept
{
    return ::VirtualFree(m.data(), m.card(), MEM_DECOMMIT) == TRUE;
}

bool AscendingPageAllocator::alignedDeallocate(mems m, uz) noexcept
{
    return deallocate(m);
}

bool AscendingPageAllocator::deallocateAll() noexcept
{
    auto ret = ::VirtualFree(myBegin, 0, MEM_RELEASE) == TRUE;
    myBegin = nullptr;
    myEnd = nullptr;
    myLimit = nullptr;

    return ret;
}

    } // namespace allocators
} // namespace kyfoo
