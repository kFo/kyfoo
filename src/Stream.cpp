#include <kyfoo/Stream.hpp>

#include <kyfoo/Utilities.hpp>
#include <kyfoo/Types.hpp>

#define NOMINMAX
#define VC_EXTRALEAN
#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
#undef WIN32_LEAN_AND_MEAN
#undef VC_EXTRALEAN
#undef NOMINMAX

namespace kyfoo {

//
// MMFile

MMFile::MMFile() noexcept = default;

MMFile::MMFile(std::filesystem::path const& path)
{
    open(path);
}

MMFile::MMFile(MMFile&& rhs) noexcept
    : myFile(rhs.myFile)
    , myMap(rhs.myMap)
    , myBegin(rhs.myBegin)
    , myEnd(rhs.myEnd)
{
    rhs.release();
}

MMFile& MMFile::operator = (MMFile&& rhs) noexcept
{
    this->~MMFile();
    new (this) MMFile(std::move(rhs));
    return *this;
}

MMFile::~MMFile()
{
    if ( valid() )
        close();
}

void MMFile::swap(MMFile& rhs) noexcept
{
    using kyfoo::swap;
    swap(myFile, rhs.myFile);
    swap(myMap, rhs.myMap);
    swap(myBegin, rhs.myBegin);
}

std::error_code MMFile::open(std::filesystem::path const& path)
{
    myFile = ::CreateFile(path.c_str(),
                          GENERIC_READ,
                          0,
                          NULL,
                          OPEN_EXISTING,
                          FILE_ATTRIBUTE_NORMAL,
                          NULL);
    if ( myFile == INVALID_HANDLE_VALUE ) {
        myFile = nullptr;
        return lastError();
    }

    myMap = ::CreateFileMapping(myFile, NULL, PAGE_READONLY, 0, 0, NULL);
    if ( myMap == NULL ) {
        ::CloseHandle(myFile);
        return lastError();
    }

    myBegin = ::MapViewOfFile(myMap, FILE_MAP_READ, 0, 0, 0);
    if ( myBegin == NULL ) {
        ::CloseHandle(myMap);
        ::CloseHandle(myFile);
        return lastError();
    }

    LARGE_INTEGER fileSize;
    if ( !::GetFileSizeEx(myFile, &fileSize) ) {
        close();
        return lastError();
    }

    myEnd = static_cast<u8 const*>(myBegin) + fileSize.QuadPart;
    return {};
}

void MMFile::close()
{
    ::UnmapViewOfFile(myBegin);
    ::CloseHandle(myMap);
    ::CloseHandle(myFile);

    release();
}

void MMFile::release()
{
    myFile = nullptr;
    myMap = nullptr;
    myBegin = nullptr;
    myEnd = nullptr;
}

Slice<char const> MMFile::view() const noexcept
{
    return slice(static_cast<char const*>(myBegin), static_cast<char const*>(myEnd));
}

bool MMFile::valid() const noexcept
{
    return myFile && myMap && myBegin && myEnd;
}

//
// Misc

ErrorWrapper<MMFile> openFile(std::filesystem::path const& path)
{
    MMFile ret;
    auto ec = ret.open(path);
    return ErrorWrapper<MMFile>(ec, std::move(ret));
}

std::error_code lastError()
{
    return std::error_code(::GetLastError(), std::system_category());
}

} // namespace kyfoo
