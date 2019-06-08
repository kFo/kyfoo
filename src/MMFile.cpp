#include <kyfoo/MMFile.hpp>

#include "win32.hpp"

namespace kyfoo {

//
// MMFile

ErrorWrapper<MMFile> MMFile::open(std::filesystem::path const& path)
{
    MMFile ret;
    auto ec = ret.tryOpen(path);
    return ErrorWrapper<MMFile>(ec, std::move(ret));
}

MMFile::MMFile() noexcept = default;

MMFile::MMFile(std::filesystem::path const& path)
{
    tryOpen(path);
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

SystemError MMFile::tryOpen(std::filesystem::path const& path)
{
    myFile = ::CreateFileW(path.c_str(),
                           GENERIC_READ,
                           0,
                           NULL,
                           OPEN_EXISTING,
                           FILE_ATTRIBUTE_NORMAL,
                           NULL);
    if ( myFile == INVALID_HANDLE_VALUE ) {
        myFile = nullptr;
        return SystemError::last();
    }

    myMap = ::CreateFileMappingW(myFile, NULL, PAGE_READONLY, 0, 0, NULL);
    if ( myMap == NULL ) {
        ::CloseHandle(myFile);
        return SystemError::last();
    }

    myBegin = ::MapViewOfFile(myMap, FILE_MAP_READ, 0, 0, 0);
    if ( myBegin == NULL ) {
        ::CloseHandle(myMap);
        ::CloseHandle(myFile);
        return SystemError::last();
    }

    LARGE_INTEGER fileSize;
    if ( !::GetFileSizeEx(myFile, &fileSize) ) {
        close();
        return SystemError::last();
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

} // namespace kyfoo
