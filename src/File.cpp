#include <kyfoo/File.hpp>

#include "win32.hpp"

namespace kyfoo {

//
// FileIO

constexpr FileIO::FileIO() noexcept = default;

FileIO::~FileIO() noexcept = default;

void FileIO::close() noexcept
{
    if ( myHandle )
        ::CloseHandle(myHandle);
}

FileIO::Handle FileIO::release() noexcept
{
    auto ret = myHandle;
    myHandle = NULL;
    return ret;
}

uz FileIO::write(memv m) noexcept
{
    ::DWORD ret;
    if ( ::WriteFile(myHandle,
                     m.data(),
                     trunc<::DWORD>(m.card()),
                     &ret,
                     NULL) == FALSE )
    {
        return 0;
    }

    return ret;
}

//
// FileOutput

ErrorWrapper<FileOutput> FileOutput::open(std::filesystem::path const& path) noexcept
{
    FileOutput ret;
    auto ec = ret.tryOpen(path);
    return { ec, std::move(ret) };
}

SystemError FileOutput::tryOpen(std::filesystem::path const& path) noexcept
{
    myHandle = ::CreateFileW(path.c_str(),
                             GENERIC_WRITE,
                             0,
                             NULL,
                             CREATE_ALWAYS,
                             FILE_ATTRIBUTE_NORMAL,
                             NULL);
    if ( myHandle == INVALID_HANDLE_VALUE )
        return SystemError::last();

    return {};
}

//
// misc

FileOutput openStdout() noexcept
{
    return ::GetStdHandle(STD_OUTPUT_HANDLE);
}

FileOutput openStderr() noexcept
{
    return ::GetStdHandle(STD_ERROR_HANDLE);
}

} // namespace kyfoo
