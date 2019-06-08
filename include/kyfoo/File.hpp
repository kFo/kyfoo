#pragma once

#include <filesystem>

#include <kyfoo/Allocators.hpp>
#include <kyfoo/SystemError.hpp>
#include <kyfoo/Types.hpp>

namespace kyfoo {

class SystemError;

class FileIO
{
public:
#ifdef _WIN32
    using Handle = void*;
#else
    #pragma error("platform not supported")
#endif

protected:
    constexpr FileIO() noexcept;

    constexpr /*implicit*/ FileIO(Handle handle) noexcept
        : myHandle(handle)
    {
    }

public:
    FileIO(FileIO const&) = delete;
    void operator = (FileIO const&) = delete;

    FileIO(FileIO&& rhs) noexcept
        : myHandle(rhs.myHandle)
    {
        rhs.myHandle = Handle();
    }

    FileIO& operator = (FileIO&& rhs) noexcept
    {
        close();
        swap(rhs);
        return *this;
    }

    ~FileIO() noexcept;

    void swap(FileIO& rhs) noexcept
    {
        using kyfoo::swap;
        swap(myHandle, rhs.myHandle);
    }

public:
    void close() noexcept;

    Handle release() noexcept;

    uz write(memv m) noexcept;
    //uz read(mems m) noexcept;

protected:
    Handle myHandle = Handle();
};

class FileOutput : protected FileIO
{
public:
    static ErrorWrapper<FileOutput> open(std::filesystem::path const& path) noexcept;

protected:
    FileOutput() noexcept = default;

public:
    /*implicit*/ FileOutput(Handle handle)
        : FileIO(handle)
    {
    }

    FileOutput(FileOutput&& rhs) noexcept = default;
    FileOutput& operator = (FileOutput&& rhs) noexcept = default;

    FileOutput(FileOutput const&) = delete;
    void operator = (FileOutput const&) = delete;

    ~FileOutput() noexcept = default;

protected:
    SystemError tryOpen(std::filesystem::path const& path) noexcept;

public:
    using FileIO::close;
    using FileIO::release;
    using FileIO::write;
};

FileOutput openStdout() noexcept;
FileOutput openStderr() noexcept;

} // namespace kyfoo
