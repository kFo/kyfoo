#pragma once

#include <filesystem>

#include <kyfoo/Slice.hpp>
#include <kyfoo/SystemError.hpp>

namespace kyfoo {

class MMFile
{
public:
    using Handle = void*;

    static ErrorWrapper<MMFile> open(std::filesystem::path const& path);

public:
    MMFile() noexcept;

protected:
    explicit MMFile(std::filesystem::path const& path);

public:
    MMFile(MMFile const&) = delete;
    void operator = (MMFile const&) = delete;

    MMFile(MMFile&& rhs) noexcept;
    MMFile& operator = (MMFile&& rhs) noexcept;

    ~MMFile();

    void swap(MMFile& rhs) noexcept;

public:
    SystemError tryOpen(std::filesystem::path const& path);
    void close();
    void release();

public:
    Slice<char const> view() const noexcept;
    bool valid() const noexcept;

public:
    explicit operator bool () const noexcept { return valid(); }

private:
    Handle myFile = nullptr;
    Handle myMap = nullptr;
    void const* myBegin = nullptr;
    void const* myEnd = nullptr;
};

} // namespace kyfoo
