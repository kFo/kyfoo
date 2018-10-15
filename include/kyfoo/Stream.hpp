#pragma once

#include <filesystem>
#include <optional>
#include <system_error>

#include <kyfoo/Slice.hpp>

namespace kyfoo {

template <typename T>
class ErrorWrapper : public std::error_code
{
public:
    explicit ErrorWrapper() noexcept = default;
    ErrorWrapper(int ec, std::error_category const& ecat) noexcept
        : std::error_code(ec, ecat)
    {
    }

    template <typename ErrorCodeEnum>
    explicit ErrorWrapper(ErrorCodeEnum e) noexcept
        : std::error_code(e)
    {
    }

    template <typename... Args>
    ErrorWrapper(std::error_code ec, Args... args)
        : std::error_code(ec)
        , myPayload(std::in_place_t{}, std::forward<Args>(args)...)
    {
    }

    template <typename... Args>
    explicit ErrorWrapper(Args... args)
        : myPayload(std::in_place_t{}, std::forward<Args>(args)...)
    {
    }

public:
    T&& unwrap() { return std::move(*myPayload); }

    T& operator * () { return *myPayload; }
    T const& operator * () const { return *myPayload; }

private:
    std::optional<T> myPayload;
};

template <typename T>
T&& unwrap(ErrorWrapper<T>& err)
{
    return err.unwrap();
}

class MMFile
{
public:
    using handle_t = void*;

public:
    MMFile() noexcept;
    explicit MMFile(std::filesystem::path const& path);

    MMFile(MMFile const&) = delete;
    void operator = (MMFile const&) = delete;

    MMFile(MMFile&& rhs) noexcept;
    MMFile& operator = (MMFile&& rhs) noexcept;

    ~MMFile();

    void swap(MMFile& rhs) noexcept;

public:
    std::error_code open(std::filesystem::path const& path);
    void close();
    void release();

public:
    Slice<char const> view() const noexcept;
    bool valid() const noexcept;

public:
    explicit operator bool () const noexcept
    {
        return valid();
    }

private:
    handle_t myFile = nullptr;
    handle_t myMap = nullptr;
    void const* myBegin = nullptr;
    void const* myEnd = nullptr;
};

ErrorWrapper<MMFile> openFile(std::filesystem::path const& path);
std::error_code lastError();

} // namespace kyfoo
