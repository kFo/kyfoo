#pragma once

#include <exception>

#define GUARD_ERR(id) err_guard_##id
#define GUARD(init, expr)                          \
    auto GUARD_ERR(id) = expr;                     \
    init = GUARD_ERR(id).unwrap();                 \
    if ( ::kyfoo::SystemError ec = GUARD_ERR(id) )

namespace kyfoo {

class SystemError
{
public:
#ifdef _WIN32
    using Code = unsigned long; // from minwindef.h
#else
    #pragma error("unsupported platform")
#endif

    static SystemError last();

public:
    constexpr SystemError() noexcept = default;

protected:
    constexpr /*implicit*/ SystemError(Code code) noexcept
        : myCode(code)
    {
    }

public:
    ~SystemError() = default;

public:
    constexpr Code code() const noexcept { return myCode; }

    constexpr explicit operator bool () const noexcept { return myCode != 0; }

private:
    Code myCode = 0;
};

class SystemException : public std::exception
{
public:
    explicit SystemException(SystemError ec)
        : myError(ec)
    {
    }

private:
    SystemError myError;
};

template <typename T>
class ErrorWrapper : public SystemError
{
public:
    /*implicit*/ ErrorWrapper(SystemError::Code ec) noexcept
        : SystemError(ec)
    {
    }

    template <typename... Args>
    ErrorWrapper(SystemError ec, Args&&... args)
        : SystemError(ec)
        , myPayload(std::forward<Args>(args)...)
    {
    }

    template <typename... Args>
    explicit ErrorWrapper(Args&&... args)
        : myPayload(std::forward<Args>(args)...)
    {
    }

    ~ErrorWrapper() noexcept { if (!operator bool()) myPayload.~T(); }

public:
    T&& unwrap() { return std::move(myPayload); }

private:
    union {
        char _unused;
        T myPayload;
    };
};

template <typename T>
T&& unwrap(ErrorWrapper<T>& err)
{
    return err.unwrap();
}

class SystemErrorMessage
{
public:
#ifdef _WIN32
    using Handle = wchar_t*;
    using Data = wchar_t const*;
#else
    #pragma error("unsupported platform")
#endif

public:
    explicit SystemErrorMessage(SystemError err);

    SystemErrorMessage(SystemErrorMessage const&) = delete;
    void operator = (SystemErrorMessage const&) = delete;

    SystemErrorMessage(SystemErrorMessage&&) noexcept = default;
    SystemErrorMessage& operator = (SystemErrorMessage&&) noexcept = default;

    ~SystemErrorMessage();

public:
    Data data() const { return myData; }
    unsigned card() const { return myCard; }

private:
    Handle myData = Handle();
    unsigned myCard = 0;
};

} // namespace kyfoo
