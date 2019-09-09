#pragma once

#include <memory>

#include <kyfoo/Utilities.hpp>

namespace kyfoo {

struct Deleter
{
    template <typename T>
    void operator () (T* p) const
    {
        delete p;
    }
};

struct LeakyDeleter
{
    template <typename T>
    void operator()(T* p) const
    {
        p->~T();
    }
};

#ifndef KYFOO_BOX_LEAK

using DefaultBoxDeleter = Deleter;

#else // KYFOO_BOX_LEAK

using DefaultBoxDeleter = LeakyDeleter;

#endif // KYFOO_BOX_LEAK

template <typename T, typename D = DefaultBoxDeleter>
class Box : private D
{
public:
    template <typename TOther, typename DOther>
    friend class Box;

    using Element = T;

public:
    constexpr /*implicit*/ Box() noexcept = default;

    constexpr /*implicit*/ Box(std::nullptr_t) noexcept
        : Box()
    {
    }

    constexpr Box& operator = (std::nullptr_t) noexcept
    {
        reset();
    }

    constexpr /*implicit*/ Box(T* p) noexcept
        : myPtr(p)
    {
    }

    constexpr Box& operator = (T* p) noexcept
    {
        reset(p);
        return *this;
    }

    Box(Box const&) = delete;
    void operator = (Box const&) = delete;

    template <typename U, typename = std::enable_if_t<std::is_convertible_v<U*, T*>>>
    constexpr /*implicit*/ Box(Box<U, D>&& rhs) noexcept
        : myPtr(rhs.release())
    {
    }

    template <typename U, typename = std::enable_if_t<std::is_convertible_v<U*, T*>>>
    constexpr Box& operator = (Box<U, D>&& rhs) noexcept
    {
        reset(rhs.release());
        return *this;
    }

    ~Box() noexcept
    {
        reset();
    }

    template <typename U, typename = std::enable_if_t<std::is_convertible_v<U*, T*>>>
    constexpr void swap(Box<U, D>& rhs) noexcept
    {
        using kyfoo::swap;
        swap(myPtr, rhs.myPtr);
    }

public:
    constexpr T* operator -> () noexcept
    {
        return myPtr;
    }

    constexpr T const* operator -> () const noexcept
    {
        return myPtr;
    }

    constexpr T& operator * () noexcept
    {
        return *myPtr;
    }

    constexpr T const& operator * () const noexcept
    {
        return *myPtr;
    }

    constexpr explicit operator bool () const noexcept
    {
        return myPtr != nullptr;
    }

    template <typename U, typename = std::enable_if_t<std::is_convertible_v<U*, T*>>>
    constexpr bool operator == (Box<U, D> const& rhs) const noexcept
    {
        return myPtr == rhs.myPtr;
    }

    template <typename U, typename = std::enable_if_t<std::is_convertible_v<U*, T*>>>
    constexpr bool operator != (Box<U, D> const& rhs) const noexcept
    {
        return !operator==(rhs);
    }

    template <typename U, typename = std::enable_if_t<std::is_convertible_v<U*, T*>>>
    constexpr bool operator == (U const* rhs) const noexcept
    {
        return myPtr == rhs;
    }

    template <typename U, typename = std::enable_if_t<std::is_convertible_v<U*, T*>>>
    constexpr bool operator != (U const* rhs) const noexcept
    {
        return !operator==(rhs);
    }

    constexpr T* get() noexcept
    {
        return myPtr;
    }

    constexpr T const* get() const noexcept
    {
        return myPtr;
    }

    constexpr T* release() noexcept
    {
        auto ret = myPtr;
        myPtr = nullptr;
        return ret;
    }

    template <typename U, typename = std::enable_if_t<std::is_convertible_v<U*, T*>>>
    constexpr void reset(U* p) noexcept
    {
        operator()(myPtr);
        myPtr = p;
    }

    constexpr void reset() noexcept
    {
        operator()(myPtr);
        myPtr = nullptr;
    }

private:
    T* myPtr = nullptr;
};

template <typename T, typename... Args>
Box<T> mk(Args&&... args)
{
    return Box<T>(new T(std::forward<Args>(args)...));
}

} // namespace kyfoo
