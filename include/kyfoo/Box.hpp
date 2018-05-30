#pragma once

#include <algorithm>
#include <memory>

namespace kyfoo {

#ifdef KYFOO_BOX_CUSTOM

template <typename T>
class Box
{
public:
    template <typename U>
    friend class Box;

    using pointer = T*;

public:
    constexpr Box() noexcept = default;

    /*implicit*/ constexpr Box(std::nullptr_t) noexcept
        : Box()
    {
    }

    Box& operator = (std::nullptr_t) noexcept
    {
        reset();
    }

    explicit Box(T* p) noexcept
        : myPtr(p)
    {
    }

    Box(Box const&) = delete;
    void operator = (Box const&) = delete;

    template <typename U, typename = std::enable_if_t<std::is_convertible_v<U*, T*>>>
    /*implicit*/ Box(Box<U>&& rhs) noexcept
        : myPtr(rhs.release())
    {
    }

    template <typename U, typename = std::enable_if_t<std::is_convertible_v<U*, T*>>>
    Box& operator = (Box<U>&& rhs) noexcept
    {
        reset(rhs.release());
        return *this;
    }

    ~Box() noexcept
    {
        reset();
    }

    template <typename U, typename = std::enable_if_t<std::is_convertible_v<U*, T*>>>
    void swap(Box<U>& rhs) noexcept
    {
        using std::swap;
        swap(myPtr, rhs.myPtr);
    }

public:
    T* operator -> () const noexcept
    {
        return myPtr;
    }

    T& operator * () const noexcept
    {
        return *myPtr;
    }

    explicit operator bool () const noexcept
    {
        return myPtr != nullptr;
    }

    T* get() const noexcept
    {
        return myPtr;
    }

    T* release() noexcept
    {
        auto ret = myPtr;
        myPtr = nullptr;
        return ret;
    }

    void reset(T* p = pointer()) noexcept
    {
#ifndef KYFOO_BOX_LEAK
        delete myPtr;
#else
        if ( myPtr )
            myPtr->~T();
#endif
        myPtr = p;
    }

    void reset(std::nullptr_t) noexcept
    {
#ifndef KYFOO_BOX_LEAK
        delete myPtr;
        myPtr = nullptr;
#else
        if ( myPtr ) {
            myPtr->~T();
            myPtr = nullptr;
        }
#endif
    }

private:
    T* myPtr = nullptr;
};

#else // KYFOO_BOX_CUSTOM

#ifndef KYFOO_BOX_LEAK

template <typename T>
using Box = std::unique_ptr<T>;

#else // KYFOO_BOX_LEAK

struct LeakyDeleter {
    template <typename T>
    void operator()(T* p) const {
        p->~T();
    }
};

template <typename T>
using Box = std::unique_ptr<T, LeakyDeleter>;

#endif // KYFOO_BOX_LEAK

#endif // KYFOO_BOX_CUSTOM

template <typename T, typename... Args>
Box<T> mk(Args&&... args)
{
    return Box<T>(new T(std::forward<Args>(args)...));
}

} // namespace kyfoo
