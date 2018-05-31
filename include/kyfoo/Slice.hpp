#pragma once

#include <vector>
#include <type_traits>

#include <kyfoo/Types.hpp>

namespace kyfoo {

template <typename T>
class Slice
{
public:
    using value_type = T;
    using pointer =
        std::conditional_t<std::is_pointer_v<value_type>
                        && std::is_const_v<std::remove_pointer_t<value_type>>,
                           value_type const*, value_type*>;
    using const_pointer = value_type const*;
    using reference = std::remove_pointer_t<pointer>&;
    using const_reference = value_type const&;
    using iterator = pointer;
    using const_iterator = const_pointer;
    using size_type = uz;

public:
    constexpr Slice() noexcept = default;

    Slice(pointer p, size_type len) noexcept
        : myData(p)
        , myLength(len)
    {
    }

    /*implicit*/ Slice(std::vector<value_type>& v) noexcept
        : Slice(v.data(), v.size())
    {
    }

    template <typename U, typename = std::enable_if_t<std::is_convertible_v<U const*, pointer>>>
    /*implicit*/ Slice(std::vector<U> const& v) noexcept
        : Slice(v.data(), v.size())
    {
    }

    template <typename U,
              typename = std::enable_if_t<sizeof(Box<U>) == sizeof(value_type)
                                       && std::is_pointer_v<value_type>
                                       && std::is_convertible_v<U const*, value_type>>>
    /*implicit*/ Slice(std::vector<Box<U>> const& v) noexcept
        : myData(reinterpret_cast<pointer>(const_cast<void*>(reinterpret_cast<void const*>(v.data()))))
        , myLength(v.size())
    {
    }

    template <typename U,
              typename = std::enable_if_t<sizeof(Box<U>) == sizeof(value_type)
                                       && std::is_pointer_v<value_type>
                                       && std::is_convertible_v<U*, value_type>>>
    /*implicit*/ Slice(std::vector<Box<U>>& v) noexcept
        : myData(reinterpret_cast<pointer>(reinterpret_cast<void*>(v.data())))
        , myLength(v.size())
    {
    }

    /*implicit*/ Slice(Slice const& s) noexcept
        : myData(s.myData)
        , myLength(s.myLength)
    {
    }

    template <typename U, typename = std::enable_if_t<std::is_convertible_v<U, value_type>>>
    /*implicit*/ Slice(Slice<U> const& s) noexcept
        : Slice(s.data(), s.size())
    {
    }

    Slice& operator = (Slice const& s) noexcept
    {
        Slice(s).swap(*this);
        return *this;
    }

    void swap(Slice& s) noexcept
    {
        using std::swap;
        swap(myData, s.myData);
        swap(myLength, s.myLength);
    }

    /*implicit*/ Slice(Slice&& rhs) noexcept
        : myData(rhs.myData)
        , myLength(rhs.myLength)
    {
        rhs.clear();
    }

    Slice& operator = (Slice&& rhs) noexcept
    {
        new (this) Slice(std::move(rhs));
        rhs.clear();
        return *this;
    }

public:
    reference operator [] (size_type index) noexcept
    {
        return myData[index];
    }

    const_reference operator [] (size_type index) const noexcept
    {
        return myData[index];
    }

    template <typename Unary>
    std::enable_if_t<
        std::is_nothrow_invocable_r_v<size_type, Unary, size_type>
        , reference>
    wut (Unary&& f) noexcept
    {
        return myData[f(myLength)];
    }

    template <typename Unary>
    std::enable_if_t<
        std::is_nothrow_invocable_r_v<size_type, Unary, size_type>
        , const_reference>
    operator [] (Unary&& f) const noexcept
    {
        return myData[f(myLength)];
    }

    Slice operator () (size_type start, size_type end) const noexcept
    {
        return Slice(myData + start, end - start);
    }

    template <typename Unary>
    std::enable_if_t<
        std::is_nothrow_invocable_r_v<size_type, Unary, size_type>
        , Slice>
    operator () (size_type start, Unary&& f) const noexcept
    {
        return Slice(myData + start, f(myLength) - start);
    }

    explicit operator bool () const noexcept
    {
        return !empty();
    }

public:
    iterator begin() noexcept
    {
        return myData;
    
    }
    const_iterator begin() const noexcept
    {
        return myData;
    }

    iterator end() noexcept
    {
        return myData + myLength;
    }

    const_iterator end() const noexcept
    {
        return myData + myLength;
    }

    bool empty() const noexcept
    {
        return begin() == end();
    }

    pointer data() noexcept
    {
        return myData;
    }

    const_pointer data() const noexcept
    {
        return myData;
    }

    uz length() const noexcept
    {
        return myLength;
    }

    uz size() const noexcept
    {
        return myLength;
    }

    reference front() noexcept
    {
        return *myData;
    }

    const_reference front() const noexcept
    {
        return *myData;
    }

    reference back() noexcept
    {
        return myData[myLength - 1];
    }

    const_reference back() const noexcept
    {
        return myData[myLength - 1];
    }

    void popFront() noexcept
    {
        ++myData;
        --myLength;
    }

    void popBack() noexcept
    {
        --myLength;
    }

    void clear() noexcept
    {
        myData = nullptr;
        myLength = 0;
    }

private:
    pointer myData = nullptr;
    size_type myLength = 0;
};

template <typename T>
typename Slice<T>::iterator begin(Slice<T>& rhs)
{
    return rhs.begin();
}

template <typename T>
typename Slice<T>::const_iterator begin(Slice<T> const& rhs)
{
    return rhs.begin();
}

template <typename T>
typename Slice<T>::iterator end(Slice<T>& rhs)
{
    return rhs.end();
}

template <typename T>
typename Slice<T>::const_iterator end(Slice<T> const& rhs)
{
    return rhs.end();
}

template <typename T>
Slice<T*> slice(std::vector<Box<T>> const& v)
{
    return Slice<T*>(v);
}

template <typename T>
Slice<T*> slice(std::vector<Box<T>>& v, uz start)
{
    return Slice<T*>(v)(start, v.size());
}

template <typename T>
Slice<T const*> slice(std::vector<Box<T>> const& v, uz start)
{
    return Slice<T const*>(v)(start, v.size());
}

template <typename T>
Slice<T*> slice(std::vector<Box<T>>& v, uz start, uz end)
{
    return Slice<T*>(v)(start, end);
}

template <typename T>
Slice<T const*> slice(std::vector<Box<T>> const& v, uz start, uz end)
{
    return Slice<T const*>(v)(start, end);
}

template <typename T>
Slice<T> slice(Slice<T> s, uz start)
{
    return s(start, s.size());
}

template <typename T>
Slice<T> slice(T& rhs)
{
    return Slice<T>(&rhs, 1);
}

template <typename T>
Slice<T const*> sliceBox(Box<T> const& rhs)
{
    return Slice<T const*>(reinterpret_cast<T const* const*>(reinterpret_cast<void const* const*>(&rhs)), 1);
}

template <typename T>
struct is_slice : std::false_type {};

template <typename T>
struct is_slice<Slice<T>> : std::true_type {};

template <typename T>
constexpr bool is_slice_v = is_slice<T>::value;

} // namespace kyfoo
