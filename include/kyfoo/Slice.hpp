#pragma once

#include <vector>
#include <type_traits>

#include <kyfoo/Types.hpp>
#include <kyfoo/Utilities.hpp>

namespace kyfoo {

enum Ordering {
    LT,
    EQ,
    GT,
};

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

    constexpr Slice(pointer p, size_type len) noexcept
        : myData(p)
        , myLength(len)
    {
    }

    template <uz N>
    constexpr /*implicit*/ Slice(value_type (&str)[N]) noexcept
        : myData(str)
        , myLength(N-1)
    {
    }

    template <typename U, typename = std::enable_if_t<std::is_convertible_v<U const*, pointer>>>
    /*implicit*/ Slice(std::basic_string<U> const& s) noexcept
        : Slice(s.data(), s.size())
    {
    }

    template <typename U, typename = std::enable_if_t<std::is_convertible_v<U*, pointer>>>
    /*implicit*/ Slice(std::basic_string<U>& s) noexcept
        : Slice(s.data(), s.size())
    {
    }

    constexpr /*implicit*/ Slice(std::vector<value_type>& v) noexcept
        : Slice(v.data(), v.size())
    {
    }

    template <typename U, typename = std::enable_if_t<std::is_convertible_v<U const*, pointer>>>
    constexpr /*implicit*/ Slice(std::vector<U> const& v) noexcept
        : Slice(v.data(), v.size())
    {
    }

    template <typename U,
              typename = std::enable_if_t<sizeof(Box<U>) == sizeof(value_type)
                                       && std::is_pointer_v<value_type>
                                       && std::is_convertible_v<U const*, value_type>>>
    constexpr /*implicit*/ Slice(std::vector<Box<U>> const& v) noexcept
        : myData(reinterpret_cast<pointer>(const_cast<void*>(reinterpret_cast<void const*>(v.data()))))
        , myLength(v.size())
    {
    }

    template <typename U,
              typename = std::enable_if_t<sizeof(Box<U>) == sizeof(value_type)
                                       && std::is_pointer_v<value_type>
                                       && std::is_convertible_v<U*, value_type>>>
    constexpr /*implicit*/ Slice(std::vector<Box<U>>& v) noexcept
        : myData(reinterpret_cast<pointer>(reinterpret_cast<void*>(v.data())))
        , myLength(v.size())
    {
    }

    constexpr /*implicit*/ Slice(Slice const& s) noexcept
        : myData(s.myData)
        , myLength(s.myLength)
    {
    }

    template <typename U, typename = std::enable_if_t<std::is_convertible_v<U, value_type>>>
    constexpr /*implicit*/ Slice(Slice<U> const& s) noexcept
        : Slice(s.data(), s.size())
    {
    }

    template <typename U,
              typename = std::enable_if_t<sizeof(Box<U>) == sizeof(value_type)
                                       && std::is_pointer_v<value_type>
                                       && std::is_convertible_v<U const*, value_type>>>
    constexpr /*implicit*/ Slice(Slice<Box<U>> const& s) noexcept
        : myData(reinterpret_cast<pointer>(const_cast<void*>(reinterpret_cast<void const*>(s.data()))))
        , myLength(s.size())
    {
    }

    template <typename U,
              typename = std::enable_if_t<sizeof(Box<U>) == sizeof(value_type)
                                       && std::is_pointer_v<value_type>
                                       && std::is_convertible_v<U*, value_type>>>
    constexpr /*implicit*/ Slice(Slice<Box<U>>& s) noexcept
        : myData(reinterpret_cast<pointer>(reinterpret_cast<void*>(s.data())))
        , myLength(s.size())
    {
    }

    Slice& operator = (Slice const& s) noexcept
    {
        Slice(s).swap(*this);
        return *this;
    }

    void swap(Slice& s) noexcept
    {
        using kyfoo::swap;
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
    constexpr reference operator [] (size_type index) noexcept
    {
        return myData[index];
    }

    constexpr const_reference operator [] (size_type index) const noexcept
    {
        return myData[index];
    }

    template <typename Unary>
    std::enable_if_t<
        std::is_nothrow_invocable_r_v<size_type, Unary, size_type>
        , reference>
    operator [] (Unary&& f) noexcept
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

    constexpr explicit operator bool () const noexcept
    {
        return !empty();
    }

public:
    constexpr iterator begin() noexcept
    {
        return myData;
    
    }
    constexpr const_iterator begin() const noexcept
    {
        return myData;
    }

    constexpr const_iterator cbegin() noexcept
    {
        return myData;
    }

    constexpr const_iterator cbegin() const noexcept
    {
        return myData;
    }

    constexpr iterator end() noexcept
    {
        return myData + myLength;
    }

    constexpr const_iterator end() const noexcept
    {
        return myData + myLength;
    }

    constexpr const_iterator cend() noexcept
    {
        return myData + myLength;
    }

    constexpr const_iterator cend() const noexcept
    {
        return myData + myLength;
    }

    constexpr bool empty() const noexcept
    {
        return begin() == end();
    }

    constexpr pointer data() noexcept
    {
        return myData;
    }

    constexpr const_pointer data() const noexcept
    {
        return myData;
    }

    constexpr uz length() const noexcept
    {
        return myLength;
    }

    constexpr uz size() const noexcept
    {
        return myLength;
    }

    constexpr reference front() noexcept
    {
        return *myData;
    }

    constexpr const_reference front() const noexcept
    {
        return *myData;
    }

    constexpr reference back() noexcept
    {
        return myData[myLength - 1];
    }

    constexpr const_reference back() const noexcept
    {
        return myData[myLength - 1];
    }

    constexpr void popFront() noexcept
    {
        ++myData;
        --myLength;
    }

    constexpr void popBack() noexcept
    {
        --myLength;
    }

    constexpr void clear() noexcept
    {
        myData = nullptr;
        myLength = 0;
    }

public:
    template <typename U>
    Ordering cmp(Slice<U> rhs) const noexcept
    {
        auto const s = std::min(length(), rhs.length());
        for ( uz i = 0; i < s; ++i ) {
            if ( operator[](i) < rhs[i] )
                return LT;
            else if ( rhs[i] < operator[](i) )
                return GT;
        }

        if ( length() < rhs.length() )
            return LT;
        else if ( rhs.length() < length() )
            return GT;

        return EQ;
    }

    template <typename U>
    bool operator < (Slice<U> rhs) const noexcept
    {
        return cmp(rhs) == LT;
    }

    template <typename U>
    bool operator <= (Slice<U> rhs) const noexcept
    {
        switch (cmp(rhs)) {
        case LT:
        case EQ:
            return true;
        }

        return false;
    }

    template <typename U>
    bool operator > (Slice<U> rhs) const noexcept
    {
        return cmp(rhs) == GT;
    }

    template <typename U>
    bool operator >= (Slice<U> rhs) const noexcept
    {
        switch (cmp(rhs)) {
        case EQ:
        case GT:
            return true;
        }

        return false;
    }

    template <typename U>
    bool operator == (Slice<U> rhs) const noexcept
    {
        if ( length() != rhs.length() )
            return false;

        return cmp(rhs) == EQ;
    }

    template <typename U>
    bool operator != (Slice<U> rhs) const noexcept
    {
        return !operator==(rhs);
    }

private:
    pointer myData = nullptr;
    size_type myLength = 0;
};

//
// operators

template <typename T, uz N>
bool operator == (Slice<T const> lhs, T const (&rhs)[N]) noexcept
{
    return lhs == Slice(rhs);
}

template <typename T, uz N>
bool operator == (T const (&lhs)[N], Slice<T const> rhs) noexcept
{
    return Slice(lhs) == rhs;
}

template <typename T, uz N>
bool operator != (Slice<T const> lhs, T const (&rhs)[N]) noexcept
{
    return !(lhs == Slice(rhs));
}

template <typename T, uz N>
bool operator != (T const (&lhs)[N], Slice<T const> rhs) noexcept
{
    return !(Slice(lhs) == rhs);
}

template <typename T, typename U>
bool operator == (std::basic_string<T> const& lhs, Slice<U> rhs) noexcept
{
    return Slice<T const>(lhs) == rhs;
}

template <typename T, typename U>
bool operator == (Slice<T> lhs, std::basic_string<U> const& rhs) noexcept
{
    return lhs == Slice<T const>(rhs);
}

template <typename T, typename U>
bool operator != (std::basic_string<T> const& lhs, Slice<U> rhs) noexcept
{
    return !(Slice<T const>(lhs) == rhs);
}

template <typename T, typename U>
bool operator != (Slice<T> lhs, std::basic_string<U> const& rhs) noexcept
{
    return !(lhs == Slice<T const>(rhs));
}

//
// accessors

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
typename Slice<T>::const_iterator cbegin(Slice<T>& rhs)
{
    return rhs.cbegin();
}

template <typename T>
typename Slice<T>::const_iterator cbegin(Slice<T> const& rhs)
{
    return rhs.cbegin();
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
typename Slice<T>::const_iterator cend(Slice<T>& rhs)
{
    return rhs.cend();
}

template <typename T>
typename Slice<T>::const_iterator cend(Slice<T> const& rhs)
{
    return rhs.cend();
}

//
// sugar

template <typename T>
Slice<T> slice(T* begin, T* end)
{
    auto const size = end - begin;
    return Slice<T>(begin, size);
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

inline Slice<char const> sliceCString(char const* str)
{
    return Slice<char const>(str, std::strlen(str));
}

//
// properties

template <typename T>
struct is_slice : std::false_type {};

template <typename T>
struct is_slice<Slice<T>> : std::true_type {};

template <typename T>
constexpr bool is_slice_v = is_slice<T>::value;

} // namespace kyfoo
