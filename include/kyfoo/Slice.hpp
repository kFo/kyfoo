#pragma once

#include <iterator>
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
constexpr uz ptrdist(T const* begin, T const* end) noexcept
{
    return end - begin;
}

template <>
constexpr uz ptrdist<void>(void const* begin, void const* end) noexcept
{
    return static_cast<u8 const*>(end) - static_cast<u8 const*>(begin);
}

template <typename T>
class SliceBase
{
public:
    using value_type = T;
    using pointer =
        std::conditional_t<std::is_pointer_v<value_type>
                        && std::is_const_v<std::remove_pointer_t<value_type>>,
                           value_type const*, value_type*>;
    using const_pointer = value_type const*;
    using iterator = pointer;
    using const_iterator = const_pointer;
    using size_type = uz;

public:
    constexpr SliceBase() noexcept = default;

    constexpr SliceBase(SliceBase const&) noexcept = default;
    constexpr SliceBase& operator = (SliceBase const&) noexcept = default;
    constexpr SliceBase(SliceBase&&) noexcept = default;
    constexpr SliceBase& operator = (SliceBase&&) noexcept = default;
    ~SliceBase() noexcept = default;

    constexpr SliceBase(pointer p, size_type len) noexcept
        : myData(p)
        , myLength(len)
    {
    }

    constexpr SliceBase(pointer begin, pointer end) noexcept
        : SliceBase(begin, ptrdist(begin, end))
    {
    }

    template <typename U>
    constexpr /*implicit*/ SliceBase(SliceBase<U> const& s) noexcept
        : SliceBase(s.begin(), s.end())
    {
    }

    template <typename U>
    constexpr /*implicit*/ SliceBase(SliceBase<U>& s) noexcept
        : SliceBase(s.begin(), s.end())
    {
    }

    void swap(SliceBase& s) noexcept
    {
        using kyfoo::swap;
        swap(myData, s.myData);
        swap(myLength, s.myLength);
    }

public:
    constexpr iterator       begin()       noexcept { return myData; }
    constexpr const_iterator begin() const noexcept { return myData; }

    constexpr const_iterator cbegin() noexcept { return myData; }
    constexpr const_iterator cbegin() const noexcept { return myData; }

    constexpr iterator end() noexcept
    {
        if constexpr(std::is_void_v<value_type>)
            return reinterpret_cast<std::conditional_t<std::is_const_v<value_type>, u8 const*, u8*>>(myData)
                 + myLength;
        else
            return myData + myLength;
    }

    constexpr const_iterator end () const noexcept { return const_cast<SliceBase*>(this)->end(); }
    constexpr const_iterator cend()       noexcept { return end(); }
    constexpr const_iterator cend() const noexcept { return end(); }

    constexpr bool empty() const noexcept { return myLength == 0; }

    constexpr pointer data() noexcept { return myData; }
    constexpr const_pointer data() const noexcept { return myData; }

    constexpr uz length() const noexcept { return myLength; }
    constexpr uz size  () const noexcept { return myLength; }

    constexpr void popFront() noexcept { ++myData; --myLength; }
    constexpr void popBack () noexcept { --myLength; }

    constexpr void clear() noexcept { myData = nullptr; myLength = 0; }

protected:
    pointer myData = nullptr;
    size_type myLength = 0;
};

template <typename T>
class SliceBaseDereferenceable : public SliceBase<T>
{
public:
    using value_type     = typename SliceBase<T>::value_type;
    using pointer        = typename SliceBase<T>::pointer;
    using const_pointer  = typename SliceBase<T>::const_pointer;
    using iterator       = typename SliceBase<T>::iterator;
    using const_iterator = typename SliceBase<T>::const_iterator;
    using size_type      = typename SliceBase<T>::size_type;

    using reference = std::remove_pointer_t<typename SliceBase<T>::pointer>&;
    using const_reference = value_type const&;

public:
    constexpr SliceBaseDereferenceable() noexcept = default;

    using SliceBase<T>::SliceBase;

public:
    template <uz N>
    constexpr /*implicit*/ SliceBaseDereferenceable(value_type (&str)[N]) noexcept
        : SliceBaseDereferenceable(str, N-1)
    {
    }

    template <typename U>
    constexpr /*implicit*/ SliceBaseDereferenceable(std::basic_string<U>&&) = delete;

    template <typename U, typename = std::enable_if_t<std::is_convertible_v<U const*, pointer>>>
    constexpr /*implicit*/ SliceBaseDereferenceable(std::basic_string<U> const& s) noexcept
        : SliceBaseDereferenceable(s.data(), s.size())
    {
    }

    template <typename U, typename = std::enable_if_t<std::is_convertible_v<U*, pointer>>>
    constexpr /*implicit*/ SliceBaseDereferenceable(std::basic_string<U>& s) noexcept
        : SliceBaseDereferenceable(s.data(), s.size())
    {
    }

    template <typename U>
    constexpr /*implicit*/ SliceBaseDereferenceable(std::vector<U>&&) = delete;

    constexpr /*implicit*/ SliceBaseDereferenceable(std::vector<std::remove_const_t<value_type>>& v) noexcept
        : SliceBaseDereferenceable(v.data(), v.size())
    {
    }

    template <typename U, typename = std::enable_if_t<std::is_convertible_v<U const*, pointer>>>
    constexpr /*implicit*/ SliceBaseDereferenceable(std::vector<U> const& v) noexcept
        : SliceBaseDereferenceable(v.data(), v.size())
    {
    }

    template <typename U,
              typename = std::enable_if_t<sizeof(Box<U>) == sizeof(value_type)
                                       && std::is_pointer_v<value_type>
                                       && std::is_convertible_v<U const*, value_type>>>
    constexpr /*implicit*/ SliceBaseDereferenceable(std::vector<Box<U>> const& v) noexcept
        : SliceBaseDereferenceable(reinterpret_cast<pointer>(const_cast<void*>(reinterpret_cast<void const*>(v.data()))),
                                   v.size())
    {
    }

    template <typename U,
              typename = std::enable_if_t<sizeof(Box<U>) == sizeof(value_type)
                                       && std::is_pointer_v<value_type>
                                       && std::is_convertible_v<U*, value_type>>>
    constexpr /*implicit*/ SliceBaseDereferenceable(std::vector<Box<U>>& v) noexcept
        : SliceBaseDereferenceable(reinterpret_cast<pointer>(reinterpret_cast<void*>(v.data())),
                                   v.size())
    {
    }

    template <typename U,
              typename = std::enable_if_t<sizeof(Box<U>) == sizeof(value_type)
                                       && std::is_pointer_v<value_type>
                                       && std::is_convertible_v<U const*, value_type>>>
    constexpr /*implicit*/ SliceBaseDereferenceable(SliceBaseDereferenceable<Box<U>> const& s) noexcept
        : SliceBaseDereferenceable(reinterpret_cast<pointer>(const_cast<void*>(reinterpret_cast<void const*>(s.data()))),
                                   s.size())
    {
    }

    template <typename U,
              typename = std::enable_if_t<sizeof(Box<U>) == sizeof(value_type)
                                       && std::is_pointer_v<value_type>
                                       && std::is_convertible_v<U*, value_type>>>
    constexpr /*implicit*/ SliceBaseDereferenceable(SliceBaseDereferenceable<Box<U>>& s) noexcept
        : SliceBaseDereferenceable(reinterpret_cast<pointer>(reinterpret_cast<void*>(s.data())),
                                   s.size())
    {
    }

public:
    constexpr reference       operator [] (size_type index)       noexcept { return this->myData[index]; }
    constexpr const_reference operator [] (size_type index) const noexcept { return this->myData[index]; }

    template <typename Unary>
    std::enable_if_t<std::is_nothrow_invocable_r_v<size_type, Unary, size_type>,
    reference> operator [] (Unary&& f) noexcept { return this->myData[f(this->myLength)]; }

    template <typename Unary>
    std::enable_if_t<std::is_nothrow_invocable_r_v<size_type, Unary, size_type>,
    const_reference> operator [] (Unary&& f) const noexcept { return this->myData[f(this->myLength)]; }

    constexpr reference       front()       noexcept { return *this->myData; }
    constexpr const_reference front() const noexcept { return *this->myData; }

    constexpr reference       back()       noexcept { return this->myData[this->myLength - 1]; }
    constexpr const_reference back() const noexcept { return this->myData[this->myLength - 1]; }

public:
    template <typename U>
    Ordering cmp(SliceBaseDereferenceable<U> rhs) const noexcept
    {
        auto const s = std::min(this->length(), rhs.length());
        for ( uz i = 0; i < s; ++i ) {
            if ( operator[](i) < rhs[i] )
                return LT;
            else if ( rhs[i] < operator[](i) )
                return GT;
        }

        if ( this->length() < rhs.length() )
            return LT;
        else if ( rhs.length() < this->length() )
            return GT;

        return EQ;
    }

    template <typename U>
    bool operator < (SliceBaseDereferenceable<U> rhs) const noexcept
    {
        return cmp(rhs) == LT;
    }

    template <typename U>
    bool operator <= (SliceBaseDereferenceable<U> rhs) const noexcept
    {
        switch (cmp(rhs)) {
        case LT:
        case EQ:
            return true;
        }

        return false;
    }

    template <typename U>
    bool operator > (SliceBaseDereferenceable<U> rhs) const noexcept
    {
        return cmp(rhs) == GT;
    }

    template <typename U>
    bool operator >= (SliceBaseDereferenceable<U> rhs) const noexcept
    {
        switch (cmp(rhs)) {
        case EQ:
        case GT:
            return true;
        }

        return false;
    }

    template <typename U>
    bool operator == (SliceBaseDereferenceable<U> rhs) const noexcept
    {
        if ( this->length() != rhs.length() )
            return false;

        return cmp(rhs) == EQ;
    }

    template <typename U>
    bool operator != (SliceBaseDereferenceable<U> rhs) const noexcept
    {
        return !operator==(rhs);
    }
};

template <typename T>
class Slice
    : public std::conditional_t<std::is_void_v<T>,
                                SliceBase<T>,
                                SliceBaseDereferenceable<T>>
{
public:
    using Base = std::conditional_t<std::is_same_v<std::decay_t<T>, void>,
                                    SliceBase<T>,
                                    SliceBaseDereferenceable<T>>;

    using value_type     = typename Base::value_type;
    using pointer        = typename Base::pointer;
    using const_pointer  = typename Base::const_pointer;
    using iterator       = typename Base::iterator;
    using const_iterator = typename Base::const_iterator;
    using size_type      = typename Base::size_type;

public:
    constexpr Slice() noexcept = default;

    using Base::Base;

    template <typename U>
    Slice& operator = (Slice<U> const& s) noexcept
    {
        Slice(s).swap(*this);
        return *this;
    }

    template <typename U>
    Slice& operator = (Slice<U>& s) noexcept
    {
        Slice(s).swap(*this);
        return *this;
    }

public:
    Slice operator () (size_type start, size_type end) const noexcept
    {
        return Slice(this->myData + start, end - start);
    }

    template <typename Unary>
    std::enable_if_t<
        std::is_nothrow_invocable_r_v<size_type, Unary, size_type>,
        Slice>
    operator () (size_type start, Unary&& f) const noexcept
    {
        return Slice(this->myData + start, f(this->myLength) - start);
    }

    constexpr explicit operator bool () const noexcept
    {
        return !this->empty();
    }

    template <typename U>
    Slice<U> cast() noexcept
    {
        return Slice<U>(static_cast<typename Slice<U>::iterator>(this->begin()),
                        static_cast<typename Slice<U>::iterator>(this->end()));
    }

    template <typename U>
    Slice<U> cast() const noexcept
    {
        return Slice<U>(static_cast<typename Slice<U>::const_iterator>(this->begin()),
                        static_cast<typename Slice<U>::const_iterator>(this->end()));
    }
};

template <typename T> Slice(T*      , uz      ) -> Slice<T>;
template <typename T> Slice(T const*, uz      ) -> Slice<T>;
template <typename T> Slice(T*      , T*      ) -> Slice<T>;
template <typename T> Slice(T const*, T const*) -> Slice<T>;

//
// operators

template <typename T, uz N>
bool operator == (SliceBaseDereferenceable<T const> lhs, T const (&rhs)[N]) noexcept
{
    return lhs == SliceBaseDereferenceable<T const>(rhs);
}

template <typename T, uz N>
bool operator == (T const (&lhs)[N], SliceBaseDereferenceable<T const> rhs) noexcept
{
    return SliceBaseDereferenceable<T const>(lhs) == rhs;
}

template <typename T, uz N>
bool operator != (SliceBaseDereferenceable<T const> lhs, T const (&rhs)[N]) noexcept
{
    return !(lhs == rhs);
}

template <typename T, uz N>
bool operator != (T const (&lhs)[N], SliceBaseDereferenceable<T const> rhs) noexcept
{
    return !(lhs == rhs);
}

template <typename T, typename U>
bool operator == (std::basic_string<T> const& lhs, SliceBaseDereferenceable<U> rhs) noexcept
{
    return SliceBaseDereferenceable<T const>(lhs) == rhs;
}

template <typename T, typename U>
bool operator == (SliceBaseDereferenceable<T> lhs, std::basic_string<U> const& rhs) noexcept
{
    return lhs == SliceBaseDereferenceable<T const>(rhs);
}

template <typename T, typename U>
bool operator != (std::basic_string<T> const& lhs, SliceBaseDereferenceable<U> rhs) noexcept
{
    return !(lhs == rhs);
}

template <typename T, typename U>
bool operator != (SliceBaseDereferenceable<T> lhs, std::basic_string<U> const& rhs) noexcept
{
    return !(lhs == rhs);
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
