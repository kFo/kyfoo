#pragma once

#include <initializer_list>
#include <iterator>
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
    using Element = T;
    using Pointer = Element*;
    using ConstPointer = Element const*;
    using Iterator = Pointer;
    using ConstIterator = ConstPointer;

public:
    constexpr SliceBase() noexcept = default;

    constexpr SliceBase(SliceBase const&) noexcept = default;
    constexpr SliceBase& operator = (SliceBase const&) noexcept = default;
    constexpr SliceBase(SliceBase&&) noexcept = default;
    constexpr SliceBase& operator = (SliceBase&&) noexcept = default;
    ~SliceBase() noexcept = default;

    constexpr SliceBase(Pointer p, uz card) noexcept
        : myData(p)
        , myCard(card)
    {
    }

    constexpr SliceBase(Pointer begin, Pointer end) noexcept
        : SliceBase(begin, ptrdist(begin, end))
    {
    }

    template <typename U,
              typename = std::enable_if_t<std::is_convertible_v<SliceBase<U>::Pointer, Pointer>>>
    constexpr /*implicit*/ SliceBase(SliceBase<U> s) noexcept
        : SliceBase(s.data(), s.card())
    {
    }

    template <typename = std::enable_if_t<std::is_pointer_v<Element>
                                       && std::is_const_v<std::remove_pointer_t<Element>>>>
    constexpr /*implicit*/ SliceBase(SliceBase<std::remove_const_t<std::remove_pointer_t<Element>>*> const& s) noexcept
        : SliceBase(const_cast<Pointer>(s.data()), s.card())
    {
    }

    void swap(SliceBase& s) noexcept
    {
        using kyfoo::swap;
        swap(myData, s.myData);
        swap(myCard, s.myCard);
    }

public:
    constexpr Iterator      begin()       noexcept { return myData; }
    constexpr ConstIterator begin() const noexcept { return myData; }

    constexpr ConstIterator cbegin()       noexcept { return myData; }
    constexpr ConstIterator cbegin() const noexcept { return myData; }

    constexpr Iterator end() noexcept
    {
        if constexpr(std::is_void_v<Element>)
            return reinterpret_cast<std::conditional_t<std::is_const_v<Element>, u8 const*, u8*>>(myData)
                 + myCard;
        else
            return myData + myCard;
    }

    constexpr ConstIterator end () const noexcept { return const_cast<SliceBase*>(this)->end(); }
    constexpr ConstIterator cend()       noexcept { return end(); }
    constexpr ConstIterator cend() const noexcept { return end(); }

    constexpr Pointer      data()       noexcept { return myData; }
    constexpr ConstPointer data() const noexcept { return myData; }

    constexpr uz card() const noexcept { return myCard; }

    constexpr void popFront() noexcept { ++myData; --myCard; }
    constexpr void popBack () noexcept { --myCard; }

protected:
    Pointer myData = nullptr;
    uz myCard = 0;
};

template <typename T>
class SliceBaseDereferenceable : public SliceBase<T>
{
public:
    using Element       = typename SliceBase<T>::Element;
    using Pointer       = typename SliceBase<T>::Pointer;
    using ConstPointer  = typename SliceBase<T>::ConstPointer;
    using Iterator      = typename SliceBase<T>::Iterator;
    using ConstIterator = typename SliceBase<T>::ConstIterator;

    using Reference      = std::remove_pointer_t<typename SliceBase<T>::Pointer>&;
    using ConstReference = Element const&;

public:
    constexpr SliceBaseDereferenceable() noexcept = default;

    using SliceBase<T>::SliceBase;

public:
    template <typename U>
    constexpr /*implicit*/ SliceBaseDereferenceable(std::initializer_list<U> list) noexcept
        : SliceBaseDereferenceable(list.begin(), list.end())
    {
    }

    template <uz N>
    constexpr /*implicit*/ SliceBaseDereferenceable(Element (&str)[N]) noexcept
        : SliceBaseDereferenceable(str, N-1)
    {
    }

    template <typename U>
    constexpr /*implicit*/ SliceBaseDereferenceable(std::basic_string<U>&&) = delete;

    template <typename U, typename = std::enable_if_t<std::is_convertible_v<U const*, Pointer>>>
    constexpr /*implicit*/ SliceBaseDereferenceable(std::basic_string<U> const& s) noexcept
        : SliceBaseDereferenceable(s.data(), s.size())
    {
    }

    template <typename U, typename = std::enable_if_t<std::is_convertible_v<U*, Pointer>>>
    constexpr /*implicit*/ SliceBaseDereferenceable(std::basic_string<U>& s) noexcept
        : SliceBaseDereferenceable(s.data(), s.size())
    {
    }

    template <typename U>
    constexpr /*implicit*/ SliceBaseDereferenceable(std::vector<U>&&) = delete;

    constexpr /*implicit*/ SliceBaseDereferenceable(std::vector<std::remove_const_t<Element>>& v) noexcept
        : SliceBaseDereferenceable(v.data(), v.size())
    {
    }

    template <typename U, typename = std::enable_if_t<std::is_convertible_v<U const*, Pointer>>>
    constexpr /*implicit*/ SliceBaseDereferenceable(std::vector<U> const& v) noexcept
        : SliceBaseDereferenceable(v.data(), v.size())
    {
    }

    template <typename U,
              typename = std::enable_if_t<sizeof(Box<U>) == sizeof(Element)
                                       && std::is_pointer_v<Element>
                                       && std::is_convertible_v<U const*, Element>>>
    constexpr /*implicit*/ SliceBaseDereferenceable(std::vector<Box<U>> const& v) noexcept
        : SliceBaseDereferenceable(reinterpret_cast<ConstPointer>(const_cast<void*>(reinterpret_cast<void const*>(v.data()))),
                                   v.size())
    {
    }

    template <typename U,
              typename = std::enable_if_t<sizeof(Box<U>) == sizeof(Element)
                                       && std::is_pointer_v<Element>
                                       && std::is_convertible_v<U*, Element>>>
    constexpr /*implicit*/ SliceBaseDereferenceable(std::vector<Box<U>>& v) noexcept
        : SliceBaseDereferenceable(reinterpret_cast<Pointer>(reinterpret_cast<void*>(v.data())),
                                   v.size())
    {
    }

    template <typename U,
              typename = std::enable_if_t<sizeof(Box<U>) == sizeof(Element)
                                       && std::is_pointer_v<Element>
                                       && std::is_convertible_v<U const*, Element>>>
    constexpr /*implicit*/ SliceBaseDereferenceable(SliceBaseDereferenceable<Box<U>> const& s) noexcept
        : SliceBaseDereferenceable(reinterpret_cast<Pointer>(const_cast<void*>(reinterpret_cast<void const*>(s.data()))),
                                   s.card())
    {
    }

    template <typename U,
              typename = std::enable_if_t<sizeof(Box<U>) == sizeof(Element)
                                       && std::is_pointer_v<Element>
                                       && std::is_convertible_v<U*, Element>>>
    constexpr /*implicit*/ SliceBaseDereferenceable(SliceBaseDereferenceable<Box<U>>& s) noexcept
        : SliceBaseDereferenceable(reinterpret_cast<Pointer>(reinterpret_cast<void*>(s.data())),
                                   s.card())
    {
    }

    template <typename U,
              typename = std::enable_if_t<sizeof(Box<U>) == sizeof(Element)
                                       && std::is_pointer_v<Element>
                                       && std::is_convertible_v<U*, Element>>>
    constexpr /*implicit*/ SliceBaseDereferenceable(SliceBaseDereferenceable<const Box<U>>& s) noexcept
        : SliceBaseDereferenceable(reinterpret_cast<Pointer>(const_cast<void const**>(reinterpret_cast<void const* const*>(s.data()))),
                                   s.card())
    {
    }

public:
    constexpr Reference      operator [] (uz index)       noexcept { return this->myData[index]; }
    constexpr ConstReference operator [] (uz index) const noexcept { return this->myData[index]; }

    template <typename Unary>
    std::enable_if_t<std::is_nothrow_invocable_r_v<uz, Unary, uz>,
    Reference> operator [] (Unary&& f) noexcept { return this->myData[f(this->myCard)]; }

    template <typename Unary>
    std::enable_if_t<std::is_nothrow_invocable_r_v<uz, Unary, uz>,
    ConstReference> operator [] (Unary&& f) const noexcept { return this->myData[f(this->myCard)]; }

    constexpr Reference      operator () (uz i)       noexcept { return this->myData[i]; }
    constexpr ConstReference operator () (uz i) const noexcept { return this->myData[i]; }

    template <typename Unary>
    constexpr std::enable_if_t<std::is_nothrow_invocable_r_v<uz, Unary, uz>, Reference> operator () (Unary&& fn) noexcept { return this->myData[fn(this->myCard)]; }

    template <typename Unary>
    constexpr std::enable_if_t<std::is_nothrow_invocable_r_v<uz, Unary, uz>, ConstReference> operator () (Unary&& fn) const noexcept { return this->myData[fn(this->myCard)]; }

    constexpr Reference      front()       noexcept { return *this->myData; }
    constexpr ConstReference front() const noexcept { return *this->myData; }

    constexpr Reference      back()       noexcept { return this->myData[this->myCard - 1]; }
    constexpr ConstReference back() const noexcept { return this->myData[this->myCard - 1]; }

public:
    template <typename U>
    Ordering cmp(SliceBaseDereferenceable<U> rhs) const noexcept
    {
        auto const s = std::min(this->card(), rhs.card());
        for ( uz i = 0; i < s; ++i ) {
            if ( operator[](i) < rhs[i] )
                return LT;
            else if ( rhs[i] < operator[](i) )
                return GT;
        }

        if ( this->card() < rhs.card() )
            return LT;
        else if ( rhs.card() < this->card() )
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
        if ( this->card() != rhs.card() )
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

    using Element       = typename Base::Element;
    using Pointer       = typename Base::Pointer;
    using ConstPointer  = typename Base::ConstPointer;
    using Iterator      = typename Base::Iterator;
    using ConstIterator = typename Base::ConstIterator;

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
    Slice operator () (uz start, uz end) const noexcept
    {
        return Slice(this->myData + start, end - start);
    }

    template <typename Unary>
    std::enable_if_t<
        std::is_nothrow_invocable_r_v<uz, Unary, uz>,
        Slice>
    operator () (uz start, Unary&& f) const noexcept
    {
        return Slice(this->myData + start, f(this->myCard) - start);
    }

    constexpr explicit operator bool () const noexcept
    {
        return this->myCard != 0;
    }

    template <typename U>
    Slice<U> cast() noexcept
    {
        return Slice<U>(static_cast<typename Slice<U>::Iterator>(this->begin()),
                        static_cast<typename Slice<U>::Iterator>(this->end()));
    }

    template <typename U>
    Slice<U> cast() const noexcept
    {
        return Slice<U>(static_cast<typename Slice<U>::ConstIterator>(this->begin()),
                        static_cast<typename Slice<U>::ConstIterator>(this->end()));
    }
};

template <typename T> Slice(T*, uz) -> Slice<T>;
template <typename T> Slice(T*, T*) -> Slice<T>;
template <typename T> Slice(std::initializer_list<T>) -> Slice<T const>;
template <typename T> Slice(std::basic_string<T>) -> Slice<T>;
template <typename T> Slice(std::vector<T>) -> Slice<T>;
template <typename T, unsigned N> Slice(T (&)[N]) -> Slice<T>;

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
typename Slice<T>::Iterator begin(Slice<T>& rhs)
{
    return rhs.begin();
}

template <typename T>
typename Slice<T>::ConstIterator begin(Slice<T> const& rhs)
{
    return rhs.begin();
}

template <typename T>
typename Slice<T>::ConstIterator cbegin(Slice<T>& rhs)
{
    return rhs.cbegin();
}

template <typename T>
typename Slice<T>::ConstIterator cbegin(Slice<T> const& rhs)
{
    return rhs.cbegin();
}

template <typename T>
typename Slice<T>::Iterator end(Slice<T>& rhs)
{
    return rhs.end();
}

template <typename T>
typename Slice<T>::ConstIterator end(Slice<T> const& rhs)
{
    return rhs.end();
}

template <typename T>
typename Slice<T>::ConstIterator cend(Slice<T>& rhs)
{
    return rhs.cend();
}

template <typename T>
typename Slice<T>::ConstIterator cend(Slice<T> const& rhs)
{
    return rhs.cend();
}

template <typename T>
Slice<void> type_erase(Slice<T> rhs)
{
    return { (void*)(rhs.data()), rhs.card() * sizeof(T) };
}

//
// sugar

template <typename T>
Slice<T> slice(T* begin, T* end)
{
    auto const card = end - begin;
    return Slice<T>(begin, card);
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
    return s(start, s.card());
}

template <typename T>
Slice<T> slice(T& rhs)
{
    return Slice<T>(&rhs, 1);
}

template <typename T>
Slice<T const*> sliceBox(Box<T> const& rhs)
{
    return Slice<T const*>(const_cast<T const**>(reinterpret_cast<T const* const*>(reinterpret_cast<void const* const*>(&rhs))), 1);
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
