#pragma once

#include <concepts>

#include <kyfoo/Math.hpp>
#include <kyfoo/Meta.hpp>
#include <kyfoo/Types.hpp>

namespace kyfoo {

namespace ranges {

DEFINE_HAS_METHOD_ARGS(front);
DEFINE_HAS_METHOD_ARGS(popFront);
DEFINE_HAS_METHOD_ARGS(back);
DEFINE_HAS_METHOD_ARGS(popBack);
DEFINE_HAS_METHOD_SIG(card, uz);

} // namespace ranges

template <typename Range>
constexpr bool is_input_range =
       std::is_class_v<Range>
    && ranges::has_method_front<Range>
    && ranges::has_method_popFront<Range>
    && is_explicitly_convertible<Range, bool>;

template <typename Range>
constexpr bool is_bidirectional_range =
       is_input_range<Range>
    && ranges::has_method_back<Range>
    && ranges::has_method_popBack<Range>;

template <typename Range>
constexpr bool is_random_access_range =
       is_bidirectional_range<Range>
    && has_index_operator<Range>
    && ranges::has_method_card<Range>;

//
// IteratorAdapter

struct EndIteratorSentinel {};

template <typename Iter>
class IterRange
{
public:
    using Element = typename std::iterator_traits<Iter>::reference;

public:
    IterRange(Iter first, Iter last)
        : myFirst(first)
        , myLast(last)
    {
    }

public:
    Element      & front()       noexcept { return *myFirst; }
    Element const& front() const noexcept { return *myFirst; }

    void popFront() noexcept { ++myFirst; }

    Element& back() noexcept
    {
        auto b = myLast;
        --b;
        return *b;
    }

    Element const& back() const noexcept { return const_cast<IterRange*>(this)->back(); }

    void popBack() noexcept { --myLast; }

    uz card() const noexcept { return std::distance(myFirst, myLast); }

    explicit operator bool () const noexcept { return myFirst == myLast; }

private:
    Iter myFirst;
    Iter myLast;
};

template <typename R>
class RangeIterator
{
public:
    using Element = typename R::Element;

    using value_type      = Element;
    using pointer         = std::remove_reference_t<Element>*;
    using const_pointer   = std::remove_reference_t<Element const>*;
    using reference       = std::remove_reference_t<Element>&;
    using const_reference = std::remove_reference_t<Element const>&;
    using iterator        = pointer;
    using const_iterator  = const_pointer;

public:
    /*implicit*/ RangeIterator(R r)
        : myRange(r)
    {
    }

public:
    auto operator * ()       { return myRange.front(); }
    auto operator * () const { return myRange.front(); }

    RangeIterator operator ++ () { myRange.popFront(); return *this; }
    RangeIterator operator ++ (int)
    {
        auto ret = *this;
        operator++();
        return ret;
    }

    bool operator == (EndIteratorSentinel) { return !bool(myRange); }
    bool operator != (EndIteratorSentinel) { return  bool(myRange); }

private:
    R myRange;
};

//
// List

template <typename T>
class List : public IterRange<T const*>
{
public:
    using Base = IterRange<T const*>;

    using Element = typename Base::Element;

public:
    constexpr /*implicit*/ List(std::initializer_list<T> list) noexcept
        : Base(list.begin(), list.end())
    {
    }
};

//
// Iota

template <typename T, int step = 1>
class Iota
{
public:
    using Element = T;

public:
    constexpr explicit Iota(Element end) noexcept
        : myBegin()
        , myEnd(end)
    {
    }

    constexpr Iota(Element begin, Element end) noexcept
        : myBegin(begin)
        , myEnd(end)
    {
    }

public:
    constexpr explicit operator bool () const noexcept { return myBegin < myEnd; }

public:
    constexpr uz card() const noexcept { return (myEnd - myBegin) / step; }

    constexpr Element front() const noexcept { return myBegin; }
    constexpr Element back() const noexcept { return roundDownToMultiple(myEnd - 1, step); }
    constexpr Element operator[](uz i) const noexcept { return myBegin + step*i; }

    constexpr void popFront() { myBegin += step; }
    constexpr void popBack() { myEnd -= step; }

private:
    Element myBegin;
    Element myEnd;
};

//
// Map

template <typename Fn, typename Range>
class MapInput : protected Fn
{
    using Base = Fn;

public:
    using Element = std::invoke_result_t<Fn, typename Range::Element>;

    MapInput(Fn&& fn, Range&& r)
        : Base(std::forward<Fn>(fn))
        , myRange(std::forward<Range>(r))
    {
    }

public:
    explicit operator bool () const { return bool(myRange); }

public:
    Element front()       { return this->operator()(myRange.front()); }
    Element front() const { return this->operator()(myRange.front()); }

    void popFront() { myRange.popFront(); }

protected:
    Range myRange;
};

template <typename Fn, typename Range>
class MapBidirectional : public MapInput<Fn, Range>
{
    using Base = MapInput<Fn, Range>;

public:
    using Element = typename Base::Element;

    using Base::Base;

    Element back()       { return this->operator()(this->myRange.back()); }
    Element back() const { return this->operator()(this->myRange.back()); }

    void popBack () { this->myRange.popBack(); }
};

template <typename Fn, typename Range>
class MapRandomAccess : public MapBidirectional<Fn, Range>
{
    using Base = MapBidirectional<Fn, Range>;

public:
    using Base::Base;

    using Element = typename Base::Element;

    Element operator[](uz i)       { return this->operator()(this->myRange[i]); }
    Element operator[](uz i) const { return this->operator()(this->myRange[i]); }

    uz card() const noexcept { return this->myRange.card(); }
};

template <typename Fn, typename Range>
class Map
    : public std::conditional_t<is_random_access_range<Range>, MapRandomAccess<Fn, Range>,
        std::conditional_t<is_bidirectional_range<Range>, MapBidirectional<Fn, Range>,
            std::enable_if_t<is_input_range<Range>, MapInput<Fn, Range>>
        >
    >
{
    using Base = std::conditional_t<is_random_access_range<Range>, MapRandomAccess<Fn, Range>,
        std::conditional_t<is_bidirectional_range<Range>, MapBidirectional<Fn, Range>,
            std::enable_if_t<is_input_range<Range>, MapInput<Fn, Range>>
        >
    >;

public:
    using Element = typename Base::Element;

    using Base::Base;
};

template <typename Fn, typename R> Map(Fn, R) -> Map<Fn, R>;

template <typename R>
std::enable_if_t<is_input_range<R>,
RangeIterator<R>> begin(R rhs) { return RangeIterator(rhs); }

template <typename R>
std::enable_if_t<is_input_range<R>,
EndIteratorSentinel> end(R) { return EndIteratorSentinel{}; }

//
// Retro

template <typename R>
class RetroInput
{
public:
    using Element = typename R::Element;

    /*implicit*/ RetroInput(R r)
        : myRange(std::move(r))
    {
    }

public:
    explicit operator bool () const { return bool(myRange); }
    Element front()       { return myRange.back(); }
    Element front() const { return myRange.back(); }

    void popFront() { myRange.popBack(); }

protected:
    R myRange;
};

template <typename R>
class RetroBidirectional : public RetroInput<R>
{
    using Base = RetroInput<R>;

public:
    using Element = typename R::Element;

    using Base::Base;

public:
    Element back()       { return this->myRange.front(); }
    Element back() const { return this->myRange.back(); }

    void popBack() { this->myRange.popFront(); }
};

template <typename R>
class RetroRandomAccess : public RetroBidirectional<R>
{
    using Base = RetroBidirectional<R>;

public:
    using Element = typename R::Element;

    using Base::Base;

public:
    uz card() const { return this->myRange.card(); }

    Element operator [] (uz i)       { return this->myRange[card() - 1 - i]; }
    Element operator [] (uz i) const { return this->myRange[card() - 1 - i]; }
};

template <typename R>
class Retro
    : public std::conditional_t<is_random_access_range<R>, RetroRandomAccess<R>,
        std::enable_if_t<is_bidirectional_range<R>, RetroBidirectional<R>>
    >
{
    using Base = std::conditional_t<is_random_access_range<R>, RetroRandomAccess<R>,
        std::enable_if_t<is_bidirectional_range<R>, RetroBidirectional<R>>
    >;

public:
    using Element = typename Base::Element;
    using Base::Base;
};

template <typename R> Retro(R) -> Retro<R>;

//
// Repeat

template <typename T>
class Repeat
{
public:
    using Element = T;

    explicit Repeat(T const& rhs)
        : myValue(rhs)
    {
    }

public:
    constexpr explicit operator bool () const noexcept { return true; }
    constexpr Element front() const noexcept { return myValue; }
    constexpr void popFront() noexcept { }
    constexpr Element back() const noexcept { return myValue; }
    constexpr void popBack() noexcept { }

    //uz card() const;
    constexpr Element operator [] (uz) const noexcept { return myValue; }

private:
    Element myValue;
};

//
// Take

template <typename R>
class TakeInput
{
public:
    using Element = typename R::Element;

    explicit TakeInput(uz n, R r)
        : myRange(r)
        , myCard(n)
    {
    }

public:
    constexpr explicit operator bool () const noexcept { return myCard != 0; }

    Element front()       { return myRange.front(); }
    Element front() const { return myRange.front(); }
    void popFront() { myRange.popFront(); --myCard; }

    constexpr uz card() const noexcept { return myCard; }

protected:
    R myRange;
    uz myCard;
};

template <typename R>
class TakeBidirectional : public TakeInput<R>
{
    using Base = TakeInput<R>;

public:
    using Element = typename Base::Element;

    using Base::Base;

public:
    Element back()       { return this->myRange.back(); }
    Element back() const { return this->myRange.back(); }
    void popBack() { this->myRange.popBack(); --this->myCard; }
};

template <typename R>
class TakeRandomAccess : public TakeBidirectional<R>
{
    using Base = TakeBidirectional<R>;

public:
    using Element = typename Base::Element;

    using Base::Base;

    Element operator [] (uz i) const noexcept { return this->myRange[i]; }
};

template <typename R>
class Take
    : public std::conditional_t<is_random_access_range<R>, TakeRandomAccess<R>,
        std::conditional_t<is_bidirectional_range<R>, TakeBidirectional<R>,
            std::enable_if_t<is_input_range<R>, TakeInput<R>>
        >
    >
{
    using Base = std::conditional_t<is_random_access_range<R>, TakeRandomAccess<R>,
        std::conditional_t<is_bidirectional_range<R>, TakeBidirectional<R>,
            std::enable_if_t<is_input_range<R>, TakeInput<R>>
        >
    >;

public:
    using Base::Base;
};

template <typename R> Take(uz, R) -> Take<R>;

//
// Replicate

template <typename T>
class Replicate
{
public:
    using Element = T;

public:
    template <typename... Args>
    /*implicit*/ Replicate(uz card, Args... args)
        : myCard(card)
        , myReplicant(std::forward<Args>(args)...)
    {
    }

public:
    constexpr Element front() const noexcept { return {}; }
    constexpr void popFront() noexcept { --myCard; }

    constexpr explicit operator bool () const noexcept { return !myCard; }

    constexpr Element back() const noexcept { return front(); }
    constexpr void popBack() noexcept { return popFront(); }

    constexpr uz card() const noexcept { return myCard; }

    constexpr Element operator [] (uz) const noexcept { return front(); }

private:
    uz myCard;
    T myReplicant;
};

//
// MoveRange

template <typename R>
class MoveRange
{
public:
    using Element = typename R::Element&&;

public:
    explicit MoveRange(R r)
        : myRange(r)
    {
    }

public:
    Element front() { return std::move(myRange.front()); }
    uz card() const { return myRange.card(); }

    explicit operator bool () const { return bool(myRange); }

    void popFront() noexcept { return myRange.popFront(); }

private:
    R myRange;
};

} // namespace kyfoo
