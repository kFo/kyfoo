#pragma once

#include <kyfoo/Math.hpp>
#include <kyfoo/Meta.hpp>
#include <kyfoo/Types.hpp>

namespace kyfoo {

namespace ranges {

DEFINE_HAS_METHOD_SIG(front);
DEFINE_HAS_METHOD_SIG(popFront);

DEFINE_HAS_METHOD_SIG(back);
DEFINE_HAS_METHOD_SIG(popBack);

DEFINE_HAS_METHOD(card, uz);

} // namespace ranges

template <typename Range>
constexpr bool is_input_range =
       std::is_class_v<Range>
    && ranges::has_method_sig_front_v<Range>
    && ranges::has_method_sig_popFront_v<Range>
    && is_explicitly_convertible<Range, bool>;

template <typename Range>
constexpr bool is_bidirectional_range =
       is_input_range<Range>
    && ranges::has_method_sig_back_v<Range>
    && ranges::has_method_sig_popBack_v<Range>;

template <typename Range>
constexpr bool is_random_access_range =
       is_bidirectional_range<Range>
    && has_index_operator_v<Range>
    && ranges::has_method_card_v<Range>;

//
// Iota

template <typename T, int step = 1>
class Iota
{
public:
    using value_type = T;

public:
    constexpr explicit Iota(T end) noexcept
        : myBegin(T())
        , myEnd(end)
    {
    }

    constexpr Iota(T begin, T end) noexcept
        : myBegin(begin)
        , myEnd(end)
    {
    }

public:
    constexpr explicit operator bool () const noexcept { return myBegin < myEnd; }

public:
    constexpr uz card() const noexcept { return (myEnd - myBegin) / step; }

    constexpr T front() const noexcept { return myBegin; }
    constexpr T back() const noexcept { return roundDownToMultiple(myEnd - 1, step); }
    constexpr T operator[](uz i) const noexcept { return myBegin + step*i; }

    constexpr void popFront() { myBegin += step; }
    constexpr void popBack() { myEnd -= step; }

private:
    T myBegin;
    T myEnd;
};

//
// Map

template <typename Fn, typename Range>
class MapInput : protected Fn
{
    using Base = Fn;

public:
    using value_type = std::invoke_result_t<Fn, typename Range::value_type>;

    MapInput(Fn&& fn, Range&& r)
        : Base(std::forward<Fn>(fn))
        , myRange(std::forward<Range>(r))
    {
    }

public:
    explicit operator bool () const { return bool(myRange); }

public:
    value_type front()       { return this->operator()(myRange.front()); }
    value_type front() const { return this->operator()(myRange.front()); }

    void popFront() { myRange.popFront(); }

protected:
    Range myRange;
};

template <typename Fn, typename Range>
class MapBidirectional : public MapInput<Fn, Range>
{
    using Base = MapInput<Fn, Range>;

public:
    using value_type = typename Base::value_type;

    using Base::Base;

    value_type back()       { return this->operator()(this->myRange.back()); }
    value_type back() const { return this->operator()(this->myRange.back()); }

    void popBack () { this->myRange.popBack(); }
};

template <typename Fn, typename Range>
class MapRandomAccess : public MapBidirectional<Fn, Range>
{
    using Base = MapBidirectional<Fn, Range>;

public:
    using Base::Base;

    using value_type = typename Base::value_type;

    value_type operator[](uz i)       { return this->operator()(this->myRange[i]); }
    value_type operator[](uz i) const { return this->operator()(this->myRange[i]); }

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
    using value_type = typename Base::value_type;

    using Base::Base;
};

template <typename Fn, typename R> Map(Fn, R) -> Map<Fn, R>;

//
// Retro

template <typename R>
class RetroInput
{
public:
    using value_type = typename R::value_type;

    /*implicit*/ RetroInput(R r)
        : myRange(std::move(r))
    {
    }

public:
    explicit operator bool () const { return bool(myRange); }
    value_type front()       { return myRange.back(); }
    value_type front() const { return myRange.back(); }

    void popFront() { myRange.popBack(); }

protected:
    R myRange;
};

template <typename R>
class RetroBidirectional : public RetroInput<R>
{
    using Base = RetroInput<R>;

public:
    using value_type = typename R::value_type;

    using Base::Base;

public:
    value_type back()       { return myRange.front(); }
    value_type back() const { return myRange.back(); }

    void popBack() { myRange.popFront(); }
};

template <typename R>
class RetroRandomAccess : public RetroBidirectional<R>
{
    using Base = RetroBidirectional<R>;

public:
    using value_type = typename R::value_type;

    using Base::Base;

public:
    uz card() const { return myRange.card(); }

    value_type operator [] (uz i)       { return myRange[card() - 1 - i]; }
    value_type operator [] (uz i) const { return myRange[card() - 1 - i]; }
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
    using value_type = typename Base::value_type;
    using Base::Base;
};

template <typename R> Retro(R) -> Retro<R>;

//
// Repeat

template <typename T>
class Repeat
{
public:
    using value_type = T;

    explicit Repeat(T const& rhs)
        : myValue(rhs)
    {
    }

public:
    constexpr explicit operator bool () const noexcept { return true; }
    constexpr value_type front() const noexcept { return myValue; }
    constexpr void popFront() noexcept { }
    constexpr value_type back() const noexcept { return myValue; }
    constexpr void popBack() noexcept { }

    //uz card() const;
    constexpr value_type operator [] (uz) const noexcept { return myValue; }

private:
    value_type myValue;
};

//
// Take

template <typename R>
class TakeInput
{
public:
    using value_type = typename R::value_type;

    explicit TakeInput(uz n, R r)
        : myRange(r)
        , myCard(n)
    {
    }

public:
    constexpr explicit operator bool () const noexcept { return myCard != 0; }

    value_type front()       { return myRange.front(); }
    value_type front() const { return myRange.front(); }
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
    using value_type = typename Base::value_type;

    using Base::Base;

public:
    value_type back()       { return this->myRange.back(); }
    value_type back() const { return this->myRange.back(); }
    void popBack() { this->myRange.popBack(); --this->myCard; }
};

template <typename R>
class TakeRandomAccess : public TakeBidirectional<R>
{
    using Base = TakeBidirectional<R>;

public:
    using value_type = typename Base::value_type;

    using Base::Base;

    value_type operator [] (uz i) const noexcept { return this->myRange[i]; }
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

} // namespace kyfoo
