#pragma once

#include <cassert>

#include <kyfoo/Allocators.hpp>
#include <kyfoo/Range.hpp>
#include <kyfoo/Slice.hpp>
#include <kyfoo/Types.hpp>
#include <kyfoo/Utilities.hpp>
#include <kyfoo/allocators/Mallocator.hpp>

namespace kyfoo {

template <typename Derived, typename T>
struct ImplicitSliceMixin
{
    constexpr operator Slice<T      > ()       noexcept { return Slice<T      >(((Derived*)this)->data(), ((Derived*)this)->card()); }
    constexpr operator Slice<T const> () const noexcept { return Slice<T const>(((Derived const*)this)->data(), ((Derived const*)this)->card()); }
};

template <typename Derived, typename T>
struct ImplicitSliceMixin<Derived, T*>
{
    constexpr operator Slice<T      *> ()       noexcept { return Slice<T      *>(((Derived*)this)->data(), ((Derived*)this)->card()); }
    constexpr operator Slice<T const*> () const noexcept { return Slice<T const*>((T const**)((Derived*)this)->data(), ((Derived*)this)->card()); }
};

template <typename Derived, typename T>
struct ImplicitSliceMixin<Derived, Box<T>>
{
    constexpr operator Slice<Box<T      >> ()       noexcept { return Slice<Box<T      >>(((Derived*)this)->data(), ((Derived*)this)->card()); }
    constexpr operator Slice<Box<T const>> () const noexcept { return Slice<Box<T const>>(((Derived const*)this)->data(), ((Derived const*)this)->card()); }

    constexpr operator Slice<T      *> ()       noexcept { return Slice<T      *>((T      **)((Derived*)this)->data(), ((Derived*)this)->card()); }
    constexpr operator Slice<T const*> () const noexcept { return Slice<T const*>((T const**)((Derived const*)this)->data(), ((Derived const*)this)->card()); }
};

template <typename T, uz N>
struct StaticArray
{
    T array[N];

public:
    using Element        = T;
    using Pointer        = T*;
    using ConstPointer   = T const*;
    using Reference      = T&;
    using ConstReference = T const&;
    using Iterator       = Pointer;
    using ConstIterator  = ConstPointer;
    using View           = Slice<Element>;
    using ConstView      = Slice<Element const>;

public:
    void swap(StaticArray& rhs) noexcept
    {
        using kyfoo::swap;
        swap(array, rhs.array);
    }

public:
    constexpr uz card() const noexcept { return N; }

    constexpr Iterator       begin()       noexcept { return array; }
    constexpr ConstIterator  begin() const noexcept { return array; }
    constexpr ConstIterator cbegin()       noexcept { return array; }
    constexpr ConstIterator cbegin() const noexcept { return array; }

    constexpr Iterator       end()       noexcept { return array + N; }
    constexpr ConstIterator  end() const noexcept { return array + N; }
    constexpr ConstIterator cend()       noexcept { return array + N; }
    constexpr ConstIterator cend() const noexcept { return array + N; }

    constexpr Pointer      data()       noexcept { return array; }
    constexpr ConstPointer data() const noexcept { return array; }

    constexpr Reference      operator [] (uz i)       noexcept { return array[i]; }
    constexpr ConstReference operator [] (uz i) const noexcept { return array[i]; }

    constexpr Reference      front()       noexcept { return array[0]; }
    constexpr ConstReference front() const noexcept { return array[0]; }

    constexpr Reference      back()       noexcept { return array[N - 1]; }
    constexpr ConstReference back() const noexcept { return array[N - 1]; }

public:
    constexpr explicit operator bool () const noexcept { return N != 0; }
};

template <typename T>
class StaticArray<T, 0> {};

template <typename... T>
StaticArray(T...) -> StaticArray<std::common_type_t<T...>, sizeof...(T)>;

template <typename T, uz N>
constexpr auto begin(StaticArray<T, N>& rhs) noexcept { return rhs.begin(); }

template <typename T, uz N>
constexpr auto begin(StaticArray<T, N> const& rhs) noexcept { return rhs.begin(); }

template <typename T, uz N>
constexpr auto cbegin(StaticArray<T, N>& rhs) noexcept { return rhs.cbegin(); }

template <typename T, uz N>
constexpr auto cbegin(StaticArray<T, N> const& rhs) noexcept { return rhs.cbegin(); }

template <typename T, uz N>
constexpr auto end(StaticArray<T, N>& rhs) noexcept { return rhs.end(); }

template <typename T, uz N>
constexpr auto end(StaticArray<T, N> const& rhs) noexcept { return rhs.end(); }

template <typename T, uz N>
constexpr auto cend(StaticArray<T, N>& rhs) noexcept { return rhs.cend(); }

template <typename T, uz N>
constexpr auto cend(StaticArray<T, N> const& rhs) noexcept { return rhs.cend(); }

template <typename T, uz N>
constexpr auto view(StaticArray<T,N>& rhs) noexcept { return rhs(); }

template <typename T, uz N>
constexpr auto view(StaticArray<T,N> const& rhs) noexcept { return rhs(); }

template <typename T, typename Allocator = allocators::Mallocator>
class Array
    : protected Allocator
{
public:
    using Element        = T;
    using Pointer        = T*;
    using ConstPointer   = T const*;
    using Reference      = T&;
    using ConstReference = T const&;
    using Iterator       = Pointer;
    using ConstIterator  = ConstPointer;
    using View           = Slice<Element>;
    using ConstView      = Slice<Element const>;

public:
    constexpr Array() noexcept = default;

    /*implicit*/ Array(std::initializer_list<T> list)
        : Array(Slice(list))
    {
    }

    template <typename Range>
    /*implicit*/ Array(Range r) noexcept
    {
        auto m = kyfoo::allocate<Element>(this->allocator(), r.card());
        myData = m.data();
        myCard = m.card();

        auto i = begin();
        for ( auto&& e : r )
            construct(*i++, std::forward<typename Range::Element>(e));
    }

    Array(Array const& rhs)
        : Array(Slice(rhs))
    {
    }

    Array& operator = (Array rhs)
    {
        swap(rhs);
        return *this;
    }

    Array(Array&& rhs) noexcept
        : myData(rhs.myData)
        , myCard(rhs.myCard)
    {
        rhs.release();
    }

    Array& operator = (Array&& rhs)
    {
        clear();
        new (this) Array(std::move(rhs));
        return *this;
    }

    ~Array() noexcept
    {
        clear();
    }

    void swap(Array& rhs) noexcept
    {
        using kyfoo::swap;
        swap(myData, rhs.myData);
        swap(myCard, rhs.myCard);
    }

public:
    constexpr uz card() const noexcept { return myCard; }

    constexpr Iterator      begin ()       noexcept { return myData; }
    constexpr ConstIterator begin () const noexcept { return myData; }
    constexpr ConstIterator cbegin()       noexcept { return myData; }
    constexpr ConstIterator cbegin() const noexcept { return myData; }

    constexpr Iterator      end ()       noexcept { return myData + myCard; }
    constexpr ConstIterator end () const noexcept { return myData + myCard; }
    constexpr ConstIterator cend()       noexcept { return myData + myCard; }
    constexpr ConstIterator cend() const noexcept { return myData + myCard; }

    constexpr Pointer      data()       noexcept { return myData; }
    constexpr ConstPointer data() const noexcept { return myData; }

    constexpr Reference      operator [] (uz i)       noexcept { return myData[i]; }
    constexpr ConstReference operator [] (uz i) const noexcept { return myData[i]; }

    constexpr View      operator () ()       noexcept { return View(myData, myCard); }
    constexpr ConstView operator () () const noexcept { return ConstView(myData, myCard); }

    template <typename Unary> constexpr std::enable_if_t<std::is_nothrow_invocable_r_v<uz, Unary, uz>, Reference>      operator () (Unary&& fn)       noexcept { return myData[fn(myCard)]; }
    template <typename Unary> constexpr std::enable_if_t<std::is_nothrow_invocable_r_v<uz, Unary, uz>, ConstReference> operator () (Unary&& fn) const noexcept { return myData[fn(myCard)]; }

    constexpr Reference      operator () (uz i)       noexcept { return myData[i]; }
    constexpr ConstReference operator () (uz i) const noexcept { return myData[i]; }

    template <typename Unary> std::enable_if_t<std::is_nothrow_invocable_r_v<uz, Unary, uz>, View>      operator () (uz i, Unary&& fn)       noexcept { return View(myData + i, fn(myCard) - i); }
    template <typename Unary> std::enable_if_t<std::is_nothrow_invocable_r_v<uz, Unary, uz>, ConstView> operator () (uz i, Unary&& fn) const noexcept { return ConstView(myData + i, fn(myCard) - i); }

    constexpr View      operator () (uz i, uz j)       noexcept { return View(myData + i, j - i); }
    constexpr ConstView operator () (uz i, uz j) const noexcept { return ConstView(myData + i, j - i); }

    /**
     * \pre card() > 0
     */
    /// \{
    constexpr Reference      front()       noexcept { return myData[0]; }
    constexpr ConstReference front() const noexcept { return myData[0]; }

    constexpr Reference      back()       noexcept { return myData[myCard - 1]; }
    constexpr ConstReference back() const noexcept { return myData[myCard - 1]; }
    /// \}

public:
    Allocator&       allocator()       noexcept { return *this; }
    Allocator const& allocator() const noexcept { return *this; }

public:
    explicit operator bool () const noexcept { return myCard != 0; }

public:
    template <typename Unary>
    std::enable_if_t<std::is_invocable_v<Unary, decltype(*std::declval<Iterator>())>,
        Iterator> find(Iterator i, Unary&& fn)
    {
        for ( auto const end = this->end(); i != end; ++i )
            if ( fn(*i) )
                break;
        return i;
    }

    Iterator find(Iterator i, ConstReference rhs)
    {
        return this->find(i, [&](auto const& e) { return e == rhs; });
    }

    template <typename Unary>
    Iterator find(Unary&& fn)
    {
        return this->find(this->begin(), std::forward<Unary>(fn));
    }

    Iterator find(ConstReference rhs)
    {
        return find(this->begin(), rhs);
    }

    template <typename Unary>
    std::enable_if_t<std::is_invocable_v<Unary, decltype(*std::declval<ConstIterator>())>,
        ConstIterator> find(ConstIterator i, Unary&& fn) const
    {
        for ( auto const end = this->end(); i != end; ++i )
            if ( fn(*i) )
                break;
        return i;
    }

    ConstIterator find(ConstIterator i, ConstReference rhs) const
    {
        return this->find(i, [&](auto const& e) { return e == rhs; });
    }

    template <typename Unary>
    ConstIterator find(Unary&& fn) const
    {
        return this->find(this->begin(), std::forward<Unary>(fn));
    }

    ConstIterator find(ConstReference rhs) const
    {
        return this->find(this->begin(), rhs);
    }

public:
    void clear() noexcept
    {
        this->destruct(this->begin(), this->end());
        this->deallocate(mems(this->begin(), this->end()));
        this->myData = nullptr;
        this->myCard = 0;
    }

    View release() noexcept
    {
        View ret(myData, myCard);
        myData = nullptr;
        myCard = 0;

        return ret;
    }

protected:
    constexpr Array(Pointer data, uz n) noexcept
        : myData(data)
        , myCard(n)
    {
    }

    template <typename... Args>
    void construct(Reference e, Args&&... args) noexcept(noexcept(T(std::forward<Args>(args)...)))
    {
        new (&e) T(std::forward<Args>(args)...);
    }

    void destruct(Reference e) noexcept
    {
        e.~T();
    }

    void destruct(Iterator first, Iterator last) noexcept
    {
        while ( first != last )
            destruct(*first++);
    }

protected:
    Pointer myData = nullptr;
    uz myCard = 0;
};

template <typename T, typename A>
constexpr auto begin(Array<T, A>& rhs) noexcept { return rhs.begin(); }

template <typename T, typename A>
constexpr auto begin(Array<T, A> const& rhs) noexcept { return rhs.begin(); }

template <typename T, typename A>
constexpr auto cbegin(Array<T, A>& rhs) noexcept { return rhs.cbegin(); }

template <typename T, typename A>
constexpr auto cbegin(Array<T, A> const& rhs) noexcept { return rhs.cbegin(); }

template <typename T, typename A>
constexpr auto end(Array<T, A>& rhs) noexcept { return rhs.end(); }

template <typename T, typename A>
constexpr auto end(Array<T, A> const& rhs) noexcept { return rhs.end(); }

template <typename T, typename A>
constexpr auto cend(Array<T, A>& rhs) noexcept { return rhs.cend(); }

template <typename T, typename A>
constexpr auto cend(Array<T, A> const& rhs) noexcept { return rhs.cend(); }

template <typename T, typename A>
constexpr auto view(Array<T,A>& a) noexcept { return a(); }

template <typename T, typename A>
constexpr auto view(Array<T,A> const& a) noexcept { return a(); }

constexpr uz DefaultArrayBuilderGrowFn([[maybe_unused]]uz card,
                                       uz capacity,
                                       uz newSize) noexcept
{
    return max(max(uz(8), capacity * 2), newSize);
}

/**
 * \invariant card() <= capacity()
 * \invariant begin() <= end()
 */
template <typename T,
          typename Allocator = allocators::Mallocator,
          uz (*GrowFn)(uz, uz, uz) = DefaultArrayBuilderGrowFn>
class ArrayBuilder
    : private Array<T, Allocator>
    , public ImplicitSliceMixin<ArrayBuilder<T, Allocator, GrowFn>, T>
{
    using Base = Array<T, Allocator>;

public:
    using Element        = typename Base::Element;
    using Pointer        = typename Base::Pointer;
    using ConstPointer   = typename Base::ConstPointer;
    using Reference      = typename Base::Reference;
    using ConstReference = typename Base::ConstReference;
    using Iterator       = typename Base::Iterator;
    using ConstIterator  = typename Base::ConstIterator;
    using View           = typename Base::View;
    using ConstView      = typename Base::ConstView;

public:
    constexpr ArrayBuilder() noexcept = default;

    explicit ArrayBuilder(uz n)
    {
        reserve(n);
    }

    /*implicit*/ ArrayBuilder(std::initializer_list<T> list)
        : ArrayBuilder(Slice(list))
    {
    }

    template <typename Range>
    /*implicit*/ ArrayBuilder(Range r) noexcept
    {
        appendRange(std::forward<Range>(r));
    }

    ArrayBuilder(ArrayBuilder const& rhs)
        : ArrayBuilder(rhs())
    {
    }

    ArrayBuilder& operator = (ArrayBuilder const& rhs)
    {
        ArrayBuilder(rhs).swap(*this);
        return *this;
    }

    ArrayBuilder(ArrayBuilder&& rhs) noexcept
        : Base(rhs.myData, rhs.myCard)
        , myCapacity(rhs.myCapacity)
    {
        rhs.release();
    }

    ArrayBuilder& operator = (ArrayBuilder&& rhs) noexcept
    {
        clear();
        new (this) ArrayBuilder(std::move(rhs));
        return *this;
    }

    ~ArrayBuilder() noexcept
    {
        clear();
    }

    void swap(ArrayBuilder& rhs) noexcept
    {
        using kyfoo::swap;
        Base::swap(rhs);
        swap(this->myCapacity, rhs.myCapacity);
    }

public:
    using Base::card;
    uz reserved() const noexcept { return this->myCapacity - card(); }
    uz capacity() const noexcept { return this->myCapacity; }

    using Base::begin;
    using Base::cbegin;
    using Base::end;
    using Base::cend;
    using Base::data;

    using Base::operator [];
    using Base::operator ();
    using Base::front;
    using Base::back;

    using Base::allocator;

    using Base::operator bool;

    using Base::find;

public:
    /**
     * \post card() == old card() + 1
     *
     * \throws std::runtime_error Allocator fails to allocate
     */
    template <typename... Args>
    Iterator insert(Iterator i, Args&&... args)
    {
        auto ret = buy(i, 1);
        this->construct(*ret, std::forward<Args>(args)...);

        return ret;
    }

    /**
     * \post card() == old card() + r.card()
     */
    template <typename InputRange>
    std::enable_if_t<is_input_range<InputRange>,
    Iterator> insertRange(Iterator i, InputRange r)
    {
        auto ret = buy(i, r.card());
        auto seat = ret;
        for ( auto&& e : r )
            this->construct(*seat++, std::forward<decltype(e)>(e));

        return ret;
    }

    /**
     * \pre card() >= card(first, last)
     * \post card() == old card() - card(first, last)
     */
    Iterator remove(Iterator first, Iterator last)
    {
        auto const oldEnd = this->end();
        auto const shift = last - first;
        auto const newEnd = oldEnd - shift;
        this->shiftDown(last, shift);
        this->destruct(newEnd, oldEnd);
        return first;
    }

    /**
     * \pre card() >= 1
     * \post card() == old card() - 1
     */
    Iterator remove(Iterator i)
    {
        return remove(i, i + 1);
    }

    template <typename Unary>
    bool zap(Iterator i, Unary&& fn)
    {
        i = this->find(i, std::forward<Unary>(fn));
        if ( i != this->end() ) {
            this->remove(i);
            return true;
        }

        return false;
    }

    bool zap(Iterator i, ConstReference rhs)
    {
        return this->zap(i, [&](auto const& e) { return e == rhs; });
    }

    template <typename Unary>
    bool zap(Unary&& fn)
    {
        return this->zap(this->begin(), std::forward<Unary>(fn));
    }

    bool zap(ConstReference rhs)
    {
        return this->zap(this->begin(), rhs);
    }

    template <typename Unary>
    std::enable_if_t<std::is_invocable_v<Unary, decltype(*std::declval<Iterator>())>,
        bool> zapAll(Unary&& fn)
    {
        auto const end = this->end();
        auto i = this->begin();
        for ( ; i != end; ++i ) {
            if ( fn(*i) )
                goto L_found;
        }

        return false;

    L_found:
        for ( auto j = succ(i); j != end; ++j ) {
            if ( !fn(*j) )
                *i++ = std::move(*j);
        }

        this->destruct(i, end);
        this->myCard = i - this->myData;
        return true;
    }

    bool zapAll(ConstReference rhs)
    {
        return this->zapAll([&](auto const& e) { return e == rhs; });
    }

    /**
     * \post card() == old card() + 1
     */
    template <typename...  Args>
    void append(Args&&... args)
    {
        insert(end(), std::forward<Args>(args)...);
    }

    /**
     * \post card() == old card() + card(r)
     */
    template <typename InputRange>
    std::enable_if_t<is_input_range<InputRange>>
    appendRange(InputRange r)
    {
        insertRange(end(), std::forward<InputRange>(r));
    }

    /**
     * \pre card() >= n
     * \post card() == old card() - n
     */
    void pop(uz n = 1)
    {
        assert( card() >= n );

        this->destruct(end() - n, end());
        this->myCard -= n;
    }

    /**
     * \pre card() >= n
     * \post card() == n
     */
    void trunc(uz n = 0)
    {
        assert( card() >= n );

        this->destruct(this->myData + n, this->end());
        this->myCard = n;
    }

    /**
     * \post card() == 0
     * \post reserved() == 0
     */
    void clear() noexcept
    {
        this->destruct(begin(), end());
        this->deallocate(mems(this->myData, myCapacity));
        this->myData = nullptr;
        this->myCard = 0;
        myCapacity = 0;
    }

    /**
     * \post card() == 0
     */
    Slice<Element> release() noexcept
    {
        Slice ret(this->myData, this->myCapacity);
        this->myData = nullptr;
        this->myCard = 0;
        this->myCapacity = 0;

        return ret;
    }

    /**
     * \post reserved() >= n
     * \post card() == old card()
     */
    void reserve(uz n)
    {
        if ( n <= reserved() )
            return;

        auto const card = this->card();
        auto const capacity = this->capacity();
        auto const newCapacity = GrowFn(card, capacity, card + n);
        Slice mCurrent(this->myData, capacity);
        if ( kyfoo::tryExpand(this->allocator(), mCurrent, newCapacity - capacity) ) {
            this->myCapacity = newCapacity;
            return;
        }

        auto mNew = kyfoo::allocate<Element>(this->allocator(), newCapacity);
        ENFORCE(mNew, "cannot allocate");

        auto dst = mNew.data();
        auto src = mCurrent.data();
        for ( auto i = card; i; --i )
            *dst++ = std::move(*src++);

        kyfoo::deallocate(this->allocator(), mCurrent);

        this->myData = mNew.data();
        this->myCapacity = mNew.card();
    }

protected:
    Iterator buy(Iterator i, uz n)
    {
        if ( n <= reserved() ) {
            shiftUp(i, n);
            return i;
        }

        auto const newCapacity = GrowFn(this->card(), this->capacity(), this->card() + n);

        Slice m(this->myData, this->myCapacity);
        if ( kyfoo::tryExpand(this->allocator(), m, newCapacity - this->myCapacity) ) {
            shiftUp(i, n);
            this->myCapacity = newCapacity;

            return i;
        }

        auto mNew = kyfoo::allocate<Element>(this->allocator(), newCapacity);
        ENFORCE(mNew, "cannot allocate");

        auto index = i - this->myData;
        auto newSeat = mNew.data();
        auto j = this->myData;
        while ( j != i )
            this->construct(*newSeat++, std::move(*j++));

        if ( auto end = this->end(); j != end ) {
            newSeat += n;
            do
                this->construct(*newSeat++, std::move(*j++));
            while ( j != end );
        }

        kyfoo::deallocate(this->allocator(), m);

        this->myData = mNew.data();
        this->myCard += n;
        this->myCapacity = newCapacity;

        return this->myData + index;
    }

    void shiftUp(Iterator first, uz shift) noexcept
    {
        auto const end = this->end();

        auto dst = end + shift;
        auto src = end;

        if ( uz elementsToMove = end - first; elementsToMove <= shift ) {
            while ( elementsToMove-- )
                this->construct(*--dst, std::move(*--src));
        }
        else {
            do
                this->construct(*--dst, std::move(*--src));
            while ( dst != end );

            do
                *--dst = std::move(*--src);
            while ( src != first );
        }

        this->myCard += shift;
    }

    void shiftDown(Iterator first, uz shift) noexcept
    {
        auto const end = this->end();
        auto dst = first - shift;
        while ( first != end )
            *dst++ = std::move(*first++);

        this->myCard -= shift;
    }

private:
    uz myCapacity = 0;
};

template <typename T, typename A, uz (*G)(uz, uz, uz)>
constexpr auto begin(ArrayBuilder<T, A, G>& rhs) noexcept { return rhs.begin(); }

template <typename T, typename A, uz (*G)(uz, uz, uz)>
constexpr auto begin(ArrayBuilder<T, A, G> const& rhs) noexcept { return rhs.begin(); }

template <typename T, typename A, uz (*G)(uz, uz, uz)>
constexpr auto cbegin(ArrayBuilder<T, A, G>& rhs) noexcept { return rhs.cbegin(); }

template <typename T, typename A, uz (*G)(uz, uz, uz)>
constexpr auto cbegin(ArrayBuilder<T, A, G> const& rhs) noexcept { return rhs.cbegin(); }

template <typename T, typename A, uz (*G)(uz, uz, uz)>
constexpr auto end(ArrayBuilder<T, A, G>& rhs) noexcept { return rhs.end(); }

template <typename T, typename A, uz (*G)(uz, uz, uz)>
constexpr auto end(ArrayBuilder<T, A, G> const& rhs) noexcept { return rhs.end(); }

template <typename T, typename A, uz (*G)(uz, uz, uz)>
constexpr auto cend(ArrayBuilder<T, A, G>& rhs) noexcept { return rhs.cend(); }

template <typename T, typename A, uz (*G)(uz, uz, uz)>
constexpr auto cend(ArrayBuilder<T, A, G> const& rhs) noexcept { return rhs.cend(); }

template <typename T, typename Allocator, uz (*GrowFn)(uz, uz, uz)>
constexpr auto view(ArrayBuilder<T, Allocator, GrowFn>& a) noexcept { return a(); }

template <typename T, typename Allocator, uz (*GrowFn)(uz, uz, uz)>
constexpr auto view(ArrayBuilder<T, Allocator, GrowFn> const& a) noexcept { return a(); }

template <typename Allocator = allocators::Mallocator,
          uz (*GrowFn)(uz, uz, uz) = DefaultArrayBuilderGrowFn>
class ArrayBuffer : private ArrayBuilder<char, Allocator, GrowFn>
{
public:
    explicit ArrayBuffer(uz size)
        : ArrayBuilder(size)
    {
    }

public:
    void write(Slice<void const> rhs) noexcept
    {
        insertRange(end(), rhs.cast<char const>());
    }

    Slice<void const> data() const noexcept { return operator Slice<char const> (); }
    Slice<void      > data()       noexcept { return operator Slice<char      > (); }

    using ArrayBuilder::operator Slice<char>;
    using ArrayBuilder::operator Slice<char const>;
    using ArrayBuilder::operator bool;
};

template <typename T>
using a = Array<T>;

template <typename T>
using ab = ArrayBuilder<T>;

} // namespace kyfoo
