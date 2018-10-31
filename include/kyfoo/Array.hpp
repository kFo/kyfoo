#pragma once

#include <kyfoo/Allocators.hpp>
#include <kyfoo/Range.hpp>
#include <kyfoo/Slice.hpp>
#include <kyfoo/Types.hpp>
#include <kyfoo/Utilities.hpp>
#include <kyfoo/allocators/Mallocator.hpp>

namespace kyfoo {

template <typename T, uz N>
struct StaticArray
{
    T array[N];

public:
    using value_type      = T;
    using pointer         = T*;
    using const_pointer   = T const*;
    using reference       = T&;
    using const_reference = T const&;
    using iterator        = pointer;
    using const_iterator  = const_pointer;
    using size_type       = uz;

public:
    void swap(StaticArray& rhs) noexcept
    {
        using kyfoo::swap;
        swap(array, rhs.array);
    }

public:
    constexpr size_type size() const noexcept { return N; }

    constexpr iterator       begin ()       noexcept { return array; }
    constexpr const_iterator begin () const noexcept { return array; }
    constexpr const_iterator cbegin()       noexcept { return array; }
    constexpr const_iterator cbegin() const noexcept { return array; }

    constexpr iterator       end ()       noexcept { return array + N; }
    constexpr const_iterator end () const noexcept { return array + N; }
    constexpr const_iterator cend()       noexcept { return array + N; }
    constexpr const_iterator cend() const noexcept { return array + N; }

    constexpr pointer       data()       noexcept { return array; }
    constexpr const_pointer data() const noexcept { return array; }

    constexpr reference       operator [] (size_type i)       noexcept { return array[i]; }
    constexpr const_reference operator [] (size_type i) const noexcept { return array[i]; }

    constexpr reference       front()       noexcept { return array[0]; }
    constexpr const_reference front() const noexcept { return array[0]; }

    constexpr reference       back()       noexcept { return array[N - 1]; }
    constexpr const_reference back() const noexcept { return array[N - 1]; }

public:
    constexpr operator Slice<T>       ()       noexcept { return Slice<T>(array, N); }
    constexpr operator Slice<T const> () const noexcept { return Slice<T>(array, N); }

    constexpr explicit operator bool () const noexcept { return N != 0; }
};

template <typename T>
class StaticArray<T, 0> {};

template <typename... T>
StaticArray(T...) -> StaticArray<std::common_type_t<T...>, sizeof...(T)>;

template <typename T, uz N>
auto begin(StaticArray<T, N>& rhs) { return rhs.begin(); }

template <typename T, uz N>
auto begin(StaticArray<T, N> const& rhs) { return rhs.begin(); }

template <typename T, uz N>
auto cbegin(StaticArray<T, N>& rhs) { return rhs.cbegin(); }

template <typename T, uz N>
auto cbegin(StaticArray<T, N> const& rhs) { return rhs.cbegin(); }

template <typename T, uz N>
auto end(StaticArray<T, N>& rhs) { return rhs.end(); }

template <typename T, uz N>
auto end(StaticArray<T, N> const& rhs) { return rhs.end(); }

template <typename T, uz N>
auto cend(StaticArray<T, N>& rhs) { return rhs.cend(); }

template <typename T, uz N>
auto cend(StaticArray<T, N> const& rhs) { return rhs.cend(); }

template <typename T, typename Allocator = allocators::Mallocator>
class Array : protected Allocator
{
public:
    using value_type      = T;
    using pointer         = T*;
    using const_pointer   = T const*;
    using reference       = T&;
    using const_reference = T const&;
    using iterator        = pointer;
    using const_iterator  = const_pointer;
    using size_type       = uz;

public:
    constexpr Array() noexcept = default;

    /*implicit*/ Array(std::initializer_list<T> list)
        : Array(Slice(list))
    {
    }

    template <typename Range>
    /*implicit*/ Array(Range r) noexcept
    {
        auto m = allocate<value_type>(this->allocator(), r.size());
        myData = m.data();
        mySize = m.size();

        auto i = begin();
        for ( auto&& e : r )
            construct(*i++, std::forward<typename Range::value_type>(e));
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
        , mySize(rhs.mySize)
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
        swap(mySize, rhs.mySize);
    }

public:
    constexpr size_type size() const noexcept { return mySize; }

    constexpr iterator       begin ()       noexcept { return myData; }
    constexpr const_iterator begin () const noexcept { return myData; }
    constexpr const_iterator cbegin()       noexcept { return myData; }
    constexpr const_iterator cbegin() const noexcept { return myData; }

    constexpr iterator       end ()       noexcept { return myData + mySize; }
    constexpr const_iterator end () const noexcept { return myData + mySize; }
    constexpr const_iterator cend()       noexcept { return myData + mySize; }
    constexpr const_iterator cend() const noexcept { return myData + mySize; }

    constexpr pointer       data()       noexcept { return myData; }
    constexpr const_pointer data() const noexcept { return myData; }

    constexpr reference       operator [] (size_type i)       noexcept { return myData[i]; }
    constexpr const_reference operator [] (size_type i) const noexcept { return myData[i]; }

    /**
     * \pre size() > 0
     */
    /// \{
    constexpr reference       front()       noexcept { return myData[0]; }
    constexpr const_reference front() const noexcept { return myData[0]; }

    constexpr reference       back()       noexcept { return myData[mySize - 1]; }
    constexpr const_reference back() const noexcept { return myData[mySize - 1]; }
    /// \}

public:
    Allocator&       allocator()       noexcept { return *this; }
    Allocator const& allocator() const noexcept { return *this; }

public:
    operator Slice<T>       ()       noexcept { return Slice<T>(myData, mySize); }
    operator Slice<T const> () const noexcept { return Slice<T>(myData, mySize); }

    explicit operator bool () const noexcept { return mySize != 0; }

public:
    void clear() noexcept
    {
        destruct(begin(), end());
        this->deallocate(mems(begin(), end()));
        myData = nullptr;
        mySize = 0;
    }

    Slice<value_type> release() noexcept
    {
        Slice<value_type> ret(myData, mySize);
        myData = nullptr;
        mySize = 0;

        return ret;
    }

protected:
    constexpr Array(pointer data, size_type n) noexcept
        : myData(data)
        , mySize(n)
    {
    }

    template <typename... Args>
    void construct(reference e, Args&&... args) noexcept(noexcept(T(args...)))
    {
        new (&e) T(std::forward<Args>(args)...);
    }

    void destruct(reference e) noexcept
    {
        e.~T();
    }

    void destruct(iterator first, iterator last) noexcept
    {
        while ( first != last )
            destruct(*first++);
    }

protected:
    pointer myData = nullptr;
    size_type mySize = 0;
};

constexpr uz DefaultArrayBuilderGrowFn(uz capacity) noexcept
{
    return max(uz(8), capacity * 2);
}

/**
 * \invariant size() <= capacity()
 * \invariant begin() <= end()
 */
template <typename T,
          typename Allocator = allocators::Mallocator,
          uz (*GrowFn)(uz) = DefaultArrayBuilderGrowFn>
class ArrayBuilder : private Array<T, Allocator>
{
    using Base = Array<T, Allocator>;

public:
    using value_type      = typename Base::value_type     ;
    using pointer         = typename Base::pointer        ;
    using const_pointer   = typename Base::const_pointer  ;
    using reference       = typename Base::reference      ;
    using const_reference = typename Base::const_reference;
    using iterator        = typename Base::iterator       ;
    using const_iterator  = typename Base::const_iterator ;
    using size_type       = typename Base::size_type      ;

public:
    constexpr ArrayBuilder() noexcept = default;

    explicit ArrayBuilder(size_type n)
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
        reserve(r.size());
        auto i = begin();
        for ( auto&& e : r )
            this->construct(*i++, std::forward<typename Range::value_type>(e));
    }

    ArrayBuilder(ArrayBuilder const& rhs)
        : ArrayBuilder(Slice(rhs))
    {
    }

    ArrayBuilder& operator = (ArrayBuilder rhs)
    {
        swap(rhs);
        return *this;
    }

    ArrayBuilder(ArrayBuilder&& rhs) noexcept
        : Base(rhs.myData, rhs.mySize)
        , myCapacity(rhs.myCapacity)
    {
        rhs.release();
    }

    ArrayBuilder operator = (ArrayBuilder&& rhs) noexcept
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
    using Base::size;
    size_type reserved() const noexcept { return this->myCapacity - size(); }
    size_type capacity() const noexcept { return this->myCapacity; }

    using Base::begin;
    using Base::cbegin;
    using Base::end;
    using Base::cend;
    using Base::data;

    using Base::operator [];
    using Base::front;
    using Base::back;

    using Base::allocator;

    using Base::operator Slice<value_type>;
    using Base::operator Slice<value_type const>;

    using Base::operator bool;

public:
    /**
     * \post size() == old size() + 1
     *
     * \throws std::runtime_error Allocator fails to allocate
     */
    template <typename... Args>
    iterator insert(const_iterator i, Args&&... args)
    {
        auto ret = buy(i, 1);
        this->construct(*ret, std::forward<Args>(args)...);

        return ret;
    }

    /**
     * \post size() == old size() + r.size()
     */
    template <typename InputRange>
    std::enable_if_t<is_input_range<InputRange>,
    iterator> insertRange(const_iterator i, InputRange r)
    {
        auto ret = buy(i, r.size());
        auto seat = ret;
        for ( auto&& e : r )
            this->construct(*seat++, std::forward<InputRange::value_type>(e));

        return ret;
    }

    /**
     * \pre size() >= distance(first, last)
     * \post size() == old size() - distance(first, last)
     */
    iterator remove(const_iterator first, const_iterator last)
    {
        destruct(first, last);
        shift_down(last, end(), first);
        return first;
    }

    /**
     * \pre size() >= 1
     * \post size() == old size() - 1
     */
    iterator remove(const_iterator i)
    {
        return remove(i, i + 1);
    }

    /**
     * \post size() == old size() + 1
     */
    template <typename...  Args>
    void append(Args&&... args)
    {
        insert(end(), std::forward<Args>(args)...);
    }

    /**
     * \pre size() >= n
     * \post size() == old size() - n
     */
    void trunc(size_type n = 1)
    {
        assert( size() > n );

        this->destruct(end() - n, end());
        this->mySize -= n;
    }

    /**
     * \post size() == 0
     * \post reserved() == 0
     */
    void clear() noexcept
    {
        this->destruct(begin(), end());
        this->deallocate(mems(this->myData, myCapacity));
        this->myData = nullptr;
        this->mySize = 0;
        myCapacity = 0;
    }

    /**
     * \post size() == 0
     */
    Slice<value_type> release() noexcept
    {
        Slice ret(this->myData, this->myCapacity);
        this->myData = nullptr;
        this->mySize = 0;
        this->myCapacity = 0;

        return ret;
    }

public:
    /**
     * \post reserved() >= n
     */
    void reserve(size_type s)
    {
        buy(end(), s);
    }

protected:
    iterator buy(const_iterator i, size_type n)
    {
        iterator mi = const_cast<iterator>(i);
        if ( n < reserved() ) {
            shift_up(mi, end(), mi + n);
            this->mySize += n;
            return mi;
        }

        auto const newCapacity = GrowFn(capacity());

        Slice m(this->myData, this->myCapacity);
        if ( tryExpand(*this, m, newCapacity - this->myCapacity) ) {
            shift_up(mi, end(), mi + n);
            this->mySize += n;
            this->myCapacity = newCapacity;

            return mi;
        }

        if ( !reallocate(this->allocator(), m, newCapacity) )
            throw std::runtime_error("cannot allocate");

        auto index = i - this->myData;
        shift_down(this->myData, mi, m.data());
        shift_down(mi + n, this->end(), m.data() + index + n);

        this->myData = m.data();
        this->mySize += n;
        this->myCapacity = newCapacity;

        return this->myData + index;
    }

    void shift_up(iterator first, iterator last,
                  iterator newSeat) noexcept
    {
        newSeat += last - first - 1;
        while ( first != last )
            *newSeat-- = std::move(*last--);
    }

    void shift_down(iterator first, iterator last,
                    iterator newSeat) noexcept
    {
        while ( first < last )
            *newSeat++ = std::move(*first++);
    }

private:
    size_type myCapacity = 0;
};

} // namespace kyfoo
