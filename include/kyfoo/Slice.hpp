#pragma once

#include <memory>
#include <vector>
#include <type_traits>

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
    using size_type = std::size_t;

public:
    Slice() = default;

    Slice(pointer p, size_type len)
        : myData(p)
        , myLength(len)
    {
    }

    /*implicit*/ Slice(std::vector<value_type>& v)
        : Slice(v.data(), v.size())
    {
    }

    template <typename U, typename = std::enable_if_t<std::is_convertible_v<U const*, pointer>>>
    /*implicit*/ Slice(std::vector<U> const& v)
        : Slice(v.data(), v.size())
    {
    }

    template <typename U,
              typename = std::enable_if_t<sizeof(std::unique_ptr<U>) == sizeof(value_type)
                                       && std::is_pointer_v<value_type>
                                       && std::is_convertible_v<U const*, value_type>>>
    /*implicit*/ Slice(std::vector<std::unique_ptr<U>> const& v)
        : myData(reinterpret_cast<pointer>(const_cast<void*>(reinterpret_cast<void const*>(v.data()))))
        , myLength(v.size())
    {
    }

    template <typename U,
              typename = std::enable_if_t<sizeof(std::unique_ptr<U>) == sizeof(value_type)
                                       && std::is_pointer_v<value_type>
                                       && std::is_convertible_v<U*, value_type>>>
    /*implicit*/ Slice(std::vector<std::unique_ptr<U>>& v)
        : myData(reinterpret_cast<pointer>(reinterpret_cast<void*>(v.data())))
        , myLength(v.size())
    {
    }

    /*implicit*/ Slice(Slice const& s)
        : myData(s.myData)
        , myLength(s.myLength)
    {
    }

    template <typename U, typename = std::enable_if_t<std::is_convertible_v<U, value_type>>>
    /*implicit*/ Slice(Slice<U> const& s)
        : Slice(s.data(), s.size())
    {
    }

    Slice& operator = (Slice const& s)
    {
        Slice(s).swap(*this);
        return *this;
    }

    void swap(Slice& s)
    {
        using std::swap;
        swap(myData, s.myData);
        swap(myLength, s.myLength);
    }

    Slice(Slice&& rhs)
        : myData(rhs.myData)
        , myLength(rhs.myLength)
    {
        rhs.clear();
    }

    Slice& operator = (Slice&& rhs)
    {
        new (this) Slice(std::move(rhs));
        rhs.clear();
        return *this;
    }

public:
    reference operator [] (size_type index)
    {
        return myData[index];
    }

    const_reference operator [] (size_type index) const
    {
        return myData[index];
    }

    Slice operator () (size_type start, size_type end)
    {
        return Slice(myData + start, end - start);
    }

    Slice operator () (size_type start, size_type end) const
    {
        return Slice(myData + start, end - start);
    }

    explicit operator bool () const
    {
        return !empty();
    }

public:
    iterator begin() { return myData; }
    const_iterator begin() const { return myData; }

    iterator end() { return myData + myLength; }
    const_iterator end() const { return myData + myLength; }

    bool empty() const { return begin() == end(); }

    pointer data() { return myData; }
    const_pointer data() const { return myData; }

    std::size_t length() const { return myLength; }
    std::size_t size() const { return myLength; }

    reference front() { return *myData; }
    const_reference front() const { return *myData; }

    reference back() { return myData[myLength - 1]; }
    const_reference back() const { return myData[myLength - 1]; }

    void popFront() { ++myData; --myLength; }
    void popBack() { --myLength; }

    void clear()
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
Slice<T*> slice(std::vector<std::unique_ptr<T>> const& v)
{
    return Slice<T*>(v);
}

template <typename T>
Slice<T*> slice(std::vector<std::unique_ptr<T>>& v, std::size_t start)
{
    return Slice<T*>(v)(start, v.size());
}

template <typename T>
Slice<T const*> slice(std::vector<std::unique_ptr<T>> const& v, std::size_t start)
{
    return Slice<T const*>(v)(start, v.size());
}

template <typename T>
Slice<T*> slice(std::vector<std::unique_ptr<T>>& v, std::size_t start, std::size_t end)
{
    return Slice<T*>(v)(start, end);
}

template <typename T>
Slice<T const*> slice(std::vector<std::unique_ptr<T>> const& v, std::size_t start, std::size_t end)
{
    return Slice<T const*>(v)(start, end);
}

template <typename T>
Slice<T> slice(Slice<T> s, std::size_t start)
{
    return s(start, s.size());
}

template <typename T>
Slice<T> slice(T& rhs)
{
    return Slice<T>(&rhs, 1);
}

template <typename T>
Slice<T const> slice(T const& rhs)
{
    return Slice<T const>(&rhs, 1);
}

template <typename T>
Slice<T const*> sliceunq(std::unique_ptr<T> const& rhs)
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
