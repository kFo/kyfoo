#pragma once

#include <memory>
#include <vector>
#include <type_traits>

namespace kyfoo {

template <typename T>
struct remove_unique_ptr {
    using type = T;
};

template <typename T>
struct remove_unique_ptr<std::unique_ptr<T>> {
    using type = T*;
};

template <typename T>
using remove_unique_ptr_t = typename remove_unique_ptr<T>::type;

// todo: implement tail const slice

template <typename T>
class Slice
{
public:
    using value_type = remove_unique_ptr_t<T>;
    using pointer = value_type*;
    using const_pointer = value_type const*;
    using reference = value_type&;
    using const_reference = value_type const&;
    using iterator = pointer;
    using const_iterator = const_pointer;
    using size_type = std::size_t;

public:
    Slice() = default;

    using pointee_t = std::remove_pointer_t<value_type>;

    //Slice(std::vector<std::unique_ptr<pointee_t>> const& v,
    //      size_type start,
    //      size_type end)
    //    : myData(reinterpret_cast<value_type*>(const_cast<void*>(reinterpret_cast<void const*>(v.data()))) + start)
    //    , myLength(end - start)
    //{
    //}

    ///*implicit*/ Slice(std::vector<std::unique_ptr<pointee_t>> const& v)
    //    : Slice(v, 0, v.size())
    //{
    //}

    // todo
    template <typename U>
    Slice(std::vector<U> const& v,
          size_type start,
          size_type end)
        : myData(reinterpret_cast<value_type*>(const_cast<void*>(reinterpret_cast<void const*>(v.data()))) + start)
        , myLength(end - start)
    {
    }

    template <typename U>
    Slice(Slice<U> s,
          size_type start,
          size_type end)
        : myData(s.data())
        , myLength(end - start)
    {
    }

    template <typename U>
    /*implicit*/ Slice(std::vector<U> const& v)
        : Slice(v, 0, v.size())
    {
    }

    template <typename U>
    /*implicit*/ Slice(Slice<U> s)
        : Slice(s, 0, s.size())
    {
    }

public:
    const_reference operator [] (std::size_t index) const
    {
        return myData[index];
    }

    Slice operator () (size_type left, size_type right) const
    {
        return Slice(myData + left, right - left);
    }

public:
    iterator begin() { return myData; }
    const_iterator begin() const { return myData; }

    iterator end() { return myData + myLength; }
    const_iterator end() const { return myData + myLength; }

    bool empty() const { return begin() == end(); }

    pointer data() const { return myData; }
    std::size_t length() const { return myLength; }
    std::size_t size() const { return myLength; }

    reference front() { return *myData; }
    const_reference front() const { return *myData; }
    reference back() { return myData[myLength - 1]; }
    const_reference back() const { return myData[myLength - 1]; }

private:
    Slice(pointer data, size_type len)
        : myData(data)
        , myLength(len)
    {
    }

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
inline Slice<T*> slice(std::vector<std::unique_ptr<T>> const& v)
{
    return Slice<T*>(v);
}

template <typename T>
inline Slice<T*> slice(std::vector<std::unique_ptr<T>> const& v, std::size_t start)
{
    return Slice<T*>(v)(start, v.size());
}

template <typename T>
inline Slice<T*> slice(std::vector<std::unique_ptr<T>> const& v, std::size_t start, std::size_t end)
{
    return Slice<T*>(v)(start, end);
}

} // namespace kyfoo
