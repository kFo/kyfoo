#pragma once

#include <memory>
#include <vector>
#include <type_traits>

namespace kyfoo {

template <typename T>
class Slice
{
    using pointee_t = std::remove_pointer_t<T>;

public:
    Slice() = default;

    Slice(std::vector<std::unique_ptr<pointee_t>> const& v,
          std::size_t start,
          std::size_t end)
        : myData(reinterpret_cast<T*>(const_cast<void*>(reinterpret_cast<void const*>(v.data()))) + start)
        , myLength(end - start)
    {
    }

    /*implicit*/ Slice(std::vector<std::unique_ptr<pointee_t>> const& v)
        : Slice(v, 0, v.size())
    {
    }

    Slice(std::vector<T> const& v,
          std::size_t start,
          std::size_t end)
        : myData(reinterpret_cast<T*>(const_cast<void*>(reinterpret_cast<void const*>(v.data()))) + start)
        , myLength(end - start)
    {
    }

    /*implicit*/ Slice(std::vector<T> const& v)
        : Slice(v, 0, v.size())
    {
    }

public:
    T const& operator [] (std::size_t index) const
    {
        return myData[index];
    }

    Slice operator () (std::size_t left, std::size_t right) const
    {
        return Slice(myData + left, right - left);
    }

public:
    T* begin() { return myData; }
    T const* begin() const { return myData; }

    T const* end() const { return myData + myLength; }
    T* end() { return myData + myLength; }

    bool empty() const { return begin() == end(); }

    T const* data() const { return myData; }
    std::size_t length() const { return myLength; }
    std::size_t size() const { return myLength; }

private:
    Slice(T* data, std::size_t len)
        : myData(data)
        , myLength(len)
    {
    }

    T* myData = nullptr;
    std::size_t myLength = 0;
};

template <typename T>
T* begin(Slice<T>& rhs) { return rhs.begin(); }

template <typename T>
T const* begin(Slice<T> const& rhs) { return rhs.begin(); }

template <typename T>
T* end(Slice<T>& rhs) { return rhs.end(); }

template <typename T>
T const* end(Slice<T> const& rhs) { return rhs.end(); }

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
