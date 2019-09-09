#pragma once

#include <algorithm>

#include <kyfoo/Array.hpp>

namespace kyfoo {

template <typename T>
class FlatSet
{
public:
    using Container = ab<T>;

    using Element        = typename Container::Element;
    using Reference      = typename Container::Reference;
    using ConstReference = typename Container::ConstReference;
    using Pointer        = typename Container::Pointer;
    using ConstPointer   = typename Container::ConstPointer;
    using Iterator       = typename Container::Iterator;
    using ConstIterator  = typename Container::ConstIterator;

public:
    FlatSet() = default;

    template <typename In>
    FlatSet(In first, In const& last)
    {
        myList.reserve(distance(first, last));
        while ( first != last )
            insert(*first++);
    }

    explicit FlatSet(std::initializer_list<Element> init)
        : FlatSet(init.begin(), init.end())
    {
    }

    ~FlatSet() = default;

    void swap(FlatSet& rhs)
    {
        using kyfoo::swap;

        swap(myList, rhs.myList);
    }

public:
    Iterator       begin()       noexcept { return myList. begin(); }
    ConstIterator  begin() const noexcept { return myList. begin(); }
    ConstIterator cbegin() const noexcept { return myList.cbegin(); }

    Iterator       end()       noexcept { return myList. end(); }
    ConstIterator  end() const noexcept { return myList. end(); }
    ConstIterator cend() const noexcept { return myList.cend(); }

    explicit operator bool () const noexcept { return bool(myList); }

    uz card() const noexcept { return myList.card(); }
    uz capacity() const noexcept { return myList.capacity(); }
    uz max_size() const noexcept { return myList.max_size(); }
    
    Reference      operator[](uz idx)       noexcept { return myList[idx]; }
    ConstReference operator[](uz idx) const noexcept { return myList[idx]; }

    ConstIterator lower_bound(ConstReference value) const noexcept
    {
        return std::lower_bound(myList.begin(), myList.end(), value);
    }

    Iterator lower_bound(ConstReference value) noexcept
    {
        return std::lower_bound(myList.begin(), myList.end(), value);
    }

    ConstIterator upper_bound(ConstReference value) const noexcept
    {
        return std::upper_bound(myList.begin(), myList.end(), value);
    }

    Iterator upper_bound(ConstReference value) noexcept
    {
        return std::upper_bound(myList.begin(), myList.end(), value);
    }

    Iterator find(ConstReference value) noexcept
    {
        auto e = lower_bound(value);
        if ( e != end() && *e == value )
            return e;

        return end();
    }

    ConstIterator find(ConstReference value) const noexcept
    {
        return const_cast<FlatSet*>(this)->find(value);
    }

    bool contains(ConstReference value) const noexcept
    {
        return find(value) != end();
    }

public:
    void clear() { myList.clear(); }
    void reserve(uz new_cap ) { myList.reserve(new_cap); }
    void shrink_to_fit() { myList.shrink_to_fit(); }

    std::pair<Iterator, bool> insert(ConstReference value)
    {
        auto e = lower_bound(value);
        if ( e == myList.end() || *e != value )
            return { myList.insert(e, value), true };

        return { e, false };
    }

    Iterator insert(Iterator hint, ConstReference value)
    {
        do {
            if ( value == *hint )
                return hint;

            if ( value < *hint )
                break;

            --hint;
        } while ( hint != myList.begin() );

        return myList.insert(hint, value);
    }

    Iterator remove(Iterator pos)
    {
        return myList.remove(pos);
    }

    bool zap(ConstReference value)
    {
        auto e = find(value);
        if ( e == end() )
            return false;

        remove(e);
        return true;
    }

    FlatSet& unionWith(FlatSet const& rhs) noexcept
    {
        auto r = rhs.begin();
        if ( r == rhs.end() )
            return *this;

        auto l = begin();
        while ( l != end() ) {
            if ( *r < *l ) {
                l = myList.insert(l, *r++);
                ++l;
            }
            else if ( *l < *r ) {
                ++l;
                continue;
            }
            else {
                ++l;
                ++r;
            }

            if ( r == rhs.end() )
                return *this;
        }

        myList.insert(l, r, rhs.end());
        return *this;
    }

    FlatSet& intersectWith(FlatSet const& rhs) noexcept
    {
        auto l = begin();
        auto r = rhs.begin();

        auto out = l;
        while ( l != end() && r != rhs.end() ) {
            if ( *l < *r ) {
                ++l;
            }
            else if ( *r < *l ) {
                ++r;
            }
            else {
                *out++ = *l;
                ++l;
                ++r;
            }
        }

        myList.trunc(std::distance(myList.begin(), out));
        return *this;
    }

    FlatSet& diffWith(FlatSet const& rhs) noexcept
    {
        auto r = rhs.begin();
        if ( r == rhs.end() )
            return *this;

        auto l = begin();
        auto out = l;
        while ( l != end() ) {
            if ( *l < *r ) {
                *out++ = *l++;
                continue;
            }
            else if ( *r < *l ) {
                ++r;
            }
            else {
                ++l;
                ++r;
            }

            if ( r == rhs.end() ) {
                out = std::copy(l, end(), out);
                break;
            }
        }

        myList.trunc(std::distance(begin(), out));
        return *this;
    }

    FlatSet& symmetricDiffWith(FlatSet const& rhs) noexcept
    {
        auto r = rhs.begin();
        if ( r == rhs.end() )
            return *this;

        auto l = begin();
        auto out = l;
        while ( l != end() ) {
            if ( *l < *r ) {
                *out++ = *l++;
                continue;
            }
            else if ( *r < *l ) {
                if ( out != l )
                    *out++ = *r;
                else {
                    l = myList.insert(l, *r);
                    ++l;
                    out = l;
                }

                ++r;
            }
            else {
                ++l;
                ++r;
            }

            if ( r == rhs.end() ) {
                std::copy(l, end(), out);
                return *this;
            }
        }

        while ( out != end() ) {
            *out++ = *r++;
            if ( r == rhs.end() ) {
                myList.trunc(std::distance(begin(), out));
                return *this;
            }
        }

        myList.insertRange(out, iterRange(r, rhs.end()));
        return *this;
    }

private:
    ab<T> myList;
};

template <typename T> auto  begin(FlatSet<T>      & rhs) { return rhs. begin(); }
template <typename T> auto  begin(FlatSet<T> const& rhs) { return rhs. begin(); }
template <typename T> auto cbegin(FlatSet<T> const& rhs) { return rhs.cbegin(); }

template <typename T> auto  end(FlatSet<T>      & rhs) { return rhs. end(); }
template <typename T> auto  end(FlatSet<T> const& rhs) { return rhs. end(); }
template <typename T> auto cend(FlatSet<T> const& rhs) { return rhs.cend(); }

} // namespace kyfoo
