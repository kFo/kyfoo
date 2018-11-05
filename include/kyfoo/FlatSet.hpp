#pragma once

#include <algorithm>
#include <vector>

namespace kyfoo {

template <typename T>
class FlatSet
{
public:
    using container_type = std::vector<T>;

    using value_type             = typename container_type::value_type            ;
    using size_type              = typename container_type::size_type             ;
    using difference_type        = typename container_type::difference_type       ;
    using reference              = typename container_type::reference             ;
    using const_reference        = typename container_type::const_reference       ;
    using pointer                = typename container_type::pointer               ;
    using const_pointer          = typename container_type::const_pointer         ;
    using iterator               = typename container_type::iterator              ;
    using const_iterator         = typename container_type::const_iterator        ;
    using reverse_iterator       = typename container_type::reverse_iterator      ;
    using const_reverse_iterator = typename container_type::const_reverse_iterator;

public:
    FlatSet() = default;

    template <typename In>
    FlatSet(In first, In const& last)
    {
        myList.reserve(distance(first, last));
        while ( first != last )
            insert(*first++);
    }

    explicit FlatSet(std::initializer_list<value_type> init)
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
    iterator        begin()       noexcept { return myList. begin(); }
    const_iterator  begin() const noexcept { return myList. begin(); }
    const_iterator cbegin() const noexcept { return myList.cbegin(); }

    iterator        end()       noexcept { return myList. end(); }
    const_iterator  end() const noexcept { return myList. end(); }
    const_iterator cend() const noexcept { return myList.cend(); }

    iterator        rbegin()       noexcept { return myList. rbegin(); }
    const_iterator  rbegin() const noexcept { return myList. rbegin(); }
    const_iterator crbegin() const noexcept { return myList.crbegin(); }

    iterator        rend()       noexcept { return myList. rend(); }
    const_iterator  rend() const noexcept { return myList. rend(); }
    const_iterator crend() const noexcept { return myList.crend(); }

    bool empty() const noexcept { return myList.empty(); }
    size_type card() const noexcept { return myList.size(); }
    size_type capacity() const noexcept { return myList.capacity(); }
    size_type max_size() const noexcept { return myList.max_size(); }
    
    reference       operator[](size_type idx)       noexcept { return myList[idx]; }
    const_reference operator[](size_type idx) const noexcept { return myList[idx]; }

    const_iterator lower_bound(const_reference value) const noexcept
    {
        return std::lower_bound(myList.begin(), myList.end(), value);
    }

    iterator lower_bound(const_reference value) noexcept
    {
        return std::lower_bound(myList.begin(), myList.end(), value);
    }

    const_iterator upper_bound(const_reference value) const noexcept
    {
        return std::upper_bound(myList.begin(), myList.end(), value);
    }

    iterator upper_bound(const_reference value) noexcept
    {
        return std::upper_bound(myList.begin(), myList.end(), value);
    }

    const_iterator find(const_reference value) const noexcept
    {
        auto e = lower_bound(value);
        if ( e != end() && *e == value )
            return e;

        return end();
    }

    bool contains(const_reference value) const noexcept
    {
        return find(value) != end();
    }

public:
    void clear() { myList.clear(); }
    void reserve(size_type new_cap ) { myList.reserve(new_cap); }
    void shrink_to_fit() { myList.shrink_to_fit(); }

    std::pair<iterator, bool> insert(const_reference value)
    {
        auto e = lower_bound(value);
        if ( e == myList.end() || *e != value )
            return { myList.insert(e, value), true };

        return { e, false };
    }

    iterator insert(const_iterator hint, const_reference value)
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

    iterator erase(const_iterator pos)
    {
        return myList.erase(pos);
    }

    bool remove(const_reference value)
    {
        auto e = find(value);
        if ( e == end() )
            return false;

        erase(e);
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

        myList.resize(distance(myList.begin(), out));
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

        myList.resize(distance(begin(), out));
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
                myList.resize(distance(begin(), out));
                return *this;
            }
        }

        myList.insert(out, r, rhs.end());
        return *this;
    }

private:
    std::vector<T> myList;
};

template <typename T> auto  begin(FlatSet<T>      & rhs) { return rhs. begin(); }
template <typename T> auto  begin(FlatSet<T> const& rhs) { return rhs. begin(); }
template <typename T> auto cbegin(FlatSet<T> const& rhs) { return rhs.cbegin(); }

template <typename T> auto  end(FlatSet<T>      & rhs) { return rhs. end(); }
template <typename T> auto  end(FlatSet<T> const& rhs) { return rhs. end(); }
template <typename T> auto cend(FlatSet<T> const& rhs) { return rhs.cend(); }

} // namespace kyfoo
