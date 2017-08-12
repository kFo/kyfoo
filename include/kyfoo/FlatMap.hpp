#pragma once

#include <algorithm>
#include <tuple>
#include <vector>

#include <kyfoo/Slice.hpp>
#include <kyfoo/ast/Node.hpp>

namespace kyfoo {

template <typename Key, typename Value>
class FlatMap
{
public:
    template <typename K, typename V>
    struct KeyValue
    {
        K key;
        V value;

        KeyValue(K&& k, V&& v)
            : key(std::forward<K>(k))
            , value(std::forward<V>(v))
        {
        }

        bool operator == (KeyValue const& rhs) const
        {
            return key == rhs.key
                && value == rhs.value;
        }

        bool operator != (KeyValue const& rhs) const
        {
            return !operator==(rhs);
        }
    };

    using size_type = std::size_t;
    using value_type = KeyValue<Key, Value>;
    using pointer = KeyValue<Key*, Value*>;
    using const_pointer = KeyValue<Key const*, Value const*>;
    using reference = KeyValue<Key&, Value&>;
    using const_reference = KeyValue<Key const&, Value const&>;

    template <typename K, typename V>
    struct iterator_base : public KeyValue<K, V>
    {
        using KeyValue::KeyValue;

        reference operator * ()
        {
            return reference(*key, *value);
        }

        const_reference operator * () const
        {
            return const_reference(*key, *value);
        }

        iterator_base& operator++()
        {
            ++key;
            ++value;
            return *this;
        }

        iterator_base operator++(int)
        {
            auto ret = *this;
            operator++();
            return ret;
        }
    };

    using iterator = iterator_base<Key*, Value*>;
    using const_iterator = iterator_base<Key const*, Value const*>;

public:
    FlatMap() = default;

    FlatMap(std::initializer_list<Key> const& keys,
            std::initializer_list<Value> const& values)
        : myKeys(keys)
        , myValues(values)
    {
    }

    FlatMap(FlatMap const& rhs) = default;
    FlatMap& operator = (FlatMap const& rhs) = default;

    FlatMap(FlatMap&& rhs) = default;
    FlatMap& operator = (FlatMap&& rhs) = default;

    ~FlatMap() = default;

    void swap(FlatMap& rhs)
    {
        using std::swap;
        swap(myKeys, rhs.myKeys);
        swap(myValues, rhs.myValues);
    }

public:
    Slice<Key> keys()
    {
        return myKeys;
    }

    Slice<Key> const keys() const
    {
        return myKeys;
    }

    Slice<Value> values()
    {
        return myValues;
    }

    Slice<Value> const values() const
    {
        return myValues;
    }

    size_type findKeyIndex(Key const& rhs) const
    {
        return distance(myKeys.begin(), find(myKeys.begin(), myKeys.end(), rhs));
    }

    size_type findValueIndex(Value const& rhs) const
    {
        return distance(myValues.begin(), find(myValues.back(), myValues.end(), rhs));
    }

public:
    iterator begin() { return { myKeys.data(), myValues.data() }; }
    const_iterator begin() const { return { myKeys.data(), myValues.data() }; }
    iterator end() { return { myKeys.data() + size(), myValues.data() + size() }; }
    const_iterator end() const { return { myKeys.data() + size(), myValues.data() + size() }; }

    std::size_t size() const
    {
        return myKeys.size();
    }

    bool empty() const
    {
        return size() == 0;
    }

    void push_back(Key const& key, Value const& value)
    {
        myKeys.push_back(key);
        myValues.push_back(value);
    }

    void emplace_back(Key&& key, Value&& value)
    {
        myKeys.emplace_back(std::forward<Key>(key));
        myValues.emplace_back(std::forward<Value>(value));
    }

private:
    std::vector<Key> myKeys;
    std::vector<Value> myValues;
};

template <typename Key, typename Value>
typename FlatMap<Key, Value>::iterator begin(FlatMap<Key, Value>& rhs) { return rhs.begin(); }

template <typename Key, typename Value>
typename FlatMap<Key, Value>::const_iterator begin(FlatMap<Key, Value> const& rhs) { return rhs.begin(); }

template <typename Key, typename Value>
typename FlatMap<Key, Value>::iterator end(FlatMap<Key, Value>& rhs) { return rhs.end(); }

template <typename Key, typename Value>
typename FlatMap<Key, Value>::const_iterator end(FlatMap<Key, Value> const& rhs) { return rhs.end(); }

    namespace ast {
        template <typename Key, typename Value, typename Dict>
        FlatMap<Key, Value> clone(FlatMap<Key, Value>& map, Dict& dict)
        {
            FlatMap<Key, Value> ret;
            for ( std::size_t i = 0; i < map.size(); ++i )
                ret.emplace_back(clone(map.keys()[i]), clone(map.values()[i]));

            return ret;
        }

        template <typename Key, typename Value>
        void remap(FlatMap<Key, Value>& rhs, clone_map_t const& map)
        {
            for ( auto& e : rhs.values() )
                remap(e, map);
        }
    } // namespace ast
} // namespace kyfoo
