#pragma once

#include <type_traits>
#include <map>

#include <kyfoo/Slice.hpp>

#define DECL_CLONE(kind)                              \
    auto beginClone(CloneMap& map) const              \
        -> Box<std::decay_t<decltype(*this)>>;        \
    void cloneChildren(kind& c, CloneMap& map) const;

#define DECL_CLONE_NOBASE(kind)                       \
    auto beginClone(CloneMap& map) const              \
        -> Box<std::decay_t<decltype(*this)>>;        \
    void cloneChildren(kind& c, CloneMap& map) const;

#define DECL_CLONE_REMAP \
    void remapReferences(CloneMap const& map);

#define DECL_CLONE_REMAP_NOBASE \
    void remapReferences(CloneMap const& map);

#define DECL_CLONE_ALL(kind) \
    DECL_CLONE(kind)         \
    DECL_CLONE_REMAP

#define DECL_CLONE_ALL_NOBASE(kind) \
    DECL_CLONE_NOBASE(kind)         \
    DECL_CLONE_REMAP_NOBASE

#define IMPL_CLONE(type)                            \
    Box<type> type::beginClone(CloneMap& map) const \
    {                                               \
        Box<type> ret(new type(*this));             \
        map[this] = ret.get();                      \
        cloneChildren(*ret, map);                   \
        return ret;                                 \
    }

#define IMPL_CLONE_NOBASE_BEGIN(type, kind)                       \
    IMPL_CLONE(type)                                              \
    void type::cloneChildren(kind& c[[maybe_unused]],             \
                             CloneMap& map[[maybe_unused]]) const \
    {

#define IMPL_CLONE_BEGIN(type, base, kind) \
    IMPL_CLONE_NOBASE_BEGIN(type, kind)    \
        base::cloneChildren(c, map);

#define IMPL_CLONE_CHILD(member) \
    ((std::decay_t<decltype(*this)>*)&c)->member = ast::beginClone(member, map);

#define IMPL_CLONE_END }

#define IMPL_CLONE_REMAP_NOBASE_BEGIN(type)                         \
    void type::remapReferences(CloneMap const& map[[maybe_unused]]) \
    {

#define IMPL_CLONE_REMAP_BEGIN(type, base) \
    IMPL_CLONE_REMAP_NOBASE_BEGIN(type)    \
        base::remapReferences(map);

#define IMPL_CLONE_REMAP(member) \
    remap(member, map);

#define IMPL_CLONE_REMAP_END }

namespace kyfoo::ast {

using CloneMap = std::map<void const*, void*>;

template <typename T>
Box<std::enable_if_t<!std::is_pointer_v<T>
                  && !is_slice_v<T>, T>>
clone(T const& rhs)
{
    CloneMap map;
    auto ret = beginClone(rhs, map);
    remap(*ret, map);

    return ret;
}

template <typename T, typename D>
Box<T> beginClone(T const* rhs, D& dict)
{
    if ( !rhs )
        return nullptr;

    return beginClone(*rhs, dict);
}

template <typename T>
Box<T> clone(T const* rhs)
{
    if ( !rhs )
        return nullptr;

    return clone(*rhs);
}

template <typename T, typename D>
Box<T> beginClone(Box<T> const& rhs, D& dict)
{
    if ( !rhs )
        return nullptr;

    return Box<T>(static_cast<T*>(beginClone(*rhs, dict).release()));
}

template <typename T>
Box<T> clone(Box<T> const& rhs)
{
    return clone(rhs.get());
}

template <typename T, typename D>
std::vector<Box<std::remove_const_t<T>>> beginClone(Slice<T*> rhs, D& dict)
{
    std::vector<Box<std::remove_const_t<T>>> ret;
    ret.reserve(rhs.card());
    for ( auto e : rhs )
        ret.emplace_back(beginClone(e, dict));

    return ret;
}

template <typename T>
std::vector<Box<std::remove_const_t<T>>> clone(Slice<T*> rhs)
{
    CloneMap map;
    auto ret = beginClone(rhs, map);
    remap(ret, map);
    return ret;
}

template <typename T, typename D>
std::vector<Box<T>> beginClone(std::vector<Box<T>> const& rhs, D& dict)
{
    std::vector<Box<T>> ret;
    ret.reserve(rhs.size());
    for ( auto const& e : rhs )
        ret.emplace_back(beginClone(e, dict));

    return ret;
}

template <typename T>
std::vector<Box<T>> clone(std::vector<Box<T>> const& rhs)
{
    CloneMap map;
    std::vector<Box<T>> ret;
    ret.reserve(rhs.size());
    for ( auto const& e : rhs )
        ret.emplace_back(beginClone(e, map));

    for ( auto& e : ret )
        remap(e, map);

    return ret;
}

template <typename T>
void remap(Box<T>& rhs, CloneMap const& map)
{
    if ( rhs )
        remap(*rhs, map);
}

template <typename T>
void remap(std::vector<T>& rhs, CloneMap const& map)
{
    for ( auto& e : rhs )
        remap(e, map);
}

template <typename T>
void remap(T*& rhs, CloneMap const& map)
{
    auto e = map.find(rhs);
    if ( e != end(map) )
        rhs = reinterpret_cast<T*>(e->second);
}

} // namespace kyfoo::ast
