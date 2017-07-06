#pragma once

#include <type_traits>
#include <map>

#include <kyfoo/ast/IO.hpp>

#define DECL_CLONE(kind)                                          \
    auto clone(clone_map_t& map) const                            \
        -> std::decay_t<decltype(*this)>* override;               \
    void cloneChildren(kind& c, clone_map_t& map) const override;

#define DECL_CLONE_REMAP \
    void remapReferences(clone_map_t const& map) override;

#define DECL_CLONE_ALL(kind) \
    DECL_CLONE(kind)         \
    DECL_CLONE_REMAP

#define IMPL_CLONE(type)                      \
    type* type::clone(clone_map_t& map) const \
    {                                         \
        auto ret = new type(*this);           \
        map[this] = ret;                      \
        cloneChildren(*ret, map);             \
        return ret;                           \
    }

#define IMPL_CLONE_NOBASE_BEGIN(type, kind)                   \
    IMPL_CLONE(type)                                          \
    void type::cloneChildren(kind& c, clone_map_t& map) const \
    {

#define IMPL_CLONE_BEGIN(type, base, kind) \
    IMPL_CLONE_NOBASE_BEGIN(type, kind)    \
        base::cloneChildren(c, map);

#define IMPL_CLONE_CHILD(member) \
    ((std::decay_t<decltype(*this)>*)&c)->member = ast::clone(member, map);

#define IMPL_CLONE_END }

#define IMPL_CLONE_REMAP_NOBASE_BEGIN(type)            \
    void type::remapReferences(clone_map_t const& map) \
    {

#define IMPL_CLONE_REMAP_BEGIN(type, base) \
    IMPL_CLONE_REMAP_NOBASE_BEGIN(type)    \
        base::remapReferences(map);

#define IMPL_CLONE_REMAP(member) \
    remap(member, map);

#define IMPL_CLONE_REMAP_END }

#define IMPL_CLONE_REMAP_NONE(type) \
    void type::remapReferences(clone_map_t const&) {}

namespace kyfoo {
    namespace ast {

class INode : public IIO
{
public:
    virtual ~INode() = default;
};

using clone_map_t = std::map<void const*, void*>;

template <typename T>
std::unique_ptr<T> clone(std::unique_ptr<T> const& rhs)
{
    return clone(rhs.get());
}

template <typename T>
std::unique_ptr<T> clone(T const* rhs)
{
    if ( !rhs )
        return nullptr;

    clone_map_t map;
    std::unique_ptr<T> ret(rhs->clone(map));
    ret->remapReferences(map);
    return ret;
}

template <typename T>
std::vector<std::unique_ptr<T>> clone(std::vector<std::unique_ptr<T>> const& rhs)
{
    clone_map_t map;
    std::vector<std::unique_ptr<T>> ret;
    ret.reserve(rhs.size());
    for ( auto const& e : rhs )
        ret.emplace_back(std::unique_ptr<T>(e->clone(map)));

    for ( auto& e : ret )
        e->remapReferences(map);

    return ret;
}

template <typename T, typename D>
std::unique_ptr<T> clone(std::unique_ptr<T> const& rhs, D& dict)
{
    if ( !rhs )
        return nullptr;

    return std::unique_ptr<T>(rhs->clone(dict));
}

template <typename T, typename D>
std::vector<std::unique_ptr<T>> clone(std::vector<std::unique_ptr<T>> const& rhs,
                                      D& dict)
{
    std::vector<std::unique_ptr<T>> ret;
    ret.reserve(rhs.size());
    for ( auto const& e : rhs )
        ret.emplace_back(std::unique_ptr<T>(e->clone(dict)));

    return ret;
}

template <typename T>
void remap(std::unique_ptr<T>& rhs, clone_map_t const& map)
{
    if ( rhs )
        rhs->remapReferences(map);
}

template <typename T>
void remap(std::vector<T>& rhs, clone_map_t const& map)
{
    for ( auto& e : rhs )
        remap(e, map);
}

template <typename T>
void remap(T*& rhs, clone_map_t const& map)
{
    auto e = map.find(rhs);
    if ( e != end(map) )
        rhs = (T*)e->second;
}

    } // namespace ast
} // namespace kyfoo
