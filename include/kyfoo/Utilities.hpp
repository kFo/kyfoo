#pragma once

#include <algorithm>
#include <functional>
#include <type_traits>
#include <utility>

#ifndef NDEBUG
#define KYFOO_DEBUG_VIRTUAL virtual
#define KYFOO_DEBUG_OVERRIDE override
#else
#define KYFOO_DEBUG_VIRTUAL
#define KYFOO_DEBUG_OVERRIDE
#endif

namespace kyfoo {

// todo: cpp17 replace with deduction guides
template <template<typename> class>
struct deducto_syntacto_impl {};

template <typename F>
struct ycombinator {
    F f;

    template <typename... Args>
    decltype(auto) operator () (Args&&... args) const {
        return f(std::ref(*this), std::forward<Args>(args)...);
    }
};

template <typename F>
ycombinator<std::decay_t<F>> ycomb(F&& f) {
    return { std::forward<F>(f) };
}

template <typename T>
class CheckPoint
{
    T mySaveState;
    T* myRef;

public:
    explicit CheckPoint(T& data) noexcept
        : mySaveState(data)
        , myRef(&data)
    {
    }

    CheckPoint(CheckPoint const&) = delete;
    CheckPoint(CheckPoint&&) = default;
    CheckPoint& operator=(CheckPoint&&) = default;

    ~CheckPoint() noexcept
    {
        restore();
    }

public:
    void restore() noexcept
    {
        *myRef = mySaveState;
    }
};

template <typename T>
CheckPoint<T> operator*(deducto_syntacto_impl<CheckPoint>, T& rhs) noexcept
{
    return CheckPoint<T>(rhs);
}

#define KYFOO_LABEL_CAT(a,b) a##b
#define KYFOO_LABEL_CHECKPOINT(a) KYFOO_LABEL_CAT(check_point_, a)
#define check_point [[maybe_unused]] auto KYFOO_LABEL_CHECKPOINT(__COUNTER__) = deducto_syntacto_impl<CheckPoint>{} *

template <typename F>
struct ScopeExit
{
    F f;

    explicit ScopeExit(F&& f)
        : f(std::forward<F>(f))
    {
    }

    ScopeExit(ScopeExit const&) = delete;
    ScopeExit(ScopeExit&&) = default;
    ScopeExit& operator=(ScopeExit&&) = default;

    ~ScopeExit() { f(); }
};

template <typename F>
ScopeExit<F> operator*(deducto_syntacto_impl<ScopeExit>, F&& f)
{
    return ScopeExit<F>(std::forward<F>(f));
}

#define KYFOO_LABEL_SCOPEEXIT(a) KYFOO_LABEL_CAT(scope_exit_, a)
#define scope_exit [[maybe_unused]] auto KYFOO_LABEL_SCOPEEXIT(__COUNTER__) = deducto_syntacto_impl<ScopeExit>{} * [&]

#define KYFOO_LABEL_REVERT(a) KYFOO_LABEL_CAT(revert_, a)
#define REVERT [[maybe_unused]] auto KYFOO_LABEL_REVERT(__COUNTER__)

#define GUARD_ERR(id) err_guard_##id
#define GUARD(id, expr)                       \
    auto GUARD_ERR(id) = expr;                \
    auto&& id = *GUARD_ERR(id);               \
    if ( std::error_code ec = GUARD_ERR(id) )

template <typename L, typename R>
std::enable_if_t<!std::is_scalar_v<L>>
swap(L& lhs, R& rhs) noexcept
{
    lhs.swap(rhs);
}

template <typename L, typename R>
std::enable_if_t<std::is_scalar_v<L>>
swap(L& lhs, R& rhs) noexcept
{
    using std::swap;
    swap(lhs, rhs);
}

template <typename T>
constexpr T const& min(T const& lhs, T const& rhs) noexcept
{
    if ( rhs < lhs )
        return rhs;

    return lhs;
}

template <typename T>
constexpr T const& max(T const& lhs, T const& rhs) noexcept
{
    if ( lhs < rhs )
        return rhs;

    return lhs;
}

} // namespace kyfoo
