#pragma once

#include <functional>
#include <type_traits>
#include <utility>

namespace kyfoo {

// todo: cpp17 replace with deduction guides
template <template<typename> class>
struct deducto_syntacto_impl {};

#define KYFOO_LABEL_CAT(a,b) a##b

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

#define KYFOO_LABEL_CHECKPOINT(a) KYFOO_LABEL_CAT(check_point_, a)
#define check_point auto KYFOO_LABEL_CHECKPOINT(__COUNTER__) = deducto_syntacto_impl<CheckPoint>{} *

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

#define KYFOO_LABEL_SCOPEEXIT(a) KYFOO_LABEL_SCOPEEXIT(scope_exit_, a)
#define scope_exit auto KYFOO_LABEL_SCOPEEXIT(__COUNTER__) = deducto_syntacto_impl<ScopeExit>{} * [&]

struct StringComp {
    struct is_transparent;

    bool operator()(std::string const& lhs, std::string const& rhs) const {
        return lhs < rhs;
    }

    bool operator()(std::string const& lhs, std::string_view rhs) const {
        return lhs < rhs;
    }

    bool operator()(std::string_view lhs, std::string const& rhs) const {
        return lhs < rhs;
    }

    bool operator()(std::string_view lhs, std::string_view rhs) const {
        return lhs < rhs;
    }

    bool equiv(std::string const& lhs, std::string const& rhs) const {
        return lhs == rhs;
    }

    bool equiv(std::string const& lhs, std::string_view rhs) const {
        return lhs == rhs;
    }

    bool equiv(std::string_view lhs, std::string const& rhs) const {
        return lhs == rhs;
    }

    bool equiv(std::string_view lhs, std::string_view rhs) const {
        return lhs == rhs;
    }
};

int stoi(std::string_view s);

} // namespace kyfoo
