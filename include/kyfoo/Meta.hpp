#pragma once

#include <type_traits>

namespace kyfoo {

#define DEFINE_HAS_METHOD(method, R, ...)                                                    \
template <typename C>                                                                        \
class has_method_##method {                                                                  \
    static std::true_type match(R (C::*)(__VA_ARGS__));                                      \
    static std::true_type match(R (C::*)(__VA_ARGS__) const);                                \
                                                                                             \
    struct pick {};                                                                          \
    template <typename T> static decltype(match(&T::method)) test(pick);                     \
    template <typename  > static std::false_type             test(...);                      \
                                                                                             \
public:                                                                                      \
    using type = decltype(test<C>(pick{}));                                                  \
    constexpr static bool value = type::value;                                               \
};                                                                                           \
                                                                                             \
template <typename C> using has_method_##method##_t = typename has_method_##method<C>::type; \
template <typename C> constexpr bool has_method_##method##_v = has_method_##method<C>::value

#define DEFINE_HAS_METHOD_SIG(method, ...)                                          \
template <typename C>                                                               \
class has_method_sig_##method {                                                     \
    template <typename R> static std::true_type match(R (C::*)(__VA_ARGS__));       \
    template <typename R> static std::true_type match(R (C::*)(__VA_ARGS__) const); \
                                                                                    \
    struct pick {};                                                                 \
    template <typename T> static decltype(match(&T::method)) test(pick);            \
    template <typename  > static std::false_type             test(...);             \
                                                                                    \
public:                                                                             \
    using type = decltype(test<C>(pick{}));                                         \
    constexpr static bool value = type::value;                                      \
};                                                                                  \
                                                                                    \
template <typename C>                                                               \
using has_method_sig_##method##_t = typename has_method_sig_##method<C>::type;      \
template <typename C>                                                               \
constexpr bool has_method_sig_##method##_v = has_method_sig_##method<C>::value

} // namespace kyfoo
