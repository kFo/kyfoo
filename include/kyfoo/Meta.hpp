#pragma once

#include <type_traits>

#include <kyfoo/Types.hpp>

namespace kyfoo {

namespace details {

struct detect_failed : std::false_type
{
    template<typename U>
    using Default = U;
};

template<typename T>
struct detect_success : std::true_type
{
    using type = T;

    template<typename>
    using Default = T;
};

template<template<typename...> typename T, typename... Args>
auto detect(int) -> decltype(detect_success<T<Args...>>());

template<template<typename...> typename T, typename... Args>
auto detect(...) -> detect_failed;

} // namespace details

template<template<typename...> typename T, typename... Args>
using detect = decltype(details::detect<T, Args...>(0));

#define DEFINE_HAS_METHOD_ARGS(method, ...)                                                                                           \
template <typename T, typename... Args> using has_method_args_##method = decltype(std::declval<T>().method(std::declval<Args>()...)); \
template <typename T> static constexpr bool has_method_##method = detect<has_method_args_##method, T, __VA_ARGS__>::value

#define DEFINE_HAS_METHOD_SIG(method, R, ...)                                                                                     \
template <typename T, typename... Args> using has_method_sig_##method = std::is_same<R, decltype(std::declval<T>().method(std::declval<Args>()...))>; \
template <typename T> static constexpr bool has_method_##method = detect<has_method_sig_##method, T, __VA_ARGS__>::value

template <typename From, typename To>
constexpr bool is_explicitly_convertible =
       std::is_constructible_v<To, From>
    && !std::is_convertible_v<From, To>;

template <typename From, typename To>
constexpr bool is_nothrow_explicitly_convertible =
       std::is_nothrow_constructible_v<To, From>
    && !std::is_convertible_v<From, To>;

template <typename T> using has_index_operator_args = decltype(std::declval<T>()[std::declval<uz>()]);
template <typename T> constexpr bool has_index_operator = detect<has_index_operator_args, T>::value;

} // namespace kyfoo
