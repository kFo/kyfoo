#pragma once

#ifdef _MSC_VER
#include <intrin.h>
#endif

#define DEFINE_HAS_INTRIN(NAME)                      \
    template <typename R, typename... Args>          \
    class has_##NAME {                               \
        static std::true_type match(R (*)(Args...)); \
                                                     \
        struct pick {};                              \
        static decltype(match(&NAME)) test(pick);    \
        static std::false_type         test(...);    \
                                                     \
    public:                                          \
        using type = decltype(test(pick{}));         \
        constexpr static bool value = type::value;   \
    };                                               \
    \
    template <typename R, typename... Args> using has_##NAME##_t = typename has_##NAME<R, Args...>::type; \
    template <typename R, typename... Args> constexpr bool has_##NAME##_v = has_##NAME<R, Args...>::value

#define DEFINE_HASNOT_INTRIN(NAME) \
    template <typename, typename...>         \
    using has_##NAME## = std::false_type;    \
    template <typename R, typename... Args> using has_##NAME##_t = typename has_##NAME<R, Args...>::type; \
    template <typename R, typename... Args> constexpr bool has_##NAME##_v = has_##NAME<R, Args...>::value

namespace kyfoo::intrinsics {

#if defined(__clang__) || defined(__GNUC__)

//
// Count Leading Zeroes

inline unsigned int       clz(unsigned int       x) { return __builtin_clz  (x); }
inline unsigned long      clz(unsigned long      x) { return __builtin_clzl (x); }
inline unsigned long long clz(unsigned long long x) { return __builtin_clzll(x); }
DEFINE_HAS_INTRIN(clz);

//
// Count Trailing Zeroes

inline unsigned int       ctz(unsigned int       x) { return __builtin_ctz  (x); }
inline unsigned long      ctz(unsigned long      x) { return __builtin_ctzl (x); }
inline unsigned long long ctz(unsigned long long x) { return __builtin_ctzll(x); }
DEFINE_HAS_INTRIN(ctz);

//
// Population Count

inline unsigned int       popcount(unsigned int       x) { return __builtin_popcount  (x); }
inline unsigned long      popcount(unsigned long      x) { return __builtin_popcountl (x); }
inline unsigned long long popcount(unsigned long long x) { return __builtin_popcountll(x); }
DEFINE_HAS_INTRIN(popcount);

#elif defined(_MSC_VER)

inline unsigned short   clz(unsigned short   x) { return __lzcnt16(x); }
inline unsigned int     clz(unsigned int     x) { return __lzcnt  (x); }
inline unsigned __int64 clz(unsigned __int64 x) { return __lzcnt64(x); }
DEFINE_HAS_INTRIN(clz);

DEFINE_HASNOT_INTRIN(ctz);

inline unsigned short   popcount(unsigned short   x) { return __popcnt16(x); }
inline unsigned int     popcount(unsigned int     x) { return __popcnt  (x); }
inline unsigned __int64 popcount(unsigned __int64 x) { return __popcnt64(x); }
DEFINE_HAS_INTRIN(popcount);

#else

DEFINE_HASNOT_INTRIN(clz);
DEFINE_HASNOT_INTRIN(ctz);
DEFINE_HASNOT_INTRIN(popcount);

#endif

} // namespace intrinsics
