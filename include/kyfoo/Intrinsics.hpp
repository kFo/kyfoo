#pragma once

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

namespace kyfoo::intrinsics {

inline unsigned int       clz(unsigned int       x) { return __builtin_clz  (x); }
inline unsigned long      clz(unsigned long      x) { return __builtin_clzl (x); }
inline unsigned long long clz(unsigned long long x) { return __builtin_clzll(x); }
DEFINE_HAS_INTRIN(clz);

inline unsigned int       ctz(unsigned int       x) { return __builtin_ctz  (x); }
inline unsigned long      ctz(unsigned long      x) { return __builtin_ctzl (x); }
inline unsigned long long ctz(unsigned long long x) { return __builtin_ctzll(x); }
DEFINE_HAS_INTRIN(ctz);

inline unsigned int       popcount(unsigned int       x) { return __builtin_popcount  (x); }
inline unsigned long      popcount(unsigned long      x) { return __builtin_popcountl (x); }
inline unsigned long long popcount(unsigned long long x) { return __builtin_popcountll(x); }
DEFINE_HAS_INTRIN(popcount);

} // namespace intrinsics
