#include <catch2/catch.hpp>

#include <kyfoo/Algorithm.hpp>
#include <kyfoo/Range.hpp>

namespace kyfoo::unittests {

TEST_CASE("fold", "[fold]") {
    auto add = [](auto lhs, auto rhs){ return lhs+rhs; };
    CHECK(fold(Iota{10}, 0, add) ==
          0 + 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9);
    CHECK(fold(Iota{10}, add) == fold(Iota{ 10 }, 0, add));
}

} // namespace kyfoo::unittests
