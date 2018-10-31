#include <catch2/catch.hpp>

#include <kyfoo/Algorithm.hpp>
#include <kyfoo/Range.hpp>

namespace kyfoo::unittests {

TEST_CASE("fold", "[fold]") {
    auto add = [](auto lhs, auto rhs){ return lhs+rhs; };
    CHECK(fold(add, 0, Iota{10}) ==
          0 + 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9);
    CHECK(fold(add, Iota{10}) == fold(add, 0, Iota{10}));
}

} // namespace kyfoo::unittests
