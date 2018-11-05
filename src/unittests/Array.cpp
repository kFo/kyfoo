#include <catch2/catch.hpp>

#include <kyfoo/Array.hpp>
#include <array>

namespace kyfoo::unittests {

TEST_CASE("static array", "[StaticArray]") {
    StaticArray a { 1, 2, 3 };
    REQUIRE(a.card() == 3);
    CHECK(sizeof(a) == sizeof(int[3]));

    for ( int i = 0; i < 3; ++i )
        CHECK(a[i] == i+1);
}

TEST_CASE("dynamic array", "[Array]") {
    Array a { 1, 2, 3 };
    REQUIRE(a.card() == 3);

    for ( int i = 0; i < 3; ++i )
        CHECK(a[i] == i+1);

    auto b = std::move(a);
    CHECK(!a);
    CHECK(b.card() == 3);
}

TEST_CASE("array builder", "ArrayBuilder") {
    ArrayBuilder a { 1, 2, 3 };
    REQUIRE(a.card() == 3);

    for ( int i = 0; i < 3; ++i )
        CHECK(a[i] == i+1);

    auto b = std::move(a);
    CHECK(!a);
    CHECK(b.card() == 3);

    b.append(4);
    CHECK(b.back() == 4);

    b.trunc();
    CHECK(b.back() == 3);
}

} // namespace kyfoo::unittests
