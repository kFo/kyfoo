#include <catch2/catch.hpp>

#include <kyfoo/Box.hpp>

namespace kyfoo::unittests {

TEST_CASE("Box<int>", "[Box]")
{
    Box<int> b;
    CHECK(!b);
    CHECK(b.get() == nullptr);
    CHECK(b == Box<int>());

    b = new int(42);
    CHECK(b);
    CHECK(*b == 42);

    b.reset(new int(23));
    CHECK(*b == 23);

    auto bb = std::move(b);
    CHECK(!b);
    CHECK(bb);
    CHECK(*bb == 23);
}

} // namespace kyfoo::unittests
