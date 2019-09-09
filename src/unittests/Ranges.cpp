#include <catch2/catch.hpp>

#include <kyfoo/Algorithms.hpp>
#include <kyfoo/Range.hpp>
#include <kyfoo/Slice.hpp>

namespace kyfoo::unittests {

static_assert(is_input_range<Slice<int>>);
static_assert(is_bidirectional_range<Slice<int>>);
static_assert(is_random_access_range<Slice<int>>);

TEST_CASE("iota", "[Iota]")
{
    {
        Iota n { 6 };
        for ( auto e : {0, 1, 2, 3, 4, 5} ) {
            REQUIRE(n);
            CHECK(e == n.front());
            n.popFront();
        }
        CHECK(!n);
    }

    {
        Iota n { 3 };
        for ( auto e : {0, 1, 2} ) {
            REQUIRE(n);
            CHECK(e == n.front());
            n.popFront();
        }
        CHECK(!n);
    }

    {
        Iota<int, 2> n { 10, 20 };
        for ( auto e : {10, 12, 14, 16, 18} ) {
            REQUIRE(n);
            CHECK(e == n.front());
            n.popFront();
        }
        CHECK(!n);
    }
}

TEST_CASE("map", "[Map]")
{
    Map sqs { [](int x){return x*x;}, Iota{10} };
    for ( auto e : {0*0, 1*1, 2*2, 3*3, 4*4, 5*5, 6*6, 7*7, 8*8, 9*9} ) {
        REQUIRE(sqs);
        CHECK(sqs.front() == e);
        sqs.popFront();
    }
    CHECK(!sqs);
}

TEST_CASE("retro", "[Retro]")
{
    Retro r { Iota{5} };
    for ( auto e : {4, 3, 2, 1, 0} ) {
        REQUIRE(r);
        CHECK(r.front() == e);
        r.popFront();
    }
    CHECK(!r);
}

TEST_CASE("repeat", "[Repeat][Take]")
{
    Repeat sp { 0 };
    Take r {5, sp};
    REQUIRE(r.card() == 5);
    for ( auto e : {0, 0, 0, 0, 0} ) {
        CHECK(e == r.front());
        r.popFront();
    }

    CHECK(!r);
}

} // namespace kyfoo::unittests
