#include <catch2/catch.hpp>

#include <kyfoo/Algorithms.hpp>
#include <kyfoo/Array.hpp>
#include <kyfoo/Box.hpp>
#include <kyfoo/Dollar.hpp>
#include <kyfoo/Range.hpp>
#include <array>

namespace kyfoo::unittests {

TEST_CASE("StaticArray", "[StaticArray]")
{
    StaticArray a { 1, 2, 3 };
    REQUIRE(a.card() == 3);
    CHECK(sizeof(a) == sizeof(int[3]));

    for ( int i = 0; i < 3; ++i )
        CHECK(a[i] == i+1);
}

TEST_CASE("Array<int>", "[Array]")
{
    Array a { 1, 2, 3 };
    REQUIRE(a.card() == 3);

    for ( int i = 0; i < 3; ++i )
        CHECK(a[i] == i+1);

    auto b = std::move(a);
    CHECK(!a);
    CHECK(b.card() == 3);
}

TEST_CASE("ArrayBuilder<int>", "[ArrayBuilder]")
{
    ArrayBuilder a { 1, 2, 3 };
    REQUIRE(a.card() == 3);

    for ( int i = 0; i < 3; ++i )
        CHECK(a[i] == i+1);

    auto b = std::move(a);
    CHECK(!a);
    CHECK(b.card() == 3);

    b.append(4);
    CHECK(b.back() == 4);

    b.pop();
    CHECK(b.back() == 3);

    b.insert(end(b), 5);
    CHECK(b.back() == 5);

    b.insert(begin(b), 42);
    CHECK(b.front() == 42);

    b.insert(succ(begin(b), 2), 23);
    CHECK(b[2] == 23);

    {
        auto e = b.remove(succ(begin(b), 2));
        CHECK(*e == 2);
        CHECK(equal(b(), List<int>{ 42, 1, 2, 3, 5 }));
    }

    b.zap(42);
    CHECK(equal(b(), List<int>{ 1, 2, 3, 5 }));

    b.insert(succ(begin(b)), 5);
    b.insert(succ(begin(b), 4), 5);
    b.zapAll(5);
    CHECK(equal(b(), List<int>{ 1, 2, 3 }));

    b.clear();
    CHECK(!b);
    CHECK(b.card() == 0);
}

TEST_CASE("ArrayBuilder<Box<int>>", "[ArrayBuilder]")
{
    ArrayBuilder<Box<int>> a;
    REQUIRE(!a);

    a.appendRange(Map([](auto i) { return mk<int>(i); }, Iota{10}));

    REQUIRE(a);
    REQUIRE(a.card() == 10);

    for ( int i = 0; i < 10; ++i )
        CHECK(i == *a[i]);

    a.insert(begin(a) + 5, mk<int>(42));
    CHECK(a.card() == 11);
    CHECK(*a[4] == 4);
    CHECK(*a[5] == 42);
    CHECK(*a[6] == 5);

    ArrayBuilder<Box<int>> b;
    b.reserve(3);

    CHECK(!b);
    CHECK(b.card() == 0);

    b.append(mk<int>(88));
    b.append(mk<int>(89));
    b.append(mk<int>(90));

    CHECK(b.card() == 3);

    a.insertRange(succ(begin(a), 2), MoveRange(b()));

    CHECK(b.card() == 3);
    CHECK(a.card() == 14);

    CHECK(equal(a(),
                List<int>({0, 1, 88, 89, 90, 2, 3, 4, 42, 5, 6, 7, 8, 9 }),
                $2(*a == b)));

    Slice<int*> s = a;
    CHECK(equal(s, a(), $2(*a == *b)));
}

} // namespace kyfoo::unittests
