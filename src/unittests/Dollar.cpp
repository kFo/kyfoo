#include <catch2/catch.hpp>

#include <kyfoo/Dollar.hpp>

namespace kyfoo::unittests {

TEST_CASE("dollar", "[Dollar][Placeholder]") {
    for ( int x = -10; x <= 10; ++x ) {
        CHECK( (+$)(x) == +x );
        CHECK( (-$)(x) == -x );
        CHECK( (~$)(x) == ~x );
        CHECK( (!$)(x) == !x );

        CHECK( ($+1 )(x) == (x+1 ) );
        CHECK( ($-1 )(x) == (x-1 ) );
        CHECK( ($*2 )(x) == (x*2 ) );
        CHECK( ($/2 )(x) == (x/2 ) );
        CHECK( ($%5 )(x) == (x%5 ) );
        CHECK( ($^1 )(x) == (x^1 ) );
        CHECK( ($&1 )(x) == (x&1 ) );
        CHECK( ($|1 )(x) == (x|1 ) );
        CHECK( ($<<1)(x) == (x<<1) );
        CHECK( ($>>1)(x) == (x>>1) );
        CHECK( ($==5)(x) == (x==5) );
        CHECK( ($!=5)(x) == (x!=5) );
        CHECK( ($<5 )(x) == (x<5 ) );
        CHECK( ($<=5)(x) == (x<=5) );
        CHECK( ($>5 )(x) == (x>5 ) );
        CHECK( ($>=5)(x) == (x>=5) );
    }

    {
        int x = 42;
        CHECK( ($++)(x) == 42 );
        CHECK( x == 43 );
        CHECK( (++$)(x) == 44 );
        CHECK( x == 44 );
        CHECK( (--$)(x) == 43 );
        CHECK( ($--)(x) == 43 );
        CHECK( x == 42 );

        CHECK( ($$ = 23)(x) == 23 );
        CHECK(            x == 23 );
        CHECK( ($$ += 7)(x) == 30 );
        CHECK(            x == 30 );
        CHECK( ($$ -= 2)(x) == 28 );
        CHECK(            x == 28 );
        CHECK( ($$ *= 3)(x) == 84 );
        CHECK(            x == 84 );
        CHECK( ($$ /= 2)(x) == 42 );
        CHECK(            x == 42 );
        CHECK( ($$ %=12)(x) == 6  );
        CHECK(            x == 6  );
        CHECK( ($$ ^= 2)(x) == 4  );
        CHECK(            x == 4  );
        CHECK( ($$ &= 1)(x) == 0  );
        CHECK(            x == 0  );
        CHECK( ($$ |= 6)(x) == 6  );
        CHECK(            x == 6  );
        CHECK( ($$ <<=2)(x) == 24 );
        CHECK(            x == 24 );
        CHECK( ($$ >>=2)(x) == 6  );
        CHECK(            x == 6  );
    }

    int arr[] = { 1, 2, 3 };
    CHECK( ($[1])(arr) == 2 );
    CHECK( ($$[2] = 42)(arr) == 42 );
    CHECK(            arr[2] == 42 );
    CHECK( (*$)(arr) == 1 );

    for ( int x = -1; x <= 1; ++x ) {
        for ( int y = -1; y <= 1; ++y ) {
            CHECK( ($a == $b)(x, y) == (x==y) );
            CHECK( ($a != $b)(x, y) == (x!=y) );
            CHECK( ($a <  $b)(x, y) == (x<y ) );
            CHECK( ($a <= $b)(x, y) == (x<=y) );
            CHECK( ($a >  $b)(x, y) == (x>y ) );
            CHECK( ($a >= $b)(x, y) == (x>=y) );
        }
    }
}

} // namespace kyfoo::unittests
