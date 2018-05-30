#pragma once

#include <kyfoo/lexer/Scanner.hpp>
#include <kyfoo/lexer/Token.hpp>
#include <kyfoo/lexer/TokenKind.hpp>

namespace gs {

template <kyfoo::lexer::TokenKind K>
struct Terminal
{
    static bool match(kyfoo::lexer::ScanPoint scan, uz& matches)
    {
        if ( scan.next().kind() != K)
            return false;

        matches += 1;
        return scan.commit();
    }

    static kyfoo::lexer::Token capture(kyfoo::lexer::Scanner& scanner)
    {
        return scanner.next();
    }
};

template <typename... T>
struct And
{
    static bool match(kyfoo::lexer::ScanPoint scan, uz& matches)
    {
        uz m = 0;
        if ( impl<T...>::match(scan, matches) ) {
            matches += m;
            return scan.commit();
        }

        return false;
    }

private:
    template <typename Head, typename... Tail>
    struct impl {
        static bool match(kyfoo::lexer::ScanPoint& scan, uz& matches)
        {
            uz m = 0;
            if ( Head::match(scan, m) && And<Tail...>::match(scan, matches) ) {
                matches += m;
                return true;
            }

            return false;
        }
    };

    template <typename Head>
    struct impl<Head>
    {
        static bool match(kyfoo::lexer::ScanPoint& scan, uz& matches)
        {
            return Head::match(scan, matches);
        }
    };
};

template <typename... T>
struct Or
{
    static bool match(kyfoo::lexer::ScanPoint scan, uz& matches)
    {
        uz m = 0;
        if ( impl<T...>::match(scan, matches) ) {
            matches += m;
            return scan.commit();
        }

        return impl<T...>::match(scan, matches);
    }

private:
    template <typename Head, typename... Tail>
    struct impl {
        static bool match(kyfoo::lexer::ScanPoint& scan, uz& matches)
        {
            uz m = 0;
            if ( Head::match(scan, matches) ) {
                matches += m;
                return true;
            }

            scan.restart();
            return impl<Tail...>::match(scan, matches);
        }
    };

    template <typename Head>
    struct impl<Head> {
        static bool match(kyfoo::lexer::ScanPoint scan, uz& matches)
        {
            return Head::match(scan, matches);
        }
    };
};

template <typename H, typename... T>
struct Long
{
    static bool match(kyfoo::lexer::ScanPoint scan,
                      uz& matches)
    {
        uz longest = 0;
        uz length = 0;

        impl<0, H, T...>::match(scan, longest, length);

        if ( !length )
            return false;

        impl<0, H, T...>::rematch(scan, matches, longest);
        return scan.commit();
    }

private:
    template <uz N, typename Head, typename... Tail>
    struct impl {
        static void match(kyfoo::lexer::ScanPoint& scan,
                          uz& longest,
                          uz& length)
        {
            uz m = 0;
            if ( Head::match(scan, m) && m > length ) {
                longest = N;
                length = m;
            }

            impl<N + 1, Tail...>::match(scan, longest, length);
        }

        static void rematch(kyfoo::lexer::ScanPoint& scan,
                            uz& matches,
                            uz n)
        {
            if ( n == N )
                Head::match(scan, matches);
            else
                impl<N + 1, Tail...>::rematch(scan, matches, n);
        }
    };

    template <uz N, typename Head>
    struct impl<N, Head> {
        static void match(kyfoo::lexer::ScanPoint& scan,
                          uz& longest,
                          uz& length)
        {
            uz m = 0;
            if ( Head::match(scan, m) && m > length ) {
                longest = N;
                length = m;
            }
        }

        static void rematch(kyfoo::lexer::ScanPoint& scan,
                            uz& matches,
                            uz n)
        {
            if (n == N)
                Head::match(scan, matches);
        }
    };
};

template <typename T>
struct Opt
{
    static bool match(kyfoo::lexer::ScanPoint scan, uz& matches)
    {
        if ( T::match(scan, matches) )
            return scan.commit();

        return true;
    }
};

template <typename T>
struct Repeat
{
    static bool match(kyfoo::lexer::ScanPoint scan, uz& matches)
    {
        uz m = 0;
        while ( T::match(scan, m) ) {
            matches += m;
            m = 0;
        }

        return scan.commit();
    }
};

template <typename U, typename V>
struct Repeat2
{
    static bool match(kyfoo::lexer::ScanPoint scan, uz& matches)
    {
        uz m0 = 0;
        if ( U::match(scan, m0))
            return scan.commit();

        matches += m0;
        m0 = 0;

        for (;;) {
            m0 = 0;
            uz m1 = 0;
            if ( !V::match(scan, m0) || !U::match(scan, m1) )
                break;

            matches += m0 + m1;
        }

        return scan.commit();
    }
};

template <typename T>
struct OneOrMore
{
    static bool match(kyfoo::lexer::ScanPoint scan, uz& matches)
    {
        uz m = 0;
        if ( !T::match(scan, m) )
            return false;

        do {
            matches += m;
            m = 0;
        } while (T::match(scan, m));

        return scan.commit();
    }
};

template <typename U, typename V>
struct OneOrMore2
{
    static bool match(kyfoo::lexer::ScanPoint scan, uz& matches)
    {
        uz m0 = 0;
        if ( !U::match(scan, m) )
            return false;

        matches += m0;

        for (;;) {
            m0 = 0;
            uz m1 = 0;
            if ( !V::match(scan, m0) || !U::match(scan, m1) )
                break;

            matches += m0 + m1;
        }

        return scan.commit();
    }
};

} // namespace gs
