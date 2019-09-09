#pragma once

#include <tuple>
#include <utility>
#include <variant>

#include <kyfoo/Types.hpp>
#include <kyfoo/lexer/Scanner.hpp>
#include <kyfoo/lexer/TokenKind.hpp>

namespace kyfoo {
    namespace parser {
        class DeclarationScopeParser;
    }
    namespace ast {
        class LiteralExpression;
    }
}

// Keep out of kyfoo::parser namespace to reduce decorated name length
namespace g {

using namespace kyfoo;

template <lexer::TokenKind K>
class Terminal
{
public:
    Terminal() = default;

public:
    bool match(lexer::ScanPoint scan, uz& matches)
    {
        if ( scan.peek().kind() != K )
            return false;

        myCapture = &scan.next();
        matches += 1;
        return scan.commit();
    }

    lexer::Token const& token() const
    {
        return *myCapture;
    }

    lexer::Token const& make(parser::DeclarationScopeParser&) const
    {
        return token();
    }

private:
    lexer::Token const* myCapture;
};

template <typename... T>
class And
{
public:
    friend class And;

public:
    And() = default;

    template <typename... U, typename... V>
    And(And<U...> const& lhs, And<V...> const& rhs)
        : myFactors(std::tuple_cat(lhs.myFactors, rhs.myFactors))
    {
    }

    template <typename... U, typename V>
    And(And<U...> const& lhs, V const& rhs)
        : myFactors(std::tuple_cat(lhs.myFactors, std::forward_as_tuple(rhs)))
    {
    }

    template <typename U, typename... V>
    And(U const& lhs, And<V...> const& rhs)
        : myFactors(std::tuple_cat(std::forward_as_tuple(lhs), rhs.myFactors))
    {
    }

    explicit And(T const&... args)
        : myFactors(args...)
    {
    }

public:
    bool match(lexer::ScanPoint scan, uz& matches)
    {
        if ( subMatch<0>(scan, matches) ) {
            return scan.commit();
        }

        return false;
    }

    template <uz N>
    auto const& factor() const
    {
        return std::get<N>(myFactors);
    }

    template <uz N>
    auto& factor()
    {
        return std::get<N>(myFactors);
    }

private:
    template <uz N>
    bool subMatch(lexer::ScanPoint& scan,
                  uz& matches)
    {
        if constexpr( N == sizeof...(T) ) {
            (void)scan;
            (void)matches;
            return true;
        }
        else {
            uz m0 = 0;
            if ( std::get<N>(myFactors).match(scan, m0) ) {
                uz m1 = 0;
                if ( subMatch<N + 1>(scan, m1) ) {
                    matches += m0 + m1;
                    return true;
                }
            }

            return false;
        }
    }

    template <>
    bool subMatch<sizeof...(T)>(lexer::ScanPoint&, uz&)
    {
        return true;
    }

private:
    std::tuple<T...> myFactors;
};

template <typename T, template <typename...> class G, typename... Branches>
struct MonomorphicMaker
{
    static T make(parser::DeclarationScopeParser& parser, G<Branches...>& rhs)
    {
        return makeImpl<0>(parser, rhs);
    }

    template <uz N>
    static T makeImpl(parser::DeclarationScopeParser& parser, G<Branches...>& rhs)
    {
        if constexpr (N == sizeof...(Branches)) {
            (void)parser;
            (void)rhs;
            ENFORCEU("invalid or make");
        }
        else {
            if ( rhs.index() == N )
                return rhs.template term<N>().make(parser);

            return makeImpl<N+1>(parser, rhs);
        }
    }
};

template <typename... T>
class Or
{
public:
    friend class Or;

public:
    explicit Or() = default;

    template <typename... U, typename... V>
    Or(Or<U...> const& lhs, Or<V...> const& rhs)
        : myTerms(std::tuple_cat(lhs.myTerms, rhs.myTerms))
    {
    }

    template <typename... U, typename V>
    Or(Or<U...> const& lhs, V const& rhs)
        : myTerms(std::tuple_cat(lhs.myTerms, std::forward_as_tuple(rhs)))
    {
    }

    template <typename U, typename... V>
    Or(U const& lhs, Or<V...> const& rhs)
        : myTerms(std::tuple_cat(std::forward_as_tuple(lhs), rhs.myTerms))
    {
    }

    explicit Or(T const&... args)
        : myTerms(args...)
    {
    }

public:
    bool match(lexer::ScanPoint scan, uz& matches)
    {
        uz m = 0;
        myTerms.template emplace<0>();
        if ( std::get<0>(myTerms).match(scan, m) ) {
            matches += m;
            return scan.commit();
        }

        m = 0;
        if ( subMatch<1>(scan, m) ) {
            matches += m;
            return scan.commit();
        }

        return false;
    }

    uz index() const
    {
        return myTerms.index();
    }

    template <uz N>
    auto const& term() const
    {
        return std::get<N>(myTerms);
    }

    template <uz N>
    auto& term()
    {
        return std::get<N>(myTerms);
    }

    template <typename AST>
    AST monoMake(parser::DeclarationScopeParser& parser)
    {
        return MonomorphicMaker<AST, g::Or, T...>::make(parser, *this);
    }

    template <typename AST>
    Box<AST> monoMakePtr(parser::DeclarationScopeParser& parser)
    {
        return MonomorphicMaker<Box<AST>, g::Or, T...>::make(parser, *this);
    }

private:
    template <int N>
    bool subMatch(lexer::ScanPoint& scan, uz& matches)
    {
        if constexpr( N == sizeof...(T) ) {
            return false;
        }
        else {
            scan.restart();

            uz m = 0;
            myTerms.template emplace<N>();
            if ( std::get<N>(myTerms).match(scan, m) ) {
                matches += m;
                return true;
            }

            return subMatch<N + 1>(scan, matches);
        }
    }

    template <>
    bool subMatch<sizeof...(T)>(lexer::ScanPoint&, uz&)
    {
        return false;
    }

private:
    std::variant<T...> myTerms;
};

template <typename... T>
class Long
{
public:
    Long() = default;

    template <typename... U, typename... V>
    Long(Long<U...> const& lhs, Long<V...> const& rhs)
        : myTerms(std::tuple_cat(lhs.myTerms, rhs.myTerms))
    {
    }

    template <typename... U, typename V>
    Long(Long<U...> const& lhs, V const& rhs)
        : myTerms(std::tuple_cat(lhs.myTerms, std::forward_as_tuple(rhs)))
    {
    }

    template <typename U, typename... V>
    Long(U const& lhs, Long<V...> const& rhs)
        : myTerms(std::tuple_cat(std::forward_as_tuple(lhs), rhs.myTerms))
    {
    }

    explicit Long(T const&... args)
        : myTerms(std::forward<T>(args)...)
    {
    }

public:
    bool match(lexer::ScanPoint scan, uz& matches)
    {
        uz longest = 0;
        uz length = 0;

        uz m = 0;
        if ( std::get<0>(myTerms).match(scan, m) ) {
            length = m;
        }

        subMatch<1>(scan, longest, length);

        if ( !length )
            return false;

        // Rescan to commit the longest term
        scan.restart();

        m = 0;
        match_nth(longest, scan, m);
        matches += m;
        myCapture = longest;
        return scan.commit();
    }

    uz index() const
    {
        return myCapture;
    }

    template <uz N>
    auto const& term() const
    {
        return std::get<N>(myTerms);
    }

    template <uz N>
    auto& term()
    {
        return std::get<N>(myTerms);
    }

private:
    template <int N>
    void subMatch(lexer::ScanPoint& scan,
                  uz& longest,
                  uz& length)
    {
        scan.restart();

        uz m = 0;
        if ( std::get<N>(myTerms).match(scan, m) && m > length ) {
            longest = N;
            length = m;
        }

        subMatch<N + 1>(scan, longest, length);
    }

    template <>
    void subMatch<sizeof...(T)>(lexer::ScanPoint&,
                                uz&,
                                uz&)
    {
        // nop
    }

    bool match_nth(uz index,
                   lexer::ScanPoint& scan,
                   uz& matches)
    {
        return match_nth_impl<0>(index, scan, matches);
    }

    template <uz N>
    bool match_nth_impl(uz index,
                        lexer::ScanPoint& scan,
                        uz& matches)
    {
        if ( N == index )
            return std::get<N>(myTerms).match(scan, matches);

        return match_nth_impl<N + 1>(index, scan, matches);
    }

    template <>
    bool match_nth_impl<sizeof...(T)>(uz,
                                      lexer::ScanPoint&,
                                      uz&)
    {
        return false;
    }

private:
    std::tuple<T...> myTerms;
    uz myCapture = 0;
};

template <typename T>
class Opt
{
public:
    Opt() = default;

public:
    bool match(lexer::ScanPoint scan, uz& matches)
    {
        if ( myRhs.match(scan, matches) ) {
            myCapture = true;
            return scan.commit();
        }

        myCapture = false;

        return true;
    }

    T* capture()
    {
        if ( myCapture )
            return &myRhs;

        return nullptr;
    }

    T const* capture() const
    {
        return const_cast<Opt*>(this)->capture();
    }

private:
    T myRhs;
    bool myCapture = false;
};

template <typename T>
class Repeat
{
public:
    Repeat() = default;

public:
    bool match(lexer::ScanPoint scan, uz& matches)
    {
        myCaptures.clear();

        uz m = 0;
        T pattern;
        while ( pattern.match(scan, m) ) {
            matches += m;
            m = 0;
            myCaptures.append(std::move(pattern));
        }

        return scan.commit();
    }

    ab<T> const& captures() const
    {
        return myCaptures;
    }

    ab<T>& captures()
    {
        return myCaptures;
    }

private:
    ab<T> myCaptures;
};

template <typename U, typename V>
class Repeat2
{
public:
    Repeat2() = default;

public:
    bool match(lexer::ScanPoint scan, uz& matches)
    {
        myCaptures.clear();

        uz m0 = 0;
        U pattern;
        if ( !pattern.match(scan, m0) )
            return scan.commit();

        myCaptures.append(std::move(pattern));
        matches += m0;
        m0 = 0;

        for (;;) {
            m0 = 0;
            uz m1 = 0;
            V weave;
            if ( !weave.match(scan, m0) || !pattern.match(scan, m1) ) 
                break;

            myCaptures.append(std::move(pattern));
            matches += m0 + m1;
        }

        return scan.commit();
    }

    ab<U> const& captures() const
    {
        return myCaptures;
    }

    ab<U>& captures()
    {
        return myCaptures;
    }

private:
    ab<U> myCaptures;
};

template <typename T>
class OneOrMore
{
public:
    using CaptureVector = ab<T>;

public:
    OneOrMore() = default;

public:
    bool match(lexer::ScanPoint scan, uz& matches)
    {
        myCaptures.clear();

        uz m = 0;
        T pattern;
        if ( !pattern.match(scan, m) )
            return false;

        do {
            matches += m;
            m = 0;
            myCaptures.append(std::move(pattern));
        } while ( pattern.match(scan, m) );

        return scan.commit();
    }

    CaptureVector const& captures() const
    {
        return myCaptures;
    }

    CaptureVector& captures()
    {
        return myCaptures;
    }

private:
    CaptureVector myCaptures;
};

template <typename U, typename V>
class OneOrMore2
{
public:
    using CaptureVector = ab<U>;

public:
    OneOrMore2() = default;

public:
    bool match(lexer::ScanPoint scan, uz& matches)
    {
        myCaptures.clear();

        uz m0 = 0;
        U pattern;
        if ( !pattern.match(scan, m0) )
            return false;

        matches += m0;
        myCaptures.append(std::move(pattern));

        for (;;) {
            m0 = 0;
            uz m1 = 0;
            V weave;
            if ( !weave.match(scan, m0) || !pattern.match(scan, m1) )
                break;

            matches += m0 + m1;
            myCaptures.append(std::move(pattern));
        }

        return scan.commit();
    }

    CaptureVector const& captures() const
    {
        return myCaptures;
    }

    CaptureVector& captures()
    {
        return myCaptures;
    }

private:
    CaptureVector myCaptures;
};

template <lexer::TokenKind Open, lexer::TokenKind Close>
class Nest
{
public:
    explicit Nest() = default;

public:
    bool match(lexer::ScanPoint scan, uz& matches)
    {
        if ( scan.peek().kind() != Open )
            return false;

        myCaptures.emplace_back(&scan.next());
        ++matches;
        for ( int nest = 1; nest; ) {
            myCaptures.emplace_back(&scan.next());
            ++matches;
            switch (myCaptures.back()->kind()) {
            case Open:
                ++nest;
                break;

            case Close:
                --nest;
                break;

            default:
                break;
            }
        }

        return scan.commit();
    }

    std::deque<lexer::Token const*> const& captures() const
    {
        return myCaptures;
    }

    std::deque<lexer::Token const*>& captures()
    {
        return myCaptures;
    }

private:
    std::deque<lexer::Token const*> myCaptures;
};

template <typename... U, typename... V>
constexpr And<U..., V...> operator & (And<U...> const& lhs, And<V...> const& rhs)
{
    return And<U..., V...>(lhs, rhs);
}

template <typename... U, typename V>
constexpr And<U..., V> operator & (And<U...> const& lhs, V const& rhs)
{
    return And<U..., V>(lhs, rhs);
}

template <typename U, typename... V>
constexpr And<U, V...> operator & (U const& lhs, And<V...> const& rhs)
{
    return And<U, V...>(lhs, rhs);
}

template <typename U, typename V>
constexpr And<U, V> operator & (U const& lhs, V const& rhs)
{
    return And<U, V>(lhs, rhs);
}

// Ordered

template <typename... U, typename... V>
constexpr Or<U..., V...> operator | (Or<U...> const& lhs, Or<V...> const& rhs)
{
    return Or<U..., V...>(lhs, rhs);
}

template <typename... U, typename V>
constexpr Or<U..., V> operator | (Or<U...> const& lhs, V const& rhs)
{
    return Or<U..., V>(lhs, rhs);
}

template <typename U, typename... V>
constexpr Or<U, V...> operator | (U const& lhs, Or<V...> const& rhs)
{
    return Or<U, V...>(lhs, rhs);
}

template <typename U, typename V>
constexpr Or<U, V> operator | (U const& lhs, V const& rhs)
{
    return Or<U, V>(lhs, rhs);
}

// Longest

template <typename... U, typename... V>
constexpr Long<U..., V...> operator || (Long<U...> const& lhs, Long<V...> const& rhs)
{
    return Long<U..., V...>(lhs, rhs);
}

template <typename... U, typename V>
constexpr Long<U..., V> operator || (Long<U...> const& lhs, V const& rhs)
{
    return Long<U..., V>(lhs, rhs);
}

template <typename U, typename... V>
constexpr Long<U, V...> operator || (U const& lhs, Long<V...> const& rhs)
{
    return Long<U, V...>(lhs, rhs);
}

template <typename U, typename V>
constexpr Long<U, V> operator || (U const & lhs, V const& rhs)
{
    return Long<U, V>(lhs, rhs);
}

// Optional

template <typename T>
constexpr Opt<T> opt(T const& rhs)
{
    return Opt<T>(rhs);
}

// Repeat

template <typename T>
constexpr Repeat<T> operator * (T const& rhs)
{
    return Repeat<T>(rhs);
}

template <typename U, typename V>
constexpr Repeat2<U, V> operator * (U const& lhs, V const& rhs)
{
    return Repeat2<U, V>(lhs, rhs);
}

// OneOrMore (optional Repeat)

template <typename T>
constexpr OneOrMore<T> operator + (T const& rhs)
{
    return OneOrMore<T>(rhs);
}

template <typename U, typename V>
constexpr OneOrMore2<U, V> operator + (U const& lhs, V const& rhs)
{
    return OneOrMore2<U, V>(lhs, rhs);
}

} // namespace g
