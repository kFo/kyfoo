#pragma once

#include <memory>
#include <tuple>
#include <utility>
#include <variant>

#include <kyfoo/lexer/Scanner.hpp>
#include <kyfoo/lexer/TokenKind.hpp>

namespace kyfoo {
    namespace ast {
        class LiteralExpression;
    }
}

// Keep out of kyfoo::parser namespace to reduce decorated name length
namespace g {

template <kyfoo::lexer::TokenKind K>
class Terminal
{
public:
    Terminal() = default;

public:
    bool match(kyfoo::lexer::ScanPoint scan, std::size_t& matches)
    {
        if ( scan.peek().kind() != K )
            return false;

        myCapture = scan.next();
        matches += 1;
        return scan.commit();
    }

    kyfoo::lexer::Token const& token() const
    {
        return myCapture;
    }

    kyfoo::lexer::Token const& make() const
    {
        return token();
    }

private:
    kyfoo::lexer::Token myCapture;
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
    bool match(kyfoo::lexer::ScanPoint scan, std::size_t& matches)
    {
        if ( subMatch<0>(scan, matches) ) {
            return scan.commit();
        }

        return false;
    }

    template <std::size_t N>
    auto const& factor() const
    {
        return std::get<N>(myFactors);
    }

private:
    template <std::size_t N>
    bool subMatch(kyfoo::lexer::ScanPoint& scan,
                  std::size_t& matches)
    {
        std::size_t m0 = 0;
        if ( std::get<N>(myFactors).match(scan, m0) ) {
            std::size_t m1 = 0;
            if ( subMatch<N + 1>(scan, m1) ) {
                matches += m0 + m1;
                return true;
            }
        }

        return false;
    }

    template<>
    bool subMatch<sizeof...(T)>(kyfoo::lexer::ScanPoint&, std::size_t&)
    {
        return true;
    }

private:
    std::tuple<T...> myFactors;
};

template <typename T, template <typename...> class G, typename... Branches>
struct MonomorphicMaker
{
    static T make(G<Branches...> const& rhs)
    {
        return make<0>(rhs);
    }

    template <std::size_t N>
    static T make(G<Branches...> const& rhs)
    {
        if ( rhs.index() == N )
            return rhs.term<N>().make();

        return make<N+1>(rhs);
    }

    template <>
    static T make<sizeof...(Branches)>(G<Branches...> const&)
    {
        throw std::runtime_error("invalid or make");
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
    bool match(kyfoo::lexer::ScanPoint scan, std::size_t& matches)
    {
        std::size_t m = 0;
        myTerms.emplace<0>();
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

    std::size_t index() const
    {
        return myTerms.index();
    }

    template <std::size_t N>
    auto const& term() const
    {
        return std::get<N>(myTerms);
    }

    template <typename AST>
    AST monoMake() const
    {
        return MonomorphicMaker<AST, g::Or, T...>::make<0>(*this);
    }

    template <typename AST>
    std::unique_ptr<AST> monoMakePtr() const
    {
        return MonomorphicMaker<std::unique_ptr<AST>, g::Or, T...>::make<0>(*this);
    }

private:
    template <int N>
    bool subMatch(kyfoo::lexer::ScanPoint& scan, std::size_t& matches)
    {
        scan.restart();

        std::size_t m = 0;
        myTerms.emplace<N>();
        if ( std::get<N>(myTerms).match(scan, m) ) {
            matches += m;
            return true;
        }

        return subMatch<N + 1>(scan, matches);
    }

    template <>
    bool subMatch<sizeof...(T)>(kyfoo::lexer::ScanPoint&, std::size_t&)
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
    bool match(kyfoo::lexer::ScanPoint scan, std::size_t& matches)
    {
        std::size_t longest = 0;
        std::size_t length = 0;

        std::size_t m = 0;
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

    std::size_t index() const
    {
        return myCapture;
    }

    template <std::size_t N>
    auto const& term() const
    {
        return std::get<N>(myTerms);
    }

private:
    template <int N>
    void subMatch(kyfoo::lexer::ScanPoint& scan,
                  std::size_t& longest,
                  std::size_t& length)
    {
        scan.restart();

        std::size_t m = 0;
        if ( std::get<N>(myTerms).match(scan, m) && m > length ) {
            longest = N;
            length = m;
        }

        subMatch<N + 1>(scan, longest, length);
    }

    template <>
    void subMatch<sizeof...(T)>(kyfoo::lexer::ScanPoint&,
                                std::size_t&,
                                std::size_t&)
    {
        // nop
    }

    bool match_nth(std::size_t index,
                   kyfoo::lexer::ScanPoint& scan,
                   std::size_t& matches)
    {
        return match_nth_impl<0>(index, scan, matches);
    }

    template <std::size_t N>
    bool match_nth_impl(std::size_t index,
                        kyfoo::lexer::ScanPoint& scan,
                        std::size_t& matches)
    {
        if ( N == index )
            return std::get<N>(myTerms).match(scan, matches);

        return match_nth_impl<N + 1>(index, scan, matches);
    }

    template <>
    bool match_nth_impl<sizeof...(T)>(std::size_t,
                                      kyfoo::lexer::ScanPoint&,
                                      std::size_t&)
    {
        return false;
    }

private:
    std::tuple<T...> myTerms;
    std::size_t myCapture = 0;
};

template <typename T>
class Opt
{
public:
    Opt() = default;

public:
    bool match(kyfoo::lexer::ScanPoint scan, std::size_t& matches)
    {
        if ( myRhs.match(scan, matches) ) {
            myCapture = true;
            return scan.commit();
        }

        myCapture = false;

        return true;
    }

    T const* capture() const
    {
        if ( myCapture )
            return &myRhs;

        return nullptr;
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
    bool match(kyfoo::lexer::ScanPoint scan, std::size_t& matches)
    {
        myCaptures.clear();

        std::size_t m = 0;
        T pattern
        while ( pattern.match(scan, m) ) {
            matches += m;
            m = 0;
            myCaptures.emplace_back(std::move(pattern));
        }

        return scan.commit();
    }

    std::vector<T> const& captures() const
    {
        return myCaptures;
    }

private:
    std::vector<T> myCaptures;
};

template <typename U, typename V>
class Repeat2
{
public:
    Repeat2() = default;

public:
    bool match(kyfoo::lexer::ScanPoint scan, std::size_t& matches)
    {
        myCaptures.clear();

        std::size_t m0 = 0;
        U pattern;
        if ( !pattern.match(scan, m0) )
            return scan.commit();

        myCaptures.emplace_back(std::move(pattern));
        matches += m0;
        m0 = 0;

        for (;;) {
            m0 = 0;
            std::size_t m1 = 0;
            V weave;
            if ( !weave.match(scan, m0) || !pattern.match(scan, m1) ) 
                break;

            myCaptures.emplace_back(std::move(pattern));
            matches += m0 + m1;
        }

        return scan.commit();
    }

    std::vector<U> const& captures() const
    {
        return myCaptures;
    }

private:
    std::vector<U> myCaptures;
};

template <typename T>
class OneOrMore
{
public:
    using CaptureVector = std::vector<T>;

public:
    OneOrMore() = default;

public:
    bool match(kyfoo::lexer::ScanPoint scan, std::size_t& matches)
    {
        myCaptures.clear();

        std::size_t m = 0;
        T pattern;
        if ( !pattern.match(scan, m) )
            return false;

        do {
            matches += m;
            m = 0;
            myCaptures.emplace_back(std::move(pattern));
        } while ( pattern.match(scan, m) );

        return scan.commit();
    }

    CaptureVector const& captures() const
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
    using CaptureVector = std::vector<U>;

public:
    OneOrMore2() = default;

public:
    bool match(kyfoo::lexer::ScanPoint scan, std::size_t& matches)
    {
        myCaptures.clear();

        std::size_t m0 = 0;
        U pattern;
        if ( !pattern.match(scan, m0) )
            return false;

        matches += m0;
        myCaptures.emplace_back(std::move(pattern));

        for (;;) {
            m0 = 0;
            std::size_t m1 = 0;
            V weave;
            if ( !weave.match(scan, m0) || !pattern.match(scan, m1) )
                break;

            matches += m0 + m1;
            myCaptures.emplace_back(std::move(pattern));
        }

        return scan.commit();
    }

    CaptureVector const& captures() const
    {
        return myCaptures;
    }

private:
    CaptureVector myCaptures;
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
