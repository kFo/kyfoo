#include <kyfoo/parser/Productions.hpp>

namespace kyfoo {
    namespace parser {

//
// Expression

struct Expression::impl : public
    g::And<g::OneOrMore<g::Or<Tuple, Primary>>, g::Opt<g::And<colon, Expression>>>
{
    std::unique_ptr<ast::Expression> make() const
    {
        auto const& primary = factor<0>();
        std::unique_ptr<ast::Expression> first;
        auto const& s = primary.captures().front();
        if ( s.index() == 0 )
            first = s.term<0>().make();
        else
            first = s.term<1>().make();

        if ( primary.captures().size() == 1 )
            return first;

        std::vector<std::unique_ptr<ast::Expression>> exprs;
        exprs.emplace_back(std::move(first));
        for ( std::size_t i = 1; i < primary.captures().size(); ++i ) {
            auto const& a = primary.captures()[i];
            if ( a.index() == 0 )
                exprs.emplace_back(a.term<0>().make());
            else
                exprs.emplace_back(a.term<1>().make());
        }

        auto subject = createTuple(ast::TupleKind::Apply, std::move(exprs));

        if ( auto c = factor<1>().capture() )
            return std::make_unique<ast::ConstraintExpression>(std::move(subject), c->factor<1>().make());

        return subject;
    }
};

Expression::Expression() = default;

Expression::Expression(Expression const& rhs)
    : myGrammar(rhs.myGrammar ? std::make_unique<impl>(*rhs.myGrammar) : nullptr)
{
}

Expression::~Expression() = default;

bool Expression::match(kyfoo::lexer::ScanPoint scan, std::size_t& matches)
{
    if ( !myGrammar )
        myGrammar = std::make_unique<impl>();

    if ( myGrammar->match(scan, matches) )
        return scan.commit();

    return false;
}

std::unique_ptr<ast::Expression> Expression::make() const
{
    return myGrammar->make();
}

    } // namespace parser
} // namespace kyfoo
