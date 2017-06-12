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

        std::unique_ptr<ast::Expression> subject;
        if ( primary.captures().size() == 1 ) {
            subject = primary.captures().front().monoMake<ast::Expression>();
        }
        else {
            std::vector<std::unique_ptr<ast::Expression>> exprs;
            for ( std::size_t i = 0; i < primary.captures().size(); ++i ) {
                auto const& a = primary.captures()[i];
                exprs.emplace_back(a.monoMake<ast::Expression>());
            }

            subject = std::make_unique<ast::ApplyExpression>(std::move(exprs));
        }

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
