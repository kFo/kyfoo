#include <kyfoo/parser/Productions.hpp>

namespace kyfoo {
    namespace parser {

//
// Expression

struct Expression::impl : public
    g::And<g::OneOrMore<DotExpression>, g::Opt<g::And<colon, Expression>>>
{
    std::unique_ptr<ast::Expression> make() const
    {
        std::unique_ptr<ast::Expression> expr;
        if ( factor<0>().captures().size() == 1 ) {
            expr = factor<0>().captures()[0].make();
        }
        else {
            std::vector<std::unique_ptr<ast::Expression>> exprs;
            for ( auto const& e : factor<0>().captures() )
                exprs.emplace_back(e.make());

            expr = std::make_unique<ast::ApplyExpression>(std::move(exprs));
        }

        if ( auto c = factor<1>().capture() )
            expr->addConstraint(c->factor<1>().make());

        return expr;
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
