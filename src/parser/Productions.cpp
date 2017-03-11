#include <kyfoo/parser/Productions.hpp>

namespace kyfoo {
    namespace parser {

//
// Compound

struct Compound::impl : public
    g::Or<Tuple, Apply, Primary>
{
    std::unique_ptr<ast::Expression> make() const
    {
        switch (index()) {
        case 0: return term<0>().make();
        case 1: return term<1>().make();
        case 2: return term<2>().make();
        }

        throw std::runtime_error("invalid expression capture");
    }
};

Compound::Compound()
{
}

Compound::Compound(Compound const& rhs)
    : myGrammar(rhs.myGrammar ? std::make_unique<impl>(*rhs.myGrammar) : nullptr)
{
}

Compound::~Compound() = default;

bool Compound::match(kyfoo::lexer::ScanPoint scan, std::size_t& matches)
{
    if ( !myGrammar )
        myGrammar = std::make_unique<impl>();

    if ( myGrammar->match(scan, matches) )
        return scan.commit();

    return false;
}

std::unique_ptr<ast::Expression> Compound::make() const
{
    return myGrammar->make();
}

    } // namespace parser
} // namespace kyfoo
