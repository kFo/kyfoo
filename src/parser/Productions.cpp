#include <kyfoo/parser/Productions.hpp>

namespace kyfoo {
    namespace parser {

//
// ValueExpression

struct ValueExpression::impl : public
    g::Or<Tuple, Apply, Primary>
{
    std::unique_ptr<ast::ValueExpression> make() const
    {
        switch (index()) {
        case 0: return term<0>().make();
        case 1: return term<1>().make();
        case 2: return term<2>().make();
        }

        throw std::runtime_error("invalid expression capture");
    }
};

ValueExpression::ValueExpression()
{
}

ValueExpression::ValueExpression(ValueExpression const& rhs)
    : myGrammar(rhs.myGrammar ? std::make_unique<impl>(*rhs.myGrammar) : nullptr)
{
}

ValueExpression::~ValueExpression() = default;

bool ValueExpression::match(kyfoo::lexer::ScanPoint scan, std::size_t& matches)
{
    if ( !myGrammar )
        myGrammar = std::make_unique<impl>();

    if ( myGrammar->match(scan, matches) )
        return scan.commit();

    return false;
}

std::unique_ptr<ast::ValueExpression> ValueExpression::make() const
{
    return myGrammar->make();
}

//
// TypeExpression

struct TypeSubExpression : public
    g::Or<
          TypeExpression
        , ValueExpression
         >
{
    ast::TypeArgument make() const
    {
        switch (index()) {
        case 0:
        {
            return ast::TypeArgument(term<0>().make());
        }
        break;

        case 1:
        {
            return ast::TypeArgument(term<1>().make());
        }
        break;
        }

        throw std::runtime_error("invalid TypeSubExpression");
    }
};

struct TypeExpression::impl : public
    g::Or<
        g::And<openParen, closeParen>
      , g::And<TupleOpen, g::Repeat2<TypeExpression, comma>, TupleClose>
      , g::And<id, g::Opt<g::And<openAngle, g::Repeat2<TypeSubExpression, comma>, closeAngle>>>
         >
{
    std::unique_ptr<ast::TypeExpression> make() const
    {
        switch (index()) {
        case 0:
        {
            return std::make_unique<ast::TypeExpressionTuple>(ast::TupleKind::Open);
        }
        break;

        case 1:
        {
            auto const& t = term<1>();
            auto tupleKind = ast::toTupleKind(t.factor<0>().token().kind(), t.factor<2>().token().kind());
            std::vector<std::unique_ptr<ast::TypeExpression>> typeExpressions;
            for ( auto&& e : t.factor<1>().captures() )
                typeExpressions.emplace_back(e.make());

            return std::make_unique<ast::TypeExpressionTuple>(tupleKind, std::move(typeExpressions));
        }
        break;

        case 2:
        {
            auto const& t = term<2>();
            auto const& id = t.factor<0>().token();
            std::vector<ast::TypeParameter> typeParameters;
            if ( t.factor<1>().capture() )
                for ( auto&& e : t.factor<1>().capture()->factor<1>().captures() )
                    typeParameters.emplace_back(e.make());

            return std::make_unique<ast::PrimaryTypeExpression>(id, std::move(typeParameters));
        }
        break;
        }

        throw std::runtime_error("invalid TypeExpression");
    }

};

TypeExpression::TypeExpression() = default;

TypeExpression::TypeExpression(TypeExpression const& rhs)
    : myGrammar(rhs.myGrammar ? std::make_unique<impl>(*rhs.myGrammar) : nullptr)
{
}

TypeExpression::~TypeExpression() = default;

bool TypeExpression::match(kyfoo::lexer::ScanPoint scan, std::size_t& matches)
{
    if ( !myGrammar )
        myGrammar = std::make_unique<impl>();

    if ( myGrammar->match(scan, matches) )
        return scan.commit();

    return false;
}

std::unique_ptr<ast::TypeExpression> TypeExpression::make() const
{
    return myGrammar->make();
}

    } // namespace parser
} // namespace kyfoo
