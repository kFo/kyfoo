#include <kyfoo/parser/Productions.hpp>

namespace kyfoo {
    namespace parser {

//
// ValueExpression

struct ValueExpression::impl : public
    g::OneOrMore<g::Or<Tuple, Primary>>
{
    std::unique_ptr<ast::ValueExpression> make() const
    {
        std::unique_ptr<ast::ValueExpression> subject;
        auto const& s = captures().front();
        if ( captures().front().index() == 0 )
            subject = s.term<0>().make();
        else
            subject = s.term<1>().make();

        std::vector<std::unique_ptr<ast::ValueExpression>> args;
        for ( std::size_t i = 1; i < captures().size(); ++i ) {
            auto const& a = captures()[i];
            if ( a.index() == 0 )
                args.emplace_back(a.term<0>().make());
            else
                args.emplace_back(a.term<1>().make());
        }

        return std::make_unique<ast::ApplyExpression>(std::move(subject),
                                                      std::make_unique<ast::TupleExpression>(std::move(args)));
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
    ast::Expression make() const
    {
        switch (index()) {
        case 0:
        {
            return ast::Expression(term<0>().make());
        }
        break;

        case 1:
        {
            return ast::Expression(term<1>().make());
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
      , g::And<
               id
             , g::Or<
                     g::And<openAngle, g::Repeat2<TypeSubExpression, comma>, closeAngle>
                   , g::Repeat<TypeSubExpression>
                    >
              >
         >
{
    std::unique_ptr<ast::TypeExpression> make() const
    {
        switch (index()) {
        case 0:
        {
            return std::make_unique<ast::TypeExpressionTuple>(ast::TupleKind::Open);
        }

        case 1:
        {
            auto const& t = term<1>();
            auto tupleKind = ast::toTupleKind(t.factor<0>().token().kind(), t.factor<2>().token().kind());
            std::vector<std::unique_ptr<ast::TypeExpression>> typeExpressions;
            for ( auto&& e : t.factor<1>().captures() )
                typeExpressions.emplace_back(e.make());

            return std::make_unique<ast::TypeExpressionTuple>(tupleKind, std::move(typeExpressions));
        }

        case 2:
        {
            auto const& t = term<2>();
            auto const& id = t.factor<0>().token();
            auto const& list = t.factor<1>();
            std::vector<ast::Expression> typeArguments;
            if ( list.index() == 0) {
                for ( auto&& e : list.term<0>().factor<1>().captures() )
                    typeArguments.emplace_back(e.make());
            }
            else
            {
                for ( auto&& e : list.term<1>().captures() )
                    typeArguments.emplace_back(e.make());
            }

            return std::make_unique<ast::PrimaryTypeExpression>(id, std::move(typeArguments));
        }

        default:
            throw std::runtime_error("invalid TypeExpression");
        }
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
