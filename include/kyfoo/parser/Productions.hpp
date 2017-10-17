#pragma once

#include <kyfoo/parser/Grammar.hpp>

#include <kyfoo/lexer/Token.hpp>

#include <kyfoo/ast/Declarations.hpp>
#include <kyfoo/ast/Expressions.hpp>
#include <kyfoo/ast/Symbol.hpp>

namespace kyfoo {
    namespace parser {

using lexer::TokenKind;
using lexer::Token;

using id       = g::Terminal<TokenKind::Identifier>;
using free     = g::Terminal<TokenKind::FreeVariable>;
using integer  = g::Terminal<TokenKind::Integer>;
using rational = g::Terminal<TokenKind::Rational>;
using string   = g::Terminal<TokenKind::String>;

using comma = g::Terminal<TokenKind::Comma>;
using dot   = g::Terminal<TokenKind::Dot>;
using equal = g::Terminal<TokenKind::Equal>;

using colon          = g::Terminal<TokenKind::Colon>;
using colonPipe      = g::Terminal<TokenKind::ColonPipe>;
using colonAmpersand = g::Terminal<TokenKind::ColonAmpersand>;
using colonEqual     = g::Terminal<TokenKind::ColonEqual>;

using yield = g::Terminal<TokenKind::Yield>;
using map   = g::Terminal<TokenKind::Map>;

using at         = g::Terminal<TokenKind::At>;
using minusMinus = g::Terminal<TokenKind::MinusMinus>;

using openParen    = g::Terminal<TokenKind::OpenParen>;
using closeParen   = g::Terminal<TokenKind::CloseParen>;
using openBracket  = g::Terminal<TokenKind::OpenBracket>;
using closeBracket = g::Terminal<TokenKind::CloseBracket>;
using openAngle    = g::Terminal<TokenKind::OpenAngle>;
using closeAngle   = g::Terminal<TokenKind::CloseAngle>;

using _import = g::Terminal<TokenKind::_import>;

struct Primary : public
    g::Or<id, free, integer, rational, string>
{
    std::unique_ptr<ast::PrimaryExpression> make() const
    {
        return monoMake<ast::PrimaryExpression>();
    }
};

struct Reference : public
    g::Or<g::And<equal, id>, Primary>
{
    std::unique_ptr<ast::Expression> make() const
    {
        if ( index() == 0 )
            return std::make_unique<ast::ReferenceExpression>(std::make_unique<ast::PrimaryExpression>(term<0>().factor<1>().token()));
        else
            return term<1>().make();
    }
};

class Expression
{
public:
    Expression();
    Expression(Expression const& rhs);
    Expression(Expression&&) = delete;
    ~Expression();

public:
    bool match(kyfoo::lexer::ScanPoint scan, std::size_t& matches);
    std::unique_ptr<ast::Expression> make() const;

private:
    struct impl;
    std::unique_ptr<impl> myGrammar;
};

inline std::vector<std::unique_ptr<ast::Expression>>
expressions(std::vector<Expression> const& rhs)
{
    std::vector<std::unique_ptr<ast::Expression>> ret;
    for ( auto const& e : rhs )
        ret.emplace_back(e.make());

    return ret;
}

inline std::unique_ptr<ast::TupleExpression>
createTuple(ast::TupleKind kind,
            std::vector<std::unique_ptr<ast::Expression>>&& expressions)
{
    return std::make_unique<ast::TupleExpression>(kind, std::move(expressions));
}

inline std::unique_ptr<ast::TupleExpression>
createTuple(lexer::Token const& open,
            lexer::Token const& close,
            std::vector<std::unique_ptr<ast::Expression>>&& expressions)
{
    return std::make_unique<ast::TupleExpression>(open, close, std::move(expressions));
}

struct TupleOpen : public
    g::And<openParen, g::Repeat2<Expression, comma>, closeParen>
{
    std::unique_ptr<ast::TupleExpression> make() const
    {
        return createTuple(factor<0>().token(),
                           factor<2>().token(),
                           expressions(factor<1>().captures()));
    }
};

struct TupleOpenRight : public
    g::And<openBracket, g::Repeat2<Expression, comma>, closeParen>
{
    std::unique_ptr<ast::TupleExpression> make() const
    {
        return createTuple(factor<0>().token(),
                           factor<2>().token(),
                           expressions(factor<1>().captures()));
    }
};

struct TupleOpenLeft : public
    g::And<openParen, g::Repeat2<Expression, comma>, closeBracket>
{
    std::unique_ptr<ast::TupleExpression> make() const
    {
        return createTuple(factor<0>().token(),
                           factor<2>().token(),
                           expressions(factor<1>().captures()));
    }
};

struct TupleClosed : public
    g::And<openBracket, g::Repeat2<Expression, comma>, closeBracket>
{
    std::unique_ptr<ast::TupleExpression> make() const
    {
        return createTuple(factor<0>().token(),
                           factor<2>().token(),
                           expressions(factor<1>().captures()));
    }
};

struct TupleSymbol : public
    g::And<g::Opt<id>, openAngle, g::Repeat2<Expression, comma>, closeAngle>
{
    std::unique_ptr<ast::SymbolExpression> make() const
    {
        auto e = expressions(factor<2>().captures());
        if ( auto i = factor<0>().capture() )
            return std::make_unique<ast::SymbolExpression>(i->token(), std::move(e));

        return std::make_unique<ast::SymbolExpression>(factor<1>().token(),
                                                       factor<3>().token(),
                                                       std::move(e));
    }
};

struct Tuple : public
    g::Or<TupleOpen, TupleOpenLeft, TupleOpenRight, TupleClosed, TupleSymbol>
{
    std::unique_ptr<ast::Expression> make() const
    {
        return monoMake<ast::Expression>();
    }
};

struct BasicExpression : public
    g::Or<Tuple, Reference>
{
    std::unique_ptr<ast::Expression> make() const
    {
        return monoMake<ast::Expression>();
    }
};

struct DotExpression : public
    g::And<g::Opt<dot>, g::OneOrMore2<BasicExpression, dot>>
{
    std::unique_ptr<ast::Expression> make() const
    {
        bool global = factor<0>().capture() != nullptr;
        auto const& list = factor<1>().captures();
        std::vector<std::unique_ptr<ast::Expression>> exprs;
        for ( auto const& be : list )
            exprs.emplace_back(be.make());

        if ( !global && exprs.size() == 1 )
            return std::move(exprs.front());

        return std::make_unique<ast::DotExpression>(global, std::move(exprs));
    }
};

struct Symbol : public
    g::Or<
          g::And<id, g::Opt<TupleSymbol>>
        , TupleSymbol
         >
{
    ast::Symbol make() const
    {
        switch (index()) {
        case 0: 
            if ( auto p = term<0>().factor<1>().capture() )
                return ast::Symbol(term<0>().factor<0>().token(), std::move(p->make()->internalExpressions()));
            else
                return ast::Symbol(term<0>().factor<0>().token());

        case 1:
            return ast::Symbol(lexer::Token(), std::move(term<1>().make()->internalExpressions()));

        default:
            throw std::runtime_error("invalid symbol expression");
        }
    }
};

struct ProcedureDeclaration : public
    g::And<
        openParen,
        g::Repeat2<Expression, comma>,
        closeParen,
        g::Opt<g::And<g::Or<map, colon>, Expression>>>
{
    std::unique_ptr<ast::ProcedureDeclaration> make() const
    {
        ast::Pattern pattern;
        for ( auto& e : factor<1>().captures() )
            pattern.emplace_back(e.make());

        std::unique_ptr<ast::Expression> returnTypeExpression;
        if ( auto r = factor<3>().capture() )
            returnTypeExpression = r->factor<1>().make();

        auto tok = factor<0>().token();
        return std::make_unique<ast::ProcedureDeclaration>(ast::Symbol(lexer::Token(lexer::TokenKind::Identifier, tok.line(), tok.column(), ""), std::move(pattern)),
                                                           std::move(returnTypeExpression));
    }
};

struct ImplicitProcedureTemplateDeclaration : public
    g::And<Symbol, ProcedureDeclaration>
{
    std::tuple<ast::Symbol, std::unique_ptr<ast::ProcedureDeclaration>> make() const
    {
        return std::make_tuple(factor<0>().make(), factor<1>().make());
    }
};

struct VarDecl
{
    lexer::Token token;
    std::unique_ptr<ast::Expression> constraint;
    std::unique_ptr<ast::Expression> expression;
};

struct ExplicitVariableDeclaration : public
    g::And<colonEqual
         , id
         , g::Opt<g::And<colon, Expression>>
         , g::Opt<g::And<equal, Expression>>>
{
    VarDecl make() const
    {
        std::unique_ptr<ast::Expression> constraint;
        if ( auto c = factor<2>().capture() )
            constraint = c->factor<1>().make();

        std::unique_ptr<ast::Expression> init;
        if ( auto i = factor<3>().capture() )
            init = i->factor<1>().make();

        return { factor<1>().token(), std::move(constraint), std::move(init) };
    }
};

struct ImplicitVariableDeclaration : public
    g::And<id
         , colon
         , g::Opt<Expression>
         , equal
         , g::Opt<Expression>>
{
    VarDecl make() const
    {
        std::unique_ptr<ast::Expression> constraint;
        if ( auto c = factor<2>().capture() )
            constraint = c->make();

        std::unique_ptr<ast::Expression> init;
        if ( auto i = factor<4>().capture() )
            init = i->make();

        return { factor<0>().token(), std::move(constraint), std::move(init) };
    }
};

struct VariableDeclaration : public
    g::Or<ExplicitVariableDeclaration, ImplicitVariableDeclaration>
{
    VarDecl make() const
    {
        if ( index() == 0 )
            return term<0>().make();
        else
            return term<1>().make();
    }
};

struct SymbolDeclaration : public
    g::And<Symbol, equal, Expression>
{
    std::unique_ptr<ast::SymbolDeclaration> make() const
    {
        return std::make_unique<ast::SymbolDeclaration>(factor<0>().make(), factor<2>().make());
    }
};

struct ImportDeclaration : public
    g::And<_import, g::OneOrMore2<id, dot>>
{
    std::unique_ptr<ast::ImportDeclaration> make() const
    {
        std::vector<lexer::Token> modulePath;
        for ( auto const& e : factor<1>().captures() )
            modulePath.emplace_back(e.token());

        return std::make_unique<ast::ImportDeclaration>(std::move(modulePath));
    }
};

struct DataSumDeclaration : public
    g::And<colonPipe, Symbol>
{
    std::unique_ptr<ast::DataSumDeclaration> make() const
    {
        return std::make_unique<ast::DataSumDeclaration>(factor<1>().make());
    }
};

struct DataSumConstructor : public
    g::And<Symbol, g::Opt<g::And<openParen, g::Repeat2<g::And<id, colon, Expression>, comma>, closeParen>>>
{
    std::unique_ptr<ast::DataSumDeclaration::Constructor> make() const
    {
        throw std::runtime_error("not implemented");
    }
};

struct DataProductDeclaration : public
    g::And<colonAmpersand, Symbol>
{
    std::unique_ptr<ast::DataProductDeclaration> make() const
    {
        return std::make_unique<ast::DataProductDeclaration>(factor<1>().make());
    }
};

struct DataProductDeclarationField : public
    g::And<id, colon, Expression, g::Opt<g::And<equal, Expression>>>
{
    std::unique_ptr<ast::DataProductDeclaration::Field> make() const
    {
        std::unique_ptr<ast::Expression> init;
        if ( auto c = factor<3>().capture() )
            init = c->factor<1>().make();

        return std::make_unique<ast::DataProductDeclaration::Field>(ast::Symbol(factor<0>().token()), factor<2>().make(), std::move(init));
    }
};

struct Attribute : public
    g::And<at, Expression>
{
    std::unique_ptr<ast::Expression> make() const
    {
        return factor<1>().make();
    }
};

    } // namespace parser
} // namespace kyfoo
