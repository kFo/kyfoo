#pragma once

#include <kyfoo/parser/Grammar.hpp>

#include <kyfoo/lexer/Token.hpp>

#include <kyfoo/ast/Declarations.hpp>
#include <kyfoo/ast/ValueExpressions.hpp>
#include <kyfoo/ast/Symbol.hpp>

namespace kyfoo {
    namespace parser {

using lexer::TokenKind;
using lexer::Token;

using id = g::Terminal<TokenKind::Identifier>;
using integer = g::Terminal<TokenKind::Integer>;
using decimal = g::Terminal<TokenKind::Decimal>;
using string = g::Terminal<TokenKind::String>;
using comma = g::Terminal<TokenKind::Comma>;
using equal = g::Terminal<TokenKind::Equal>;
using colon = g::Terminal<TokenKind::Colon>;
using yield = g::Terminal<TokenKind::Yield>;
using openParen = g::Terminal<TokenKind::OpenParen>;
using closeParen = g::Terminal<TokenKind::CloseParen>;
using openBracket = g::Terminal<TokenKind::OpenBracket>;
using closeBracket = g::Terminal<TokenKind::CloseBracket>;
using openAngle = g::Terminal<TokenKind::OpenAngle>;
using closeAngle = g::Terminal<TokenKind::CloseAngle>;
using _import = g::Terminal<TokenKind::_import>;
using _type = g::Terminal<TokenKind::_type>;

struct TupleOpen : public
    g::Or<openParen, openBracket>
{
    lexer::Token const& token() const
    {
        switch(index()) {
        case 0: return term<0>().token();
        case 1: return term<1>().token();
        default:
            throw std::runtime_error("invalid tuple-open capture");
        }
    }
};

struct TupleClose : public
    g::Or<closeParen, closeBracket>
{
    lexer::Token const& token() const
    {
        switch(index()) {
        case 0: return term<0>().token();
        case 1: return term<1>().token();
        default:
            throw std::runtime_error("invalid tuple-close capture");
        }
    }
};

struct Primary : public
    g::Or<id, integer, decimal, string>
{
    std::unique_ptr<ast::PrimaryExpression> make() const
    {
        lexer::Token tok;
        switch (index()) {
        case 0: tok = term<0>().token(); break;
        case 1: tok = term<1>().token(); break;
        case 2: tok = term<2>().token(); break;
        case 3: tok = term<3>().token(); break;
        default:
            throw std::runtime_error("invalid primary expression");
        }

        return std::make_unique<ast::PrimaryExpression>(tok);
    }
};

class ValueExpression
{
public:
    ValueExpression();
    ValueExpression(ValueExpression const& rhs);
    ValueExpression(ValueExpression&&) = delete;
    ~ValueExpression();

public:
    bool match(kyfoo::lexer::ScanPoint scan, std::size_t& matches);
    std::unique_ptr<ast::ValueExpression> make() const;

private:
    struct impl;
    std::unique_ptr<impl> myGrammar;
};

struct Tuple : public
    g::Or<
        g::And<openParen, closeParen>
      , g::And<TupleOpen, g::Repeat2<ValueExpression, comma>, TupleClose>
        >
{
    std::unique_ptr<ast::TupleExpression> make() const
    {
        Token open;
        Token close;
        std::vector<std::unique_ptr<ast::ValueExpression>> expressions;

        switch (index()) {
        case 0:
        {
            auto const& t = term<0>();
            open = t.factor<0>().token();
            close = t.factor<1>().token();
            break;
        }

        case 1:
        {
            auto const& t = term<1>();
            open = t.factor<0>().token();
            close = t.factor<2>().token();
            for (auto&& e : t.factor<1>().captures())
                expressions.emplace_back(e.make());
            break;
        }

        default:
            throw std::runtime_error("invalid tuple expression");
        }

        return std::make_unique<ast::TupleExpression>(open, close, std::move(expressions));
    }
};

struct ImplicitTuple : public
    g::OneOrMore<ValueExpression>
{
    std::unique_ptr<ast::TupleExpression> make() const
    {
        std::vector<std::unique_ptr<ast::ValueExpression>> expressions;
        for (auto&& e : captures())
            expressions.emplace_back(e.make());

        return std::make_unique<ast::TupleExpression>(std::move(expressions));
    }
};

struct Apply : public
    g::And<id, g::Or<ImplicitTuple, Tuple>>
{
    std::unique_ptr<ast::ApplyExpression> make() const
    {
        std::unique_ptr<ast::TupleExpression> tupleExpression;
        switch (factor<1>().index()) {
        case 0: tupleExpression = factor<1>().term<0>().make(); break;
        case 1: tupleExpression = factor<1>().term<1>().make(); break;
        default:
            throw std::runtime_error("invalid apply expression");
        }

        return std::make_unique<ast::ApplyExpression>(factor<0>().token(), std::move(tupleExpression));
    }
};

class TypeExpression
{
public:
    TypeExpression();
    TypeExpression(TypeExpression const& rhs);
    TypeExpression(TypeExpression&&) = delete;
    ~TypeExpression();

public:
    bool match(kyfoo::lexer::ScanPoint scan, std::size_t& matches);
    std::unique_ptr<ast::TypeExpression> make() const;

private:
    struct impl;
    std::unique_ptr<impl> myGrammar;
};

struct Parameter : public
    g::And<id, g::Opt<g::And<colon, TypeExpression>>>
{
    std::unique_ptr<ast::ProcedureParameter> make() const
    {
        std::unique_ptr<ast::TypeExpression> type;
        if ( factor<1>().capture() )
            type = factor<1>().capture()->factor<1>().make();

        return std::make_unique<ast::ProcedureParameter>(ast::Symbol(factor<0>().token()), std::move(type));
    }
};

struct ProcedureDeclaration : public
    g::And<id, openParen, g::Repeat2<Parameter, comma>, closeParen, g::Opt<g::And<colon, TypeExpression>>>
{
    std::unique_ptr<ast::ProcedureDeclaration> make() const
    {
        std::vector<std::unique_ptr<ast::ProcedureParameter>> parameters;
        for (auto&& e : factor<2>().captures())
            parameters.emplace_back(e.make());

        std::unique_ptr<ast::TypeExpression> returnTypeExpression;
        if ( factor<4>().capture() )
            returnTypeExpression = factor<4>().capture()->factor<1>().make();

        return std::make_unique<ast::ProcedureDeclaration>(ast::Symbol(factor<0>().token()), std::move(parameters), std::move(returnTypeExpression));
    }
};

struct SymbolDeclaration : public
    g::And<id, equal, g::Long<ValueExpression, TypeExpression>>
{
    std::unique_ptr<ast::SymbolDeclaration> make() const
    {
        switch (factor<2>().index()) {
        case 0: return std::make_unique<ast::SymbolDeclaration>(ast::Symbol(factor<0>().token()), factor<2>().term<0>().make());
        case 1: return std::make_unique<ast::SymbolDeclaration>(ast::Symbol(factor<0>().token()), factor<2>().term<1>().make());
        default:
            throw std::runtime_error("invalid SymbolDeclaration");
        }
    }
};

struct ImportDeclaration : public
    g::And<_import, id>
{
    std::unique_ptr<ast::ImportDeclaration> make() const
    {
        return std::make_unique<ast::ImportDeclaration>(ast::Symbol(factor<1>().token()));
    }
};

struct TypeParameters : public
    g::And<
        openAngle
      , g::Repeat2<
            g::Or<
                 g::And<g::Long<ValueExpression, TypeExpression>, g::Opt<g::And<colon, TypeExpression>>>
               , g::And<ValueExpression, colon>
               , g::And<colon, TypeExpression>
                 >
            , comma>
      , closeAngle>
{
    ast::Symbol::paramlist_t make() const
    {
        ast::Symbol::paramlist_t typeParameters;
        auto const& params = factor<1>().captures();
        for ( auto&& p : params ) {
            switch (p.index()) {
            case 0:
            {
                auto const& t = p.term<0>();
                auto c = t.factor<1>().capture();
                std::unique_ptr<ast::TypeExpression> typeConstraint =
                    c ? c->factor<1>().make() : nullptr;
                if ( t.factor<0>().index() == 0 )
                    typeParameters.emplace_back(
                        ast::SymbolParameter(
                            ast::Expression(t.factor<0>().term<0>().make()), std::move(typeConstraint)));
                else
                    typeParameters.emplace_back(
                        ast::SymbolParameter(
                            ast::Expression(t.factor<0>().term<1>().make()), std::move(typeConstraint)));

                break;
            }

            case 1:
            {
                auto const& t = p.term<1>();
                typeParameters.emplace_back(
                    ast::SymbolParameter(t.factor<0>().make()));
                break;
            }

            case 2:
            {
                auto const& t = p.term<2>();
                typeParameters.emplace_back(
                    ast::SymbolParameter(t.factor<1>().make()));
                break;
            }

            default:
                throw std::runtime_error("invalid type declaration");
            }
        }

        return typeParameters;
    }
};

struct TypeDeclaration : public
    g::And<_type, id, g::Opt<TypeParameters>>
{
    std::unique_ptr<ast::TypeDeclaration> make() const
    {
        auto typeName = factor<1>().token();
        ast::Symbol::paramlist_t typeParameters;
        if ( auto p = factor<2>().capture() )
            typeParameters = p->make();

        return std::make_unique<ast::TypeDeclaration>(ast::Symbol(typeName, std::move(typeParameters)));
    }
};

    } // namespace parser
} // namespace kyfoo

