#pragma once

#include <kyfoo/parser/Grammar.hpp>

#include <kyfoo/lexer/Token.hpp>

#include <kyfoo/ast/Declarations.hpp>
#include <kyfoo/ast/Expressions.hpp>

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

struct TupleOpen : public
    g::Or<openParen, openBracket>
{
    lexer::Token const& token() const
    {
        switch(index()) {
        case 0: return term<0>().token();
        case 1: return term<1>().token();
        }

        throw std::runtime_error("invalid tuple-open capture");
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
        }

        throw std::runtime_error("invalid tuple-close capture");
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
        }

        return std::make_unique<ast::PrimaryExpression>(tok);
    }
};

class Compound
{
public:
    Compound();
    Compound(Compound const& rhs);
    Compound(Compound&&) = delete;
    ~Compound();

public:
    bool match(kyfoo::lexer::ScanPoint scan, std::size_t& matches);
    std::unique_ptr<ast::Expression> make() const;

private:
    struct impl;
    std::unique_ptr<impl> myGrammar;
};

struct Tuple : public
    g::Or<
        g::And<openParen, closeParen>
      , g::And<TupleOpen, g::Repeat2<Compound, comma>, TupleClose>
        >
{
    std::unique_ptr<ast::TupleExpression> make() const
    {
        Token open;
        Token close;
        std::vector<std::unique_ptr<ast::Expression>> expressions;

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
        }

        return std::make_unique<ast::TupleExpression>(open, close, std::move(expressions));
    }
};

struct ImplicitTuple : public
    g::OneOrMore<Compound>
{
    std::unique_ptr<ast::TupleExpression> make() const
    {
        std::vector<std::unique_ptr<ast::Expression>> expressions;
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

        return std::make_unique<ast::ProcedureParameter>(factor<0>().token(), std::move(type));
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

        return std::make_unique<ast::ProcedureDeclaration>(factor<0>().token(), std::move(parameters), std::move(returnTypeExpression));
    }
};

struct SymbolDeclaration : public
    g::And<id, equal, g::Long<Compound, TypeExpression>>
{
    std::unique_ptr<ast::SymbolDeclaration> make() const
    {
        switch (factor<2>().index()) {
        case 0: return std::make_unique<ast::SymbolDeclaration>(factor<0>().token(), factor<2>().term<0>().make());
        case 1: return std::make_unique<ast::SymbolDeclaration>(factor<0>().token(), factor<2>().term<1>().make());
        }

        throw std::runtime_error("invalid SymbolDeclaration");
    }
};

    } // namespace parser
} // namespace kyfoo

