#pragma once

#include <kyfoo/parser/Grammar.hpp>

#include <kyfoo/lexer/Token.hpp>

#include <kyfoo/ast/ControlFlow.hpp>
#include <kyfoo/ast/Declarations.hpp>
#include <kyfoo/ast/Expressions.hpp>
#include <kyfoo/ast/Fabrication.hpp>
#include <kyfoo/ast/Scopes.hpp>
#include <kyfoo/ast/Semantics.hpp>
#include <kyfoo/ast/Symbol.hpp>

namespace kyfoo::parser {

using id       = g::Terminal<lexer::TokenKind::Identifier>;
using meta     = g::Terminal<lexer::TokenKind::MetaVariable>;
using integer  = g::Terminal<lexer::TokenKind::Integer>;
using rational = g::Terminal<lexer::TokenKind::Rational>;
using string   = g::Terminal<lexer::TokenKind::String>;

using equal  = g::Terminal<lexer::TokenKind::Equal>;
using comma  = g::Terminal<lexer::TokenKind::Comma>;
using dot    = g::Terminal<lexer::TokenKind::Dot>;
using dotdot = g::Terminal<lexer::TokenKind::DotDot>;

using colon          = g::Terminal<lexer::TokenKind::Colon>;
using colonPipe      = g::Terminal<lexer::TokenKind::ColonPipe>;
using colonAmpersand = g::Terminal<lexer::TokenKind::ColonAmpersand>;
using colonEqual     = g::Terminal<lexer::TokenKind::ColonEqual>;
using colonOpenAngle = g::Terminal<lexer::TokenKind::ColonOpenAngle>;
using colonQuestion  = g::Terminal<lexer::TokenKind::ColonQuestion>;
using colonSlash     = g::Terminal<lexer::TokenKind::ColonSlash>;
using colonPlus      = g::Terminal<lexer::TokenKind::ColonPlus>;
using colonMinus     = g::Terminal<lexer::TokenKind::ColonMinus>;
using colonDot       = g::Terminal<lexer::TokenKind::ColonDot>;

using yield = g::Terminal<lexer::TokenKind::Yield>;
using arrow = g::Terminal<lexer::TokenKind::Arrow>;

using at         = g::Terminal<lexer::TokenKind::At>;
using minusMinus = g::Terminal<lexer::TokenKind::MinusMinus>;

using openParen    = g::Terminal<lexer::TokenKind::OpenParen>;
using closeParen   = g::Terminal<lexer::TokenKind::CloseParen>;
using openBracket  = g::Terminal<lexer::TokenKind::OpenBracket>;
using closeBracket = g::Terminal<lexer::TokenKind::CloseBracket>;
using openAngle    = g::Terminal<lexer::TokenKind::OpenAngle>;
using closeAngle   = g::Terminal<lexer::TokenKind::CloseAngle>;

using _import = g::Terminal<lexer::TokenKind::_import>;

class DeclarationScopeParser;

struct Literal :
    g::Or<integer, rational, string>
{
    Box<ast::LiteralExpression> make(DeclarationScopeParser& parser)
    {
        return mk<ast::LiteralExpression>(monoMake<lexer::Token>(parser));
    }
};

struct Identifier :
    g::Or<id, meta>
{
    Box<ast::IdentifierExpression> make(DeclarationScopeParser& parser)
    {
        return mk<ast::IdentifierExpression>(monoMake<lexer::Token>(parser));
    }
};

struct Primary :
    g::Or<Identifier, Literal>
{
    Box<ast::Expression> make(DeclarationScopeParser& parser)
    {
        return monoMakePtr<ast::Expression>(parser);
    }
};

template <typename T>
class Defer
{
public:
    Defer();

    Defer(Defer const& rhs);
    Defer& operator = (Defer const& rhs);

    Defer(Defer&& rhs);
    Defer& operator = (Defer&& rhs);

    ~Defer();

    void swap(Defer& rhs);

public:
    bool match(kyfoo::lexer::ScanPoint scan, uz& matches);
    Box<ast::Expression> make(DeclarationScopeParser& parser);

private:
    Box<T> myGrammar;
};

template <typename T>
struct AssignExpression;
struct Lambda;
using Expression = Defer<AssignExpression<Lambda>>;

inline std::vector<Box<ast::Expression>>
expressions(DeclarationScopeParser& parser, std::vector<Expression>& rhs)
{
    std::vector<Box<ast::Expression>> ret;
    for ( auto& e : rhs )
        ret.emplace_back(e.make(parser));

    return ret;
}

inline Box<ast::TupleExpression>
createTuple(ast::TupleKind kind,
            std::vector<Box<ast::Expression>>&& expressions)
{
    return mk<ast::TupleExpression>(kind, std::move(expressions));
}

inline Box<ast::TupleExpression>
createTuple(lexer::Token& open,
            lexer::Token& close,
            std::vector<Box<ast::Expression>>&& expressions)
{
    return mk<ast::TupleExpression>(open, close, std::move(expressions));
}

struct TupleOpen :
    g::And<openParen, g::Repeat2<Expression, comma>, closeParen>
{
    Box<ast::TupleExpression> make(DeclarationScopeParser& parser)
    {
        return createTuple(factor<0>().token(),
                           factor<2>().token(),
                           expressions(parser, factor<1>().captures()));
    }
};

using NonLambdaExpression = Defer<AssignExpression<TupleOpen>>;

Box<ast::ProcedureDeclaration> makeProc(DeclarationScopeParser& parser,
                                                    lexer::Token& start,
                                                    std::vector<Expression>& paramExprs,
                                                    NonLambdaExpression* returnExpr)
{
    ast::Pattern pattern = parser.parameterContext();
    for ( auto& e : paramExprs )
        pattern.emplace_back(e.make(parser));

    Box<ast::Expression> returnTypeExpression;
    if ( returnExpr )
        returnTypeExpression = returnExpr->make(parser);

    return mk<ast::ProcedureDeclaration>(ast::Symbol(lexer::Token(lexer::TokenKind::Identifier, start.line(), start.column(), ""), std::move(pattern)),
                                                        std::move(returnTypeExpression));
}

struct ProcedureDeclaration :
    g::And<
        openParen,
        g::Repeat2<Expression, comma>,
        closeParen,
        g::Opt<g::And<arrow, NonLambdaExpression>>>
{
    Box<ast::ProcedureDeclaration> make(DeclarationScopeParser& parser)
    {
        return makeProc(parser,
                        factor<0>().token(),
                        factor<1>().captures(),
                        factor<3>().capture() ? &factor<3>().capture()->factor<1>() : nullptr);
    }
};

template <typename T>
struct Scope :
    g::Nest<lexer::TokenKind::IndentGT, lexer::TokenKind::IndentLT>
{
    Box<T> make(DeclarationScopeParser& parser, typename T::declaration_t& decl)
    {
        lexer::Scanner scanner(std::move(captures()));
        auto scope = mk<T>(parser.scope(), decl);
        parseScope(mk<DeclarationScopeParser>(parser.diagnostics(), scanner, *scope));
        return scope;
    }
};

struct Lambda :
    g::And<openParen
         , g::Repeat2<Expression, comma>
         , closeParen
         , g::Opt<g::And<arrow, NonLambdaExpression>>
         , g::Opt<g::And<yield, g::Or<g::And<g::Opt<colonDot>, Expression>, Scope<ast::ProcedureScope>>>>>
{
    Box<ast::Expression> make(DeclarationScopeParser& parser)
    {
        auto arrow = factor<3>().capture();
        auto yield = factor<4>().capture();
        if ( yield ) {
            auto procDecl = makeProc(parser,
                                     factor<0>().token(),
                                     factor<1>().captures(),
                                     arrow ? &arrow->factor<1>() : nullptr);

            Box<ast::ProcedureScope> procScope;
            if ( yield->factor<1>().index() == 0 ) {
                procScope = mk<ast::ProcedureScope>(parser.scope(), *procDecl);
                auto expr = yield->factor<1>().term<0>().factor<1>().make(parser);
                lexer::Token tok;
                if ( auto retTok = yield->factor<1>().term<0>().factor<0>().capture() )
                    tok = retTok->token();
                else
                    tok = front(*expr);

                procScope->basicBlocks().back()->setJunction(mk<ast::ReturnJunction>(tok, std::move(expr)));
            }
            else {
                procScope = yield->factor<1>().term<1>().make(parser, *procDecl);
            }

            procDecl->define(*procScope);

            auto proc = procDecl.get();
            parser.scope().appendLambda(std::move(procDecl), std::move(procScope));
            return mk<ast::LambdaExpression>(yield->factor<0>().token(), proc);
        }

        auto tuple = createTuple(factor<0>().token(),
                                 factor<2>().token(),
                                 expressions(parser, factor<1>().captures()));
        if ( arrow )
            return mk<ast::ArrowExpression>(std::move(tuple),
                                                          arrow->factor<1>().make(parser));

        return std::move(tuple);
    }
};

struct TupleOpenRight :
    g::And<openBracket, g::Repeat2<Expression, comma>, closeParen>
{
    Box<ast::TupleExpression> make(DeclarationScopeParser& parser)
    {
        return createTuple(factor<0>().token(),
                           factor<2>().token(),
                           expressions(parser, factor<1>().captures()));
    }
};

struct TupleOpenLeft :
    g::And<openParen, g::Repeat2<Expression, comma>, closeBracket>
{
    Box<ast::TupleExpression> make(DeclarationScopeParser& parser)
    {
        return createTuple(factor<0>().token(),
                           factor<2>().token(),
                           expressions(parser, factor<1>().captures()));
    }
};

struct TupleClosed :
    g::And<openBracket, g::Repeat2<Expression, comma>, closeBracket>
{
    Box<ast::TupleExpression> make(DeclarationScopeParser& parser)
    {
        return createTuple(factor<0>().token(),
                           factor<2>().token(),
                           expressions(parser, factor<1>().captures()));
    }
};

struct SymbolExpression :
    g::And<id, openAngle, g::Repeat2<Expression, comma>, closeAngle>
{
    Box<ast::SymbolExpression> make(DeclarationScopeParser& parser)
    {
        return mk<ast::SymbolExpression>(factor<0>().token(),
                                                       expressions(parser, factor<2>().captures()));
    }
};

struct Tuple :
    g::Or<TupleOpenLeft, TupleOpenRight, TupleClosed, SymbolExpression>
{
    Box<ast::Expression> make(DeclarationScopeParser& parser)
    {
        return monoMakePtr<ast::Expression>(parser);
    }
};

template <typename T>
struct BasicExpression :
    g::Or<T, Tuple, Primary>
{
    Box<ast::Expression> make(DeclarationScopeParser& parser)
    {
        return monoMakePtr<ast::Expression>(parser);
    }
};

template <typename T>
struct DotExpression :
    g::And<g::Opt<dot>, g::OneOrMore2<BasicExpression<T>, dot>>
{
    Box<ast::Expression> make(DeclarationScopeParser& parser)
    {
        bool global = factor<0>().capture() != nullptr;
        auto& list = factor<1>().captures();
        std::vector<Box<ast::Expression>> exprs;
        for ( auto& be : list )
            exprs.emplace_back(be.make(parser));

        if ( !global && exprs.size() == 1 )
            return std::move(exprs.front());

        return mk<ast::DotExpression>(global, std::move(exprs));
    }
};

template <typename T>
struct RangeExpression :
    g::Or<
        g::And<g::OneOrMore2<DotExpression<T>, dotdot>, g::Opt<dotdot>>
      , g::And<dotdot, g::OneOrMore2<DotExpression<T>, dotdot>, g::Opt<dotdot>>
      , dotdot
    >
{
    Box<ast::Expression> make(DeclarationScopeParser& parser)
    {
        if ( index() == 0 ) {
            auto& c = term<0>().factor<0>().captures();
            Box<ast::Expression> from = c[0].make(parser);
            for ( uz i = 1; i < c.size(); ++i )
                from = mk<ast::TupleExpression>(ast::createPtrList<ast::Expression>(std::move(from)),
                                                              c[i].make(parser));

            if ( auto cc = term<0>().factor<1>().capture() )
                return mk<ast::TupleExpression>(ast::createPtrList<ast::Expression>(std::move(from)),
                                                              nullptr);

            return std::move(from);
        }
        else if ( index() == 1 ) {
            Box<ast::Expression> from;
            for ( auto& c : term<1>().factor<1>().captures() )
                from = mk<ast::TupleExpression>(ast::createPtrList<ast::Expression>(std::move(from)),
                                                              c.make(parser));

            if ( auto c = term<1>().factor<2>().capture() )
                return mk<ast::TupleExpression>(ast::createPtrList<ast::Expression>(std::move(from)),
                                                              nullptr);

            return std::move(from);
        }

        return mk<ast::TupleExpression>(ast::createPtrList<ast::Expression>(), nullptr);
    }
};

template <typename T>
struct ApplyExpression :
    g::OneOrMore<RangeExpression<T>>
{
    Box<ast::Expression> make(DeclarationScopeParser& parser)
    {
        if ( captures().size() == 1 )
            return captures().front().make(parser);

        std::vector<Box<ast::Expression>> exprs;
        exprs.reserve(captures().size());
        for ( auto& c : captures() )
            exprs.emplace_back(c.make(parser));

        return mk<ast::ApplyExpression>(std::move(exprs));
    }
};

template <typename T>
struct ConstraintExpression :
    g::OneOrMore2<ApplyExpression<T>, colon>
{
    Box<ast::Expression> make(DeclarationScopeParser& parser)
    {
        auto ret = captures().front().make(parser);
        for ( uz i = 1; i < captures().size(); ++i )
            ret->addConstraint(captures()[i].make(parser));

        return ret;
    }
};

template <typename T>
struct AssignExpression :
    g::OneOrMore2<ConstraintExpression<T>, equal>
{
    Box<ast::Expression> make(DeclarationScopeParser& parser)
    {
        auto& c = captures();
        Box<ast::Expression> ret = c.back().make(parser);
        for ( uz i = c.size() - 2; ~i; --i )
            ret = mk<ast::AssignExpression>(c[i].make(parser),
                                                          std::move(ret));

        return ret;
    }
};

struct Symbol :
    g::Or<SymbolExpression, id>
{
    ast::Symbol make(DeclarationScopeParser& parser)
    {
        if ( index() == 0 )
            return ast::Symbol(term<0>().make(parser));
        else
            return ast::Symbol(term<1>().token());
    }
};

struct ImplicitProcedureTemplateDeclaration :
    g::And<Symbol, ProcedureDeclaration>
{
    std::tuple<ast::Symbol, Box<ast::ProcedureDeclaration>> make(DeclarationScopeParser& parser)
    {
        return {factor<0>().make(parser), factor<1>().make(parser)};
    }
};

struct VarDecl
{
    lexer::Token token;
    std::vector<Box<ast::Expression>> constraints;
    Box<ast::Expression> initializer;
};

struct VariableDeclaration :
    g::And<colonEqual
         , id
         , g::Opt<g::And<colon, ConstraintExpression<Lambda>>>
         , g::Opt<g::And<equal, AssignExpression<Lambda>>>>
{
    VarDecl make(DeclarationScopeParser& parser)
    {
        std::vector<Box<ast::Expression>> constraints;
        if ( auto c = factor<2>().capture() )
            constraints = flattenConstraints(c->factor<1>().make(parser));

        Box<ast::Expression> init;
        if ( auto i = factor<3>().capture() )
            init = i->factor<1>().make(parser);

        return { factor<1>().token(), std::move(constraints), std::move(init) };
    }
};

struct BlockDecl
{
    lexer::Token open;
    lexer::Token id;
    Box<ast::Expression> expr;
};

struct BlockDeclaration :
    g::And<colonOpenAngle
         , g::Opt<id>
         , closeAngle
         , g::Opt<Expression>>
{
    BlockDecl make(DeclarationScopeParser& parser)
    {
        lexer::Token id;
        if ( auto c = factor<1>().capture() )
            id = c->token();

        Box<ast::Expression> expr;
        if ( auto c = factor<3>().capture() )
            expr = c->make(parser);

        return { factor<0>().token(), id, std::move(expr) };
    }
};

struct BranchJunction :
    g::And<colonQuestion, Expression>
{
    Box<ast::BranchJunction> make(DeclarationScopeParser& parser)
    {
        return mk<ast::BranchJunction>(factor<0>().token(), factor<1>().make(parser));
    }
};

struct BranchElseJunction :
    g::And<colonSlash, g::Opt<Expression>>
{
    Box<ast::BranchJunction> make(DeclarationScopeParser& parser)
    {
        Box<ast::Expression> cond;
        if ( auto e = factor<1>().capture() )
            cond = e->make(parser);

        return mk<ast::BranchJunction>(factor<0>().token(), std::move(cond));
    }
};

struct ReturnJunction :
    g::And<colonDot, g::Opt<Expression>>
{
    Box<ast::ReturnJunction> make(DeclarationScopeParser& parser)
    {
        Box<ast::Expression> expr;
        if ( auto c = factor<1>().capture() )
            expr = c->make(parser);

        return mk<ast::ReturnJunction>(factor<0>().token(), std::move(expr));
    }
};

struct JumpJunction :
    g::And<g::Or<colonPlus, colonMinus>, g::Opt<id>>
{
    Box<ast::JumpJunction> make(DeclarationScopeParser&)
    {
        lexer::Token label;
        if ( auto c = factor<1>().capture() )
            label = c->token();

        auto anchor = factor<0>().index() == 0 ? factor<0>().term<0>().token() : factor<0>().term<1>().token();
        auto kind = factor<0>().index() == 0 ? ast::JumpJunction::JumpKind::Loop : ast::JumpJunction::JumpKind::Break;
        return mk<ast::JumpJunction>(anchor, kind, label);
    }
};

struct SymbolDeclaration :
    g::And<Symbol, colonEqual, Expression>
{
    Box<ast::SymbolDeclaration> make(DeclarationScopeParser& parser)
    {
        return mk<ast::SymbolDeclaration>(factor<0>().make(parser), factor<2>().make(parser));
    }
};

struct ImportDeclaration :
    g::And<_import, g::OneOrMore2<id, dot>>
{
    Box<ast::ImportDeclaration> make(DeclarationScopeParser&)
    {
        std::vector<lexer::Token> modulePath;
        for ( auto& e : factor<1>().captures() )
            modulePath.emplace_back(e.token());

        return mk<ast::ImportDeclaration>(std::move(modulePath));
    }
};

struct DataSumDeclaration :
    g::And<colonPipe, Symbol>
{
    Box<ast::DataSumDeclaration> make(DeclarationScopeParser& parser)
    {
        return mk<ast::DataSumDeclaration>(factor<1>().make(parser));
    }
};

struct DataSumConstructor :
    g::And<Symbol, g::Opt<g::And<openParen, g::Repeat2<g::And<id, colon, Expression>, comma>, closeParen>>>
{
    Box<ast::DataSumDeclaration::Constructor> make(DeclarationScopeParser&)
    {
        throw std::runtime_error("not implemented");
    }
};

struct DataProductDeclaration :
    g::And<colonAmpersand, Symbol>
{
    Box<ast::DataProductDeclaration> make(DeclarationScopeParser& parser)
    {
        return mk<ast::DataProductDeclaration>(factor<1>().make(parser));
    }
};

struct DataProductDeclarationField :
    g::And<id, colon, Expression, g::Opt<g::And<equal, Expression>>>
{
    Box<ast::DataProductDeclaration::Field> make(DeclarationScopeParser& parser)
    {
        Box<ast::Expression> init;
        if ( auto c = factor<3>().capture() )
            init = c->factor<1>().make(parser);

        return mk<ast::DataProductDeclaration::Field>(ast::Symbol(factor<0>().token()), flattenConstraints(factor<2>().make(parser)), std::move(init));
    }
};

struct Attribute :
    g::And<at, Expression>
{
    Box<ast::Expression> make(DeclarationScopeParser& parser)
    {
        return factor<1>().make(parser);
    }
};

//
// Defer

template <typename T>
Defer<T>::Defer() = default;

template <typename T>
Defer<T>::Defer(Defer const& rhs)
    : myGrammar(rhs.myGrammar ? mk<T>(*rhs.myGrammar) : nullptr)
{
}

template <typename T>
Defer<T>& Defer<T>::operator = (Defer const& rhs)
{
    Expression(rhs).swap(*this);
    return *this;
}

template <typename T>
Defer<T>::Defer(Defer&& rhs)
    : myGrammar(std::move(rhs.myGrammar))
{
}

template <typename T>
Defer<T>& Defer<T>::operator = (Defer&& rhs)
{
    myGrammar = std::move(rhs.myGrammar);
    return *this;
}

template <typename T>
Defer<T>::~Defer() = default;

template <typename T>
void Defer<T>::swap(Defer& rhs)
{
    using std::swap;
    swap(myGrammar, rhs.myGrammar);
}

template <typename T>
bool Defer<T>::match(kyfoo::lexer::ScanPoint scan, uz& matches)
{
    if ( !myGrammar )
        myGrammar = mk<T>();

    if ( myGrammar->match(scan, matches) )
        return scan.commit();

    return false;
}

template <typename T>
Box<ast::Expression> Defer<T>::make(DeclarationScopeParser& parser)
{
    return myGrammar->make(parser);
}

} // namespace kyfoo::parser
