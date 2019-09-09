#pragma once

#include <kyfoo/Utilities.hpp>

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

using vacuum   = g::Terminal<lexer::TokenKind::Vacuum>;
using hyphen   = g::Terminal<lexer::TokenKind::Hyphen>;

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
using colonStar      = g::Terminal<lexer::TokenKind::ColonStar>;
using colonQuestion  = g::Terminal<lexer::TokenKind::ColonQuestion>;
using colonSlash     = g::Terminal<lexer::TokenKind::ColonSlash>;
using colonStarAngle     = g::Terminal<lexer::TokenKind::ColonStarAngle>;
using colonQuestionAngle = g::Terminal<lexer::TokenKind::ColonQuestionAngle>;
using colonSlashAngle    = g::Terminal<lexer::TokenKind::ColonSlashAngle>;
using colonOpenAngle = g::Terminal<lexer::TokenKind::ColonOpenAngle>;
using colonPlus      = g::Terminal<lexer::TokenKind::ColonPlus>;
using colonMinus     = g::Terminal<lexer::TokenKind::ColonMinus>;
using colonDot       = g::Terminal<lexer::TokenKind::ColonDot>;

using yield    = g::Terminal<lexer::TokenKind::Yield>;
using arrow    = g::Terminal<lexer::TokenKind::Arrow>;
using question = g::Terminal<lexer::TokenKind::Question>;
using slash    = g::Terminal<lexer::TokenKind::Slash>;

using at         = g::Terminal<lexer::TokenKind::At>;
using minusMinus = g::Terminal<lexer::TokenKind::MinusMinus>;

using openParen    = g::Terminal<lexer::TokenKind::OpenParen>;
using closeParen   = g::Terminal<lexer::TokenKind::CloseParen>;
using openBracket  = g::Terminal<lexer::TokenKind::OpenBracket>;
using closeBracket = g::Terminal<lexer::TokenKind::CloseBracket>;
using openAngle    = g::Terminal<lexer::TokenKind::OpenAngle>;
using closeAngle   = g::Terminal<lexer::TokenKind::CloseAngle>;

using _import = g::Terminal<lexer::TokenKind::_import>;

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

    void swap(Defer& rhs) noexcept;

public:
    bool match(kyfoo::lexer::ScanPoint scan, uz& matches);
    Box<ast::Expression> make(DeclarationScopeParser& parser);

private:
    Box<T> myGrammar;
};

enum ExpressionProperties
{
    Default,
    DisallowLambda,
};

template <ExpressionProperties Prop, template<ExpressionProperties> typename T>
struct pred_expr {};

#define PRECEDES(A,B) \
    template <ExpressionProperties> struct A; \
    template <ExpressionProperties> struct B; \
    template <ExpressionProperties Prop> struct pred_expr<Prop, B> { \
        using type = A<Prop>; };

PRECEDES(BasicExpression      , BasicExpression      )
PRECEDES(BasicExpression      , ApplyHyphenExpression)
PRECEDES(ApplyHyphenExpression, DotExpression        )
PRECEDES(DotExpression        , RangeExpression      )
PRECEDES(RangeExpression      , TightArrowExpression )
PRECEDES(TightArrowExpression , TightLambdaExpression)
PRECEDES(TightLambdaExpression, ApplyExpression      )
PRECEDES(ApplyExpression      , ArrowExpression      )
PRECEDES(ArrowExpression      , LambdaExpression     )
PRECEDES(LambdaExpression     , ConstraintExpression )
PRECEDES(ConstraintExpression , AssignExpression     )

template <ExpressionProperties Prop, template<ExpressionProperties> typename T>
struct Filter
{
    using H = typename pred_expr<Prop, T>::type;

    using type = std::conditional_t<Prop==DisallowLambda,
        std::conditional_t<std::is_same_v<LambdaExpression<Prop>, H>,
            typename pred_expr<Prop, LambdaExpression>::type,
            H>,
        H>;
};

template <ExpressionProperties Prop, template<ExpressionProperties> typename T>
using PredExpr = typename Filter<Prop, T>::type;

// Recursive expressions
using Expression = Defer<AssignExpression<Default>>;
using NonLambdaExpression = Defer<AssignExpression<DisallowLambda>>;

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

Box<ast::ProcedureDeclaration> makeProc(DeclarationScopeParser& parser,
                                        lexer::Token const& start,
                                        ab<Expression>& paramExprs,
                                        NonLambdaExpression* returnExpr)
{
    ast::Pattern pattern = parser.parameterContext();
    for ( auto& e : paramExprs )
        pattern.append(e.make(parser));

    Box<ast::Expression> returnTypeExpression;
    if ( returnExpr )
        returnTypeExpression = returnExpr->make(parser);

    return mk<ast::ProcedureDeclaration>(ast::Symbol(lexer::Token(lexer::TokenKind::Identifier, "", start.location()),
                                                     std::move(pattern)),
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
        lexer::Scanner scanner(parser.scope().module().moduleSet().tokenFactory(),
                               std::move(captures()));
        auto scope = mk<T>(parser.scope(), decl);
        parseScope(mk<DeclarationScopeParser>(parser.diagnostics(), scanner, *scope));
        return scope;
    }
};

inline ab<Box<ast::Expression>>
expressions(DeclarationScopeParser& parser, ab<Expression>& rhs)
{
    ab<Box<ast::Expression>> ret;
    for ( auto& e : rhs )
        ret.append(e.make(parser));

    return ret;
}

inline Box<ast::TupleExpression>
createTuple(ast::TupleKind kind,
            ab<Box<ast::Expression>> expressions)
{
    return mk<ast::TupleExpression>(kind, std::move(expressions));
}

inline Box<ast::TupleExpression>
createTuple(lexer::Token const& open,
            lexer::Token const& close,
            ab<Box<ast::Expression>> expressions)
{
    return mk<ast::TupleExpression>(open, close, std::move(expressions));
}

struct TupleExpression :
    g::And<
        g::Or<openParen, openBracket>
      , g::Repeat2<Expression, comma>
      , g::Or<closeParen, closeBracket>>
{
    Box<ast::TupleExpression> make(DeclarationScopeParser& parser)
    {
        return createTuple(factor<0>().monoMake<lexer::Token>(parser),
                           factor<2>().monoMake<lexer::Token>(parser),
                           expressions(parser, factor<1>().captures()));
    }
};

struct SymbolExpression :
    g::And<id, g::Opt<vacuum>, openAngle, g::Repeat2<Expression, comma>, closeAngle>
{
    Box<ast::SymbolExpression> make(DeclarationScopeParser& parser)
    {
        return mk<ast::SymbolExpression>(factor<0>().token(),
                                         expressions(parser, factor<3>().captures()));
    }
};

struct Tuple :
    g::Or<TupleExpression, SymbolExpression>
{
    Box<ast::Expression> make(DeclarationScopeParser& parser)
    {
        return monoMakePtr<ast::Expression>(parser);
    }
};

template <ExpressionProperties Prop>
struct BasicExpression :
    g::Or<Tuple, Primary>
{
    Box<ast::Expression> make(DeclarationScopeParser& parser)
    {
        return monoMakePtr<ast::Expression>(parser);
    }
};

template <typename Mixin>
struct ApplyMixin :
    Mixin
{
    Box<ast::Expression> make(DeclarationScopeParser& parser)
    {
        if ( this->captures().card() == 1 )
            return this->captures().front().make(parser);

        ab<Box<ast::Expression>> exprs;
        exprs.reserve(this->captures().card());
        for ( auto& c : this->captures() )
            exprs.append(c.make(parser));

        return mk<ast::ApplyExpression>(std::move(exprs));
    }
};

template <ExpressionProperties Prop>
struct ApplyHyphenExpression :
    ApplyMixin<g::OneOrMore2<PredExpr<Prop, ApplyHyphenExpression>, hyphen>>
{
};

template <ExpressionProperties Prop>
struct DotExpression :
    g::And<g::Opt<dot>
         , PredExpr<Prop, DotExpression>
         , g::Repeat<g::And<g::Or<dot, vacuum>, PredExpr<Prop, DotExpression>>>>
{
    Box<ast::Expression> make(DeclarationScopeParser& parser)
    {
        auto global = this->template factor<0>().capture() != nullptr;
        ab<Box<ast::Expression>> exprs;
        exprs.append(this->template factor<1>().make(parser));

        enum { Dot = 0, Apply = 1, Unknown = 2 };
        uz current = global ? Dot : Unknown;

        for ( auto& e : this->template factor<2>().captures() ) {
            auto const index = e.template factor<0>().index();
            if ( current != Unknown && index != current ) {
                Box<ast::Expression> prev;
                if ( current == Dot ) {
                    prev = mk<ast::DotExpression>(global, std::move(exprs));
                    global = false;
                }
                else {
                    prev = mk<ast::ApplyExpression>(std::move(exprs));
                }

                exprs.append(std::move(prev));
            }

            exprs.append(e.template factor<1>().make(parser));
            current = index;
        }

        if ( exprs.card() == 1 )
            return std::move(exprs.front());

        if ( current == Dot )
            return mk<ast::DotExpression>(false, std::move(exprs));

        return mk<ast::ApplyExpression>(std::move(exprs));
    }
};

template <ExpressionProperties Prop>
struct RangeExpression :
    g::Or<
        g::And<g::OneOrMore2<PredExpr<Prop, RangeExpression>, dotdot>, g::Opt<dotdot>>
      , g::And<dotdot, g::OneOrMore2<PredExpr<Prop, RangeExpression>, dotdot>, g::Opt<dotdot>>
      , dotdot
    >
{
    Box<ast::Expression> make(DeclarationScopeParser& parser)
    {
        if ( this->index() == 0 ) {
            auto& c = this->template term<0>().template factor<0>().captures();
            Box<ast::Expression> from = c[0].make(parser);
            for ( uz i = 1; i < c.card(); ++i )
                from = mk<ast::TupleExpression>(ast::createPtrList<ast::Expression>(std::move(from)),
                                                              c[i].make(parser));

            if ( auto cc = this->template term<0>().template factor<1>().capture() )
                return mk<ast::TupleExpression>(ast::createPtrList<ast::Expression>(std::move(from)),
                                                              nullptr);

            return std::move(from);
        }
        else if ( this->index() == 1 ) {
            Box<ast::Expression> from;
            for ( auto& c : this->template term<1>().template factor<1>().captures() )
                from = mk<ast::TupleExpression>(ast::createPtrList<ast::Expression>(std::move(from)),
                                                              c.make(parser));

            if ( auto c = this->template term<1>().template factor<2>().capture() )
                return mk<ast::TupleExpression>(ast::createPtrList<ast::Expression>(std::move(from)),
                                                              nullptr);

            return std::move(from);
        }

        return mk<ast::TupleExpression>(ast::createPtrList<ast::Expression>(), nullptr);
    }
};

template <ExpressionProperties Prop>
struct ApplyExpression :
    ApplyMixin<g::OneOrMore<PredExpr<Prop, ApplyExpression>>>
{
};

template <ExpressionProperties Prop>
struct TightArrowExpression :
    g::OneOrMore2<PredExpr<Prop, TightArrowExpression>, g::And<vacuum, arrow>>
{
    Box<ast::Expression> make(DeclarationScopeParser& parser)
    {
        auto& c = this->captures();
        auto ret = c.front().make(parser);
        for ( uz i = 1, card = c.card(); i < card; ++i )
            ret = mk<ast::ArrowExpression>(std::move(ret), c[i].make(parser));

        return ret;
    }
};

template <ExpressionProperties Prop>
struct ArrowExpression :
    g::OneOrMore2<PredExpr<Prop, ArrowExpression>, arrow>
{
    Box<ast::Expression> make(DeclarationScopeParser& parser)
    {
        auto& c = this->captures();
        auto ret = c.front().make(parser);
        for ( uz i = 1, card = c.card(); i < card; ++i )
            ret = mk<ast::ArrowExpression>(std::move(ret), c[i].make(parser));

        return ret;
    }
};

using LambdaSingleLine = g::And<yield, g::Opt<colonDot>>;

template <ExpressionProperties Prop>
struct TightLambdaExpression :
    g::And<PredExpr<Prop, TightLambdaExpression>
         , g::Opt<g::And<vacuum, LambdaSingleLine, Expression>>>
{
    Box<ast::Expression> make(DeclarationScopeParser& parser)
    {
        auto ret = this->template factor<0>().make(parser);
        if ( auto c = this->template factor<1>().capture() )
            ret = mkLambda(parser.scope(), std::move(ret), c->template factor<2>().make(parser));

        return ret;
    }
};

template <ExpressionProperties Prop>
struct LambdaExpression :
    g::And<PredExpr<Prop, LambdaExpression>
         , g::Opt<g::And<LambdaSingleLine, Expression>>>
{
    Box<ast::Expression> make(DeclarationScopeParser& parser)
    {
        auto ret = this->template factor<0>().make(parser);
        if ( auto c = this->template factor<1>().capture() )
            ret = mkLambda(parser.scope(), std::move(ret), c->template factor<1>().make(parser));

        return ret;
    }
};

template <ExpressionProperties Prop>
struct ConstraintExpression :
    g::OneOrMore2<PredExpr<Prop, ConstraintExpression>, colon>
{
    Box<ast::Expression> make(DeclarationScopeParser& parser)
    {
        auto ret = this->captures().front().make(parser);
        for ( uz i = 1; i < this->captures().card(); ++i )
            ret->appendConstraint(this->captures()[i].make(parser));

        return ret;
    }
};

template <ExpressionProperties Prop>
struct AssignWithLambdaExpression :
    g::And<g::OneOrMore2<PredExpr<Prop, AssignExpression>, equal>
         , g::Opt<g::And<yield, Scope<ast::ProcedureScope>>>>
{
    Box<ast::Expression> make(DeclarationScopeParser& parser)
    {
        auto& c = this->template factor<0>().captures();

        Box<ast::Expression> ret = c.back().make(parser);
        if ( auto tailBlock = this->template factor<1>().capture() ) {
            auto proc = mkProc(std::move(ret));
            auto defn = tailBlock->template factor<1>().make(parser, *proc);
            ret = mk<ast::LambdaExpression>(*proc);
            parser.scope().appendLambda(std::move(proc), std::move(defn));
        }

        for ( uz i = c.card() - 2; ~i; --i )
            ret = mk<ast::AssignExpression>(c[i].make(parser), std::move(ret));

        return ret;
    }
};

template <ExpressionProperties Prop>
struct AssignNoLambdaExpression :
    g::OneOrMore2<PredExpr<Prop, AssignExpression>, equal>
{
    Box<ast::Expression> make(DeclarationScopeParser& parser)
    {
        auto& c = this->captures();
        auto ret = c.back().make(parser);
        for ( uz i = c.card() - 2; ~i; --i )
            ret = mk<ast::AssignExpression>(c[i].make(parser), std::move(ret));

        return ret;
    }
};

template <ExpressionProperties Prop>
struct AssignExpression :
    std::conditional_t<Prop==DisallowLambda,
        AssignNoLambdaExpression<Prop>,
        AssignWithLambdaExpression<Prop>>
{
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

struct ImplicitProcDecl
{
    ast::Symbol templSym;
    Box<ast::ProcedureDeclaration> proc;
};

struct ImplicitProcedureTemplateDeclaration :
    g::And<Symbol, g::Opt<vacuum>, ProcedureDeclaration>
{
    ImplicitProcDecl make(DeclarationScopeParser& parser)
    {
        return { factor<0>().make(parser), factor<2>().make(parser) };
    }
};

struct VarDecl
{
    lexer::Token token;
    ab<Box<ast::Expression>> constraints;
    Box<ast::Expression> initializer;
};

struct VariableDeclaration :
    g::And<colonEqual
         , id
         , g::Opt<g::And<colon, ConstraintExpression<Default>>>
         , g::Opt<g::And<equal, AssignExpression<Default>>>>
{
    VarDecl make(DeclarationScopeParser& parser)
    {
        ab<Box<ast::Expression>> constraints;
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
    lexer::Token label;
    Box<ast::Expression> expr;
};

struct BlockDeclaration :
    g::And<colonOpenAngle
         , g::Opt<id>
         , closeAngle
         , g::Opt<vacuum>
         , g::Opt<Expression>>
{
    BlockDecl make(DeclarationScopeParser& parser)
    {
        lexer::Token id;
        if ( auto c = factor<1>().capture() )
            id = c->token();

        Box<ast::Expression> expr;
        if ( auto c = factor<4>().capture() )
            expr = c->make(parser);

        return { factor<0>().token(), id, std::move(expr) };
    }
};

struct BranchJunction :
    g::Or<
        g::And<colonQuestion, Expression>
      , g::And<colonQuestionAngle, g::Opt<id>, closeAngle, g::Opt<vacuum>, Expression>>
{
    Box<ast::BranchJunction> make(DeclarationScopeParser& parser)
    {
        Box<ast::Expression> cond;
        lexer::Token tok;
        id* label = nullptr;
        if ( index() == 0 ) {
            tok = term<0>().factor<0>().token();
            cond = term<0>().factor<1>().make(parser);
        }
        else {
            tok = term<1>().factor<0>().token();
            cond = term<1>().factor<4>().make(parser);
            label = term<1>().factor<1>().capture();
        }

        return mk<ast::BranchJunction>(tok,
                                       label ? label->token() : lexer::Token(),
                                       std::move(cond));
    }
};

struct BranchElseJunction :
    g::Or<
        g::And<colonSlash, g::Opt<Expression>>
      , g::And<colonSlashAngle, g::Opt<id>, closeAngle, g::Opt<vacuum>, g::Opt<Expression>>
    >
{
    Box<ast::BranchJunction> make(DeclarationScopeParser& parser)
    {
        Box<ast::Expression> cond;
        id* label = nullptr;
        lexer::Token tok;
        if ( index() == 0 ) {
            tok = term<0>().factor<0>().token();
            if ( auto e = term<0>().factor<1>().capture() )
                cond = e->make(parser);
        }
        else {
            tok = term<1>().factor<0>().token();
            if ( auto e = term<1>().factor<4>().capture() )
                cond = e->make(parser);

            label = term<1>().factor<1>().capture();
        }

        return mk<ast::BranchJunction>(tok,
                                       label ? label->token() : lexer::Token(),
                                       std::move(cond));
    }
};

struct LoopJunction :
    g::Or<
        g::And<colonStar, g::Opt<Expression>>
      , g::And<colonStarAngle, g::Opt<id>, closeAngle, g::Opt<vacuum>, g::Opt<Expression>>
    >
{
    Box<ast::BranchJunction> make(DeclarationScopeParser& parser)
    {
        Box<ast::Expression> cond;
        id* label = nullptr;
        lexer::Token tok;
        if ( index() == 0 ) {
            tok = term<0>().factor<0>().token();
            if ( auto e = term<0>().factor<1>().capture() )
                cond = e->make(parser);
        }
        else {
            tok = term<1>().factor<0>().token();
            if ( auto e = term<1>().factor<4>().capture() )
                cond = e->make(parser);

            label = term<1>().factor<1>().capture();
        }

        return mk<ast::BranchJunction>(tok,
                                       label ? label->token() : lexer::Token(),
                                       std::move(cond));
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
        else
            expr = ast::createEmptyExpression(factor<0>().token().location());

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
        auto kind = factor<0>().index() == 0 ? ast::JumpJunction::JumpKind::Continue : ast::JumpJunction::JumpKind::Break;
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
        ab<lexer::Token> modulePath;
        for ( auto& e : factor<1>().captures() )
            modulePath.append(e.token());

        return mk<ast::ImportDeclaration>(std::move(modulePath));
    }
};

struct DataVariation :
    g::And<colonPipe, Symbol>
{
    std::optional<ast::Symbol> make(DeclarationScopeParser& parser)
    {
        return factor<1>().make(parser);
    }
};

struct DataTypeDeclaration :
    g::And<colonAmpersand, Symbol>
{
    Box<ast::DataTypeDeclaration> make(DeclarationScopeParser& parser)
    {
        return mk<ast::DataTypeDeclaration>(factor<1>().make(parser));
    }
};

struct Field
{
    ast::Symbol symbol;
    ab<Box<ast::Expression>> constraints;
    Box<ast::Expression> init;
};

struct DataTypeDeclarationField :
    g::And<id, colon, Expression, g::Opt<g::And<equal, Expression>>>
{
    Field make(DeclarationScopeParser& parser)
    {
        Box<ast::Expression> init;
        if ( auto c = factor<3>().capture() )
            init = c->factor<1>().make(parser);

        return {
            ast::Symbol(factor<0>().token()),
            flattenConstraints(factor<2>().make(parser)),
            std::move(init)
        };
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
void Defer<T>::swap(Defer& rhs) noexcept
{
    using kyfoo::swap;
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
