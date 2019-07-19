#pragma once

#include <utility>
#include <vector>

#include <kyfoo/Types.hpp>
#include <kyfoo/ast/ControlFlow.hpp>
#include <kyfoo/ast/Declarations.hpp>
#include <kyfoo/ast/Expressions.hpp>
#include <kyfoo/ast/Scopes.hpp>
#include <kyfoo/ast/Semantics.hpp>

namespace kyfoo::ast {

inline lexer::Token
mkToken(stringv id, lexer::SourceLocation loc = {})
{
    return lexer::Token(lexer::TokenKind::Identifier, id, loc);
}

inline lexer::Token
mkToken(stringv id, lexer::TokenKind kind, lexer::SourceLocation loc = {})
{
    return lexer::Token(kind, id, loc);
}

inline Symbol
makeSym(lexer::Token const& token, std::vector<Box<Expression>> exprs)
{
    return Symbol(token, PatternsPrototype(std::move(exprs)));
}

inline Symbol
makeSym(lexer::Token const& token, Slice<Expression const*> exprs)
{
    return makeSym(token, ast::clone(exprs));
}

inline Symbol
makeSym(Symbol const& sym)
{
    return Symbol(sym.token(), PatternsPrototype(ast::clone(sym.prototype().pattern())));
}

inline Symbol
copyProcSym(Symbol const& s)
{
    std::vector<Box<Expression>> exprs;
    exprs.reserve(s.prototype().pattern().card());
    for ( auto const& p : s.prototype().pattern() ) {
        if ( auto decl = getDeclaration(*p) ) {
            if ( auto param = decl->as<ProcedureParameter>() ) {
                exprs.emplace_back(mk<IdentifierExpression>(param->symbol().token()));
                exprs.back()->appendConstraints(ast::clone(param->constraints()));
                continue;
            }
        }

        exprs.emplace_back(ast::clone(p));
    }

    return makeSym(s.token(), std::move(exprs));
}

inline Box<TupleExpression>
createEmptyExpression()
{
    return mk<TupleExpression>(TupleKind::Open, std::vector<Box<Expression>>());
}

inline Box<TupleExpression>
createEmptyExpression(lexer::SourceLocation loc)
{
    return mk<TupleExpression>(lexer::Token(lexer::TokenKind::OpenParen, "(", loc),
                               lexer::Token(lexer::TokenKind::CloseParen, ")", loc),
                               std::vector<Box<Expression>>());
}

inline Box<IdentifierExpression>
createIdentifier(lexer::Token const& tok)
{
    return mk<IdentifierExpression>(tok);
}

inline Box<IdentifierExpression>
createIdentifier(lexer::Token const& tok, Declaration const& decl)
{
    return mk<IdentifierExpression>(tok, decl);
}

inline Box<IdentifierExpression>
createIdentifier(Declaration const& decl)
{
    return createIdentifier(lexer::Token(), decl);
}

inline Box<LiteralExpression>
mkIntegerLiteral(const char* lexeme, lexer::SourceLocation loc = {})
{
    return mk<LiteralExpression>(mkToken(sliceCString(lexeme), lexer::TokenKind::Integer, loc));
}

template <typename T, typename F, typename Head, typename... Args>
void appendExprList_impl(std::vector<T>& members,
                         F&& f,
                         Head&& member,
                         Args&&... args)
{
    members.emplace_back(f(std::forward<Head&&>(member)));
    appendExprList_impl<T>(members, std::forward<F&&>(f), std::forward<Args>(args)...);
}

template <typename T, typename F>
void appendExprList_impl(std::vector<T>&, F&&)
{
}

template <typename T, typename... Args>
std::vector<T>
createList(Args&&... args)
{
    std::vector<T> ret;
    ret.reserve(sizeof...(Args));
    auto id = [](auto&& r) { return std::forward<decltype(r)>(r); };
    appendExprList_impl<T>(ret, id, std::forward<Args>(args)...);
    return ret;
}

template <typename T, typename... Args>
std::vector<Box<T>>
createPtrList(Args&&... args)
{
    return createList<Box<T>>(std::forward<Args>(args)...);
}

template <typename... Decls>
Box<DotExpression>
createMemberAccess(Decls&&... decls)
{
    std::vector<Box<Expression>> members;
    members.reserve(sizeof...(Decls));

    Box<IdentifierExpression> (*f)(Declaration const&) = &createIdentifier;
    appendExprList_impl(members, f, std::forward<Decls>(decls)...);
    return mk<DotExpression>(false, std::move(members));
}

template <typename... Args>
Box<ApplyExpression>
createMemberCall(ProcedureDeclaration const& proc,
                 Declaration const& thisParam,
                 Args&&... exprs)
{
    return mk<ApplyExpression>(
        createPtrList<Expression>(createMemberAccess(thisParam, proc),
                                  std::forward<Args>(exprs)...));
}

template <typename... Args>
Box<ApplyExpression>
createApply(Args&&... exprs)
{
    return mk<ApplyExpression>(
        createPtrList<Expression>(std::forward<Args>(exprs)...));
}

inline Box<TupleExpression>
createTuple(std::vector<Box<Expression>>&& exprs)
{
    return mk<TupleExpression>(TupleKind::Open, std::move(exprs));
}

inline Box<TupleExpression>
createTuple(Slice<Expression const*> exprs)
{
    return createTuple(ast::clone(exprs));
}

inline Box<ReturnJunction>
createReturn(lexer::SourceLocation loc,
             Box<Expression> expr)
{
    return mk<ReturnJunction>(mkToken("", loc),
                              std::move(expr));
}

inline Box<ReturnJunction>
createReturn(Box<Expression> expr)
{
    return createReturn(lexer::SourceLocation(), std::move(expr));
}

inline Box<SymbolExpression>
createRefType(lexer::SourceLocation loc, Box<Expression> expr)
{
    return mk<ast::SymbolExpression>(
        ast::mkToken("ref", loc),
        ast::createPtrList<ast::Expression>(std::move(expr)));
}

inline Box<ProcedureDeclaration>
mkProc(Box<Expression> expr)
{
    Box<Expression> paramExpr;
    Box<Expression> returnExpr;
    if ( auto arrow = expr->as<ArrowExpression>() ) {
        paramExpr = arrow->takeFrom();
        returnExpr = arrow->takeTo();
    }
    else {
        paramExpr = std::move(expr);
    }

    auto tok = mkToken("", front(*paramExpr).location());
    auto symParams = createPtrList<Expression>(std::move(paramExpr));
    return mk<ast::ProcedureDeclaration>(
        Symbol(tok, std::move(symParams)),
        std::move(returnExpr));
}

inline Box<LambdaExpression>
mkLambda(Scope& scope, Box<Expression> params, Box<Expression> body)
{
    auto proc = mkProc(std::move(params));
    auto ret = mk<LambdaExpression>(*proc);
    auto defn = mk<ProcedureScope>(scope, *proc);
    defn->basicBlocks().back()->setJunction(mk<ReturnJunction>(front(*body), std::move(body)));
    scope.appendLambda(std::move(proc), std::move(defn));

    return ret;
}

} // namespace kyfoo::ast
