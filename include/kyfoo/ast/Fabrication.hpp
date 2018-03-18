#pragma once

#include <memory>
#include <utility>
#include <vector>

#include <kyfoo/ast/Declarations.hpp>
#include <kyfoo/ast/Expressions.hpp>

namespace kyfoo {
    namespace ast {

inline lexer::Token
makeToken(const char* id, std::size_t line = 0, std::size_t column = 0)
{
    return lexer::Token(lexer::TokenKind::Identifier, line, column, id);
}

inline Symbol
makeSym(lexer::Token const& token, std::vector<std::unique_ptr<Expression>>&& exprs)
{
    return Symbol(token, PatternsPrototype(std::move(exprs)));
}

inline Symbol
makeSym(lexer::Token const& token, Slice<Expression*> exprs)
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
    std::vector<std::unique_ptr<Expression>> exprs;
    exprs.reserve(s.prototype().pattern().size());
    for ( auto const& p : s.prototype().pattern() ) {
        if ( auto decl = getDeclaration(*p) ) {
            if ( auto param = decl->as<ProcedureParameter>() ) {
                exprs.emplace_back(std::make_unique<IdentifierExpression>(param->symbol().token()));
                exprs.back()->addConstraints(ast::clone(param->constraints()));
                continue;
            }
        }

        exprs.emplace_back(ast::clone(p));
    }

    return makeSym(s.token(), std::move(exprs));
}

inline std::unique_ptr<TupleExpression>
createEmptyExpression()
{
    return std::make_unique<TupleExpression>(TupleKind::Open, std::vector<std::unique_ptr<Expression>>());
}

inline std::unique_ptr<IdentifierExpression>
createIdentifier(Declaration const& decl)
{
    return std::make_unique<IdentifierExpression>(lexer::Token(), decl);
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
std::vector<std::unique_ptr<T>>
createPtrList(Args&&... args)
{
    return createList<std::unique_ptr<T>>(std::forward<Args>(args)...);
}

template <typename... Decls>
std::unique_ptr<DotExpression>
createMemberAccess(Decls&&... decls)
{
    std::vector<std::unique_ptr<Expression>> members;
    members.reserve(sizeof...(Decls));

    appendExprList_impl(members, createIdentifier, std::forward<Decls>(decls)...);
    auto decl = members.back()->as<IdentifierExpression>()->declaration();
    auto ret = std::make_unique<DotExpression>(false, std::move(members));
    ret->setDeclaration(*decl);
    return ret;
}

template <typename... Args>
std::unique_ptr<ApplyExpression>
createMemberCall(ProcedureDeclaration const& proc,
                 Declaration const& thisParam,
                 Args&&... exprs)
{
    return std::make_unique<ApplyExpression>(
        createPtrList<Expression>(createMemberAccess(thisParam, proc),
                                  std::forward<Args>(exprs)...));
}

inline std::unique_ptr<TupleExpression>
createTuple(std::vector<std::unique_ptr<Expression>>&& exprs)
{
    return std::make_unique<TupleExpression>(TupleKind::Open, std::move(exprs));
}

inline std::unique_ptr<TupleExpression>
createTuple(Slice<Expression*> exprs)
{
    return createTuple(ast::clone(exprs));
}

    } // namespace ast
} // namespace kyfoo
