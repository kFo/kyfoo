#pragma once

#include <memory>
#include <utility>
#include <vector>

#include <kyfoo/ast/Declarations.hpp>
#include <kyfoo/ast/Expressions.hpp>

namespace kyfoo {
    namespace ast {

inline std::unique_ptr<TupleExpression>
createEmptyExpression()
{
    return std::make_unique<TupleExpression>(TupleKind::Open, std::vector<std::unique_ptr<Expression>>());
}

template <typename Head, typename... Args>
void appendExprList_impl(std::vector<std::unique_ptr<Expression>>& members,
                         Head&& member,
                         Args&&... args)
{
    members.emplace_back(std::make_unique<PrimaryExpression>(lexer::Token()));
    members.back()->setDeclaration(std::forward<Head&&>(member));

    appendExprList_impl(members, args...);
}

inline void appendExprList_impl(std::vector<std::unique_ptr<Expression>>&)
{
}

template <typename... Args>
std::unique_ptr<DotExpression>
createMemberAccess(Args&&... args)
{
    std::vector<std::unique_ptr<Expression>> members;
    members.reserve(sizeof...(Args));

    appendExprList_impl(members, std::forward<Args>(args)...);
    auto decl = members.back()->declaration();
    auto ret = std::make_unique<DotExpression>(false, std::move(members));
    ret->setDeclaration(*decl);
    return ret;
}

template <typename... Args>
std::unique_ptr<ApplyExpression>
createMemberCallExpression(ProcedureDeclaration const& proc,
                           Declaration const& thisParam,
                           Args&&... args)
{
    std::vector<std::unique_ptr<Expression>> members;
    members.reserve(sizeof...(Args)+1);
    members.emplace_back(createMemberAccess(thisParam, proc));

    appendExprList_impl(members, std::forward<Args>(args)...);

    auto ret = std::make_unique<ApplyExpression>(std::move(members));
    ret->setDeclaration(proc);
    return ret;
}

    } // namespace ast
} // namespace kyfoo
