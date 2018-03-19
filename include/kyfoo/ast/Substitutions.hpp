#pragma once

#include <cstdint>

#include <vector>

#include <kyfoo/Slice.hpp>

namespace kyfoo {
    namespace ast {

class Declaration;
class Expression;
class IdentifierExpression;
class SymbolVariable;

class Substitutions
{
public:
    enum State
    {
        Mismatch = 1 << 0,
    };

    using state_t = std::uint32_t;

public:
    Substitutions() = default;

    Substitutions(Declaration const& target, Slice<Expression*> const& query);
    Substitutions(Slice<Expression*> const& lhs, Slice<Expression*> const& rhs);

    Substitutions(Substitutions const&) = default;
    Substitutions& operator = (Substitutions const&) = default;

    Substitutions(Substitutions&&) = default;
    Substitutions& operator = (Substitutions&&) = default;

    ~Substitutions();

public:
    bool deduce(Slice<Expression*> lhs, Slice<Expression*> rhs);
    bool deduce(Slice<Expression*> target, Expression const& query);
    bool deduce(Expression const& target, Slice<Expression*> query);
    bool deduce(Expression const& lhs, Expression const& rhs);

    bool empty() const;
    std::size_t size() const;

    SymbolVariable const& var(std::size_t index) const;
    Expression const& expr(std::size_t index) const;

    bool bind(SymbolVariable const& symVar, Expression const& expr);

    explicit operator bool() const;

private:
    std::size_t findVarIndex(SymbolVariable const& symVar);
    void setMismatch();

private:
    state_t myState = 0;
    std::vector<SymbolVariable const*> myVariables;
    std::vector<Expression const*> myContexts;
};

    } // namespace ast
} // namespace kyfoo
