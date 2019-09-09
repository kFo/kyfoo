#pragma once

#include <kyfoo/Array.hpp>
#include <kyfoo/Slice.hpp>

namespace kyfoo::ast {

class Declaration;
class Expression;
class IdentifierExpression;
class SymbolVariable;

class Substitutions
{
public:
    using State = kyfoo::u32;
    enum : State
    {
        Mismatch = 1 << 0,
    };

    struct Item
    {
        SymbolVariable const* symVar;
        Expression const* expr;
    };

    struct BYOS {};

public:
    Substitutions();

    Substitutions(BYOS, Declaration const& target, Slice<Item> items);
    Substitutions(BYOS, Declaration const& target, Slice<Expression const*> exprs);

    Substitutions(Declaration const& target, Slice<Expression const*> query);
    Substitutions(Slice<Expression const*> target, Slice<Expression const*> query);

    Substitutions(Substitutions const&) = default;
    Substitutions& operator = (Substitutions const&) = default;

    Substitutions(Substitutions&&) = default;
    Substitutions& operator = (Substitutions&&) = default;

    ~Substitutions();

public:
    bool deduce(Slice<Expression const*> target, Slice<Expression const*> query);
    bool deduce(Slice<Expression const*> target, Expression const& query);
    bool deduce(Expression const& target, Slice<Expression const*> query);
    bool deduce(Expression const& target, Expression const& query);

    bool empty() const;
    uz card() const;

    SymbolVariable const& var(uz index) const;
    Expression const& expr(uz index) const;

    bool bind(SymbolVariable const& symVar, Expression const& expr);

    explicit operator bool() const;

private:
    uz findVarIndex(SymbolVariable const& symVar);
    void setMismatch();

private:
    State myState = 0;
    ab<SymbolVariable const*> myVariables;
    ab<Expression const*> myContexts;
};

} // namespace kyfoo::ast
