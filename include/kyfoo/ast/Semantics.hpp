#pragma once

#include <string>
#include <vector>
#include <type_traits>

#include <kyfoo/ast/Declarations.hpp>
#include <kyfoo/ast/Expressions.hpp>

namespace kyfoo {
    namespace ast {

class Module;
class Declaration;
class ProcedureDeclaration;
class DeclarationScope;
class Expression;
class Symbol;
class SymbolReference;
class LookupHit;

class IResolver
{
public:
    virtual ~IResolver() = default;

    virtual Module const* module() const = 0;
    virtual LookupHit inScope(SymbolReference const& symbol) const = 0;
    virtual LookupHit matchEquivalent(SymbolReference const& symbol) const = 0;
    virtual LookupHit matchValue(SymbolReference const& symbol) const = 0;
    virtual LookupHit matchProcedure(SymbolReference const& procOverload) const = 0;
};

class ScopeResolver : public IResolver
{
public:
    explicit ScopeResolver(DeclarationScope* scope);

    // IResolver
public:
    Module const* module() const override;
    LookupHit inScope(SymbolReference const& symbol) const override;
    LookupHit matchEquivalent(SymbolReference const& symbol) const override;
    LookupHit matchValue(SymbolReference const& symbol) const override;
    LookupHit matchProcedure(SymbolReference const& procOverload) const override;

public:
    void addSupplementarySymbol(Symbol const& sym);

private:
    DeclarationScope* myScope = nullptr;
    std::vector<Symbol const*> mySupplementarySymbols;
};

class SymbolVariableCreatorFailoverResolver : public IResolver
{
public:
    SymbolVariableCreatorFailoverResolver(IResolver& resolver, Symbol& symbol);
    ~SymbolVariableCreatorFailoverResolver();

public:
    Module const* module() const override;
    LookupHit inScope(SymbolReference const& symbol) const override;
    LookupHit matchEquivalent(SymbolReference const& symbol) const override;
    LookupHit matchValue(SymbolReference const& symbol) const override;
    LookupHit matchProcedure(SymbolReference const& procOverload) const override;

private:
    IResolver* myResolver = nullptr;
    Symbol* mySymbol = nullptr;
};

template <template<class> typename Op>
class ShallowApply
{
public:
    using operator_t = Op<ShallowApply>;

    ShallowApply()
        : myOperator(*this)
    {
    }

    template <typename... Args>
    ShallowApply(Args&&... args)
        : myOperator(*this, std::forward<Args>(args)...)
    {
    }

    template <typename U>
    typename operator_t::result_t operator()(U const&) = delete;

    template<>
    typename operator_t::result_t operator()(Expression const& expr)
    {
#define X(a,b) if ( auto e = expr.as<b>() ) return myOperator.expr##a(*e);
        EXPRESSION_KINDS(X)
#undef X

        throw std::runtime_error("invalid expression kind");
    }

    template <>
    typename operator_t::result_t operator()(Declaration const& decl)
    {
#define X(a,b,c) if ( auto d = decl.as<c>() ) return myOperator.decl##a(*d);
        DECLARATION_KINDS(X)
#undef X

        throw std::runtime_error("invalid declaration kind");
    }

private:
    operator_t myOperator;
};

bool matchEquivalent(Expression const& lhs, Expression const& rhs);
bool matchValue(Expression const& lhs, Expression const& rhs);

bool matchEquivalent(SymbolReference::paramlist_t lhs,
                     SymbolReference::paramlist_t rhs);

bool matchValue(SymbolReference::paramlist_t lhs,
                SymbolReference::paramlist_t rhs);

std::vector<PrimaryExpression*> gatherFreeVariables(Expression& expr);
bool hasFreeVariable(Expression const& expr);

    } // namespace ast
} // namespace kyfoo
