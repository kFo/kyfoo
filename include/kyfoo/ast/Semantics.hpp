#pragma once

#include <string>
#include <vector>

namespace kyfoo {
    namespace ast {

class Module;
class Declaration;
class ProcedureDeclaration;
class DeclarationScope;
class Expression;
class Symbol;
class SymbolReference;

class IResolver
{
public:
    virtual ~IResolver() = default;

    virtual Module const* module() const = 0;
    virtual Declaration const* inScope(SymbolReference const& symbol) const = 0;
    virtual Declaration const* lookup(SymbolReference const& symbol) const = 0;
    virtual Declaration const* match(SymbolReference const& symbol) const = 0;
    virtual ProcedureDeclaration const* matchProcedure(SymbolReference const& procOverload) const = 0;
};

class ScopeResolver : public IResolver
{
public:
    explicit ScopeResolver(DeclarationScope* scope);

    // IResolver
public:
    Module const* module() const override;
    Declaration const* inScope(SymbolReference const& symbol) const override;
    Declaration const* lookup(SymbolReference const& symbol) const override;
    Declaration const* match(SymbolReference const& symbol) const override;
    ProcedureDeclaration const* matchProcedure(SymbolReference const& procOverload) const override;

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
    Declaration const* inScope(SymbolReference const& symbol) const override;
    Declaration const* lookup(SymbolReference const& symbol) const override;
    Declaration const* match(SymbolReference const& symbol) const override;
    ProcedureDeclaration const* matchProcedure(SymbolReference const& procOverload) const override;

private:
    IResolver* myResolver = nullptr;
    Symbol* mySymbol = nullptr;
};

bool matchEquivalent(Expression const& lhs, Expression const& rhs);
bool matchPattern(Expression const& lhs, Expression const& rhs);

    } // namespace ast
} // namespace kyfoo
