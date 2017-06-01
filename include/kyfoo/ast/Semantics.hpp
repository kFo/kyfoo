#pragma once

#include <string>
#include <vector>

namespace kyfoo {
    namespace ast {

class Module;
class Declaration;
class DeclarationScope;
class Expression;
class Symbol;

class IResolver
{
public:
    virtual ~IResolver() = default;

    virtual Module const* module() const = 0;
    virtual Declaration const* inScope(std::string const& identifier) const = 0;
    virtual Declaration const* lookup(std::string const& identifier) const = 0;
};

class ScopeResolver : public IResolver
{
public:
    explicit ScopeResolver(DeclarationScope* scope);

    // IResolver
public:
    Module const* module() const override;
    Declaration const* inScope(std::string const& identifier) const override;
    Declaration const* lookup(std::string const& identifier) const override;

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
    Declaration const* inScope(std::string const& identifier) const override;
    Declaration const* lookup(std::string const& identifier) const override;

private:
    IResolver* myResolver = nullptr;
    Symbol* mySymbol = nullptr;
};

bool matchOverload(Expression const& lhs, Expression const& rhs);
bool matchPattern(Expression const& lhs, Expression const& rhs);

    } // namespace ast
} // namespace kyfoo
