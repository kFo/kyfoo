#pragma once

#include <string>

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
    virtual Declaration const* inScope(std::string const& symbol) const = 0;
    virtual Declaration const* lookup(std::string const& symbol) const = 0;
};

class ScopeResolver : public IResolver
{
public:
    explicit ScopeResolver(DeclarationScope* scope);

public:
    Module const* module() const override;
    Declaration const* inScope(std::string const& symbol) const override;
    Declaration const* lookup(std::string const& symbol) const override;

private:
    DeclarationScope* myScope = nullptr;
};

class SymbolVariableCreatorFailoverResolver : public IResolver
{
public:
    SymbolVariableCreatorFailoverResolver(IResolver& resolver, Symbol& symbol);
    ~SymbolVariableCreatorFailoverResolver();

public:
    Module const* module() const override;
    Declaration const* inScope(std::string const& symbol) const override;
    Declaration const* lookup(std::string const& symbol) const override;

private:
    IResolver* myResolver = nullptr;
    Symbol* mySymbol = nullptr;
};

bool matchOverload(Expression const& lhs, Expression const& rhs);
bool matchPattern(Expression const& lhs, Expression const& rhs);

    } // namespace ast
} // namespace kyfoo
