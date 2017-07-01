#pragma once

#include <csetjmp>

#include <memory>
#include <vector>

namespace kyfoo {

    class Diagnostics;
    class Error;

    namespace lexer {
        class Token;
    }

    namespace ast {

class DeclarationScope;
class Expression;
class LookupHit;
class Module;
class Symbol;
class SymbolReference;

class IResolver
{
public:
    virtual ~IResolver() = default;

    virtual Module const* module() const = 0;
    virtual LookupHit matchEquivalent(SymbolReference const& symbol) const = 0;
    virtual LookupHit matchValue(Diagnostics& dgn, SymbolReference const& symbol) const = 0;
    virtual LookupHit matchProcedure(Diagnostics& dgn, SymbolReference const& procOverload) const = 0;
};

class ScopeResolver : public IResolver
{
public:
    explicit ScopeResolver(DeclarationScope* scope);

    // IResolver
public:
    Module const* module() const override;
    LookupHit matchEquivalent(SymbolReference const& symbol) const override;
    LookupHit matchValue(Diagnostics& dgn, SymbolReference const& symbol) const override;
    LookupHit matchProcedure(Diagnostics& dgn, SymbolReference const& procOverload) const override;

public:
    void addSupplementarySymbol(Symbol const& sym);
    LookupHit matchSupplementary(SymbolReference const& symbol) const;

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
    LookupHit matchEquivalent(SymbolReference const& symbol) const override;
    LookupHit matchValue(Diagnostics& dgn, SymbolReference const& symbol) const override;
    LookupHit matchProcedure(Diagnostics& dgn, SymbolReference const& procOverload) const override;

private:
    IResolver* myResolver = nullptr;
    Symbol* mySymbol = nullptr;
};

class Context : public IResolver
{
public:
    Context(Diagnostics& dgn, IResolver& resolver);
    ~Context();

    // IResolver
public:
    Module const* module() const override;
    LookupHit matchEquivalent(SymbolReference const& sym) const override;
    LookupHit matchValue(Diagnostics& dgn, SymbolReference const& sym) const override;
    LookupHit matchProcedure(Diagnostics& dgn, SymbolReference const& sym) const override;

public:
    Error& error(lexer::Token const& token);
    Error& error(Expression const& expr);
    std::size_t errorCount() const;

    LookupHit matchValue(SymbolReference const& sym) const;
    LookupHit matchProcedure(SymbolReference const& sym) const;

    void rewrite(std::unique_ptr<Expression> expr);

    void resolveExpression(std::unique_ptr<Expression>& expression);
    void resolveExpressions(std::vector<std::unique_ptr<Expression>>& expressions);

private:
    Diagnostics* myDiagnostics;
    IResolver* myResolver;
    std::unique_ptr<Expression> myRewrite;
};

    } // namespace ast
} // namespace kyfoo
