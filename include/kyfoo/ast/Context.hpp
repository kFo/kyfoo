#pragma once

#include <memory>
#include <vector>

#include <kyfoo/ast/Symbol.hpp>

namespace kyfoo {

    class Diagnostics;
    class Error;

    namespace lexer {
        class Token;
    }

    namespace ast {

class AxiomsModule;
class DeclarationScope;
class Declaration;
class Expression;
class LookupHit;
class Module;

class IResolver
{
public:
    virtual ~IResolver() = default;

    virtual Module const& module() const = 0;

    virtual LookupHit matchEquivalent(Diagnostics& dgn, SymbolReference const& symbol) const = 0;
    virtual LookupHit matchCovariant(Diagnostics& dgn, SymbolReference const& symbol) = 0;
};

class ScopeResolver : public IResolver
{
public:
    explicit ScopeResolver(DeclarationScope& scope);
    explicit ScopeResolver(DeclarationScope const& scope);

    ScopeResolver(ScopeResolver&& rhs);
    ScopeResolver& operator = (ScopeResolver&& rhs);

    ~ScopeResolver();

    void swap(ScopeResolver& rhs);

    // IResolver
public:
    Module const& module() const;

    LookupHit matchEquivalent(Diagnostics& dgn, SymbolReference const& symbol) const override;
    LookupHit matchCovariant(Diagnostics& dgn, SymbolReference const& symbol) override;

public:
    void addSupplementaryPrototype(PatternsPrototype const& proto);
    LookupHit matchSupplementary(SymbolReference const& symbol) const;

private:
    DeclarationScope* myScope = nullptr;
    std::vector<PatternsPrototype const*> mySupplementaryPrototypes;
};

class Context
{
public:
    Context(Diagnostics& dgn, IResolver& resolver);
    ~Context();

public:
    AxiomsModule const& axioms() const;
    Module const& module() const;

    Diagnostics& diagnostics();
    Diagnostics const& diagnostics() const;

    IResolver& resolver();
    IResolver const& resolver() const;

public:
    Error& error(lexer::Token const& token);
    Error& error(Expression const& expr);
    Error& error(Declaration const& decl);
    std::size_t errorCount() const;

    LookupHit matchCovariant(SymbolReference const& sym) const;

    IResolver* changeResolver(IResolver& resolver);
    void rewrite(std::unique_ptr<Expression> expr);

    void resolveExpression(std::unique_ptr<Expression>& expression);
    void resolveExpressions(std::vector<std::unique_ptr<Expression>>::iterator left,
                            std::vector<std::unique_ptr<Expression>>::iterator right);
    void resolveExpressions(std::vector<std::unique_ptr<Expression>>& expressions);

private:
    Diagnostics* myDiagnostics;
    IResolver* myResolver;
    std::unique_ptr<Expression> myRewrite;
};

    } // namespace ast
} // namespace kyfoo
