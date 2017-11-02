#pragma once

#include <functional>
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
class Statement;
class Declaration;
class Expression;
class LookupHit;
class Module;

class IResolver
{
public:
    virtual ~IResolver() = default;

    virtual DeclarationScope const& scope() const = 0;

    virtual LookupHit matchEquivalent(SymbolReference const& symbol) const = 0;
    virtual LookupHit matchOverload(Module& endModule, Diagnostics& dgn, SymbolReference const& symbol) = 0;
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
    DeclarationScope const& scope() const;

    LookupHit matchEquivalent(SymbolReference const& symbol) const override;
    LookupHit matchOverload(Module& endModule, Diagnostics& dgn, SymbolReference const& symbol) override;

public:
    void addSupplementaryPrototype(PatternsPrototype& proto);
    LookupHit matchSupplementary(SymbolReference const& symbol) const;

private:
    DeclarationScope* myScope = nullptr;
    std::vector<PatternsPrototype*> mySupplementaryPrototypes;
};

class Context
{
public:
    Context(Module& module, Diagnostics& dgn, IResolver& resolver);
    ~Context();

public:
    AxiomsModule const& axioms() const;
    Module& module();
    Module const& module() const;

    Diagnostics& diagnostics();
    Diagnostics const& diagnostics() const;

    IResolver& resolver();
    IResolver const& resolver() const;

    Statement& statement();
    Statement const& statement() const;

public:
    Error& error(lexer::Token const& token);
    Error& error(Expression const& expr);
    Error& error(Declaration const& decl);
    std::size_t errorCount() const;

    LookupHit matchOverload(SymbolReference const& sym) const;

    IResolver* changeResolver(IResolver& resolver);
    Statement* changeStatement(Statement* statement);

    void rewrite(std::unique_ptr<Expression> expr);
    void rewrite(std::function<std::unique_ptr<Expression>(std::unique_ptr<Expression>&)> func);

    void resolveExpression(std::unique_ptr<Expression>& expression);
    void resolveExpressions(std::vector<std::unique_ptr<Expression>>::iterator left,
                            std::vector<std::unique_ptr<Expression>>::iterator right);
    void resolveExpressions(std::vector<std::unique_ptr<Expression>>& expressions);

    void resolveStatement(Statement& stmt);
    void resolveStatements(std::vector<Statement>::iterator left,
                           std::vector<Statement>::iterator right);
    void resolveStatements(std::vector<Statement>& stmts);

private:
    Module* myModule = nullptr;
    Diagnostics* myDiagnostics = nullptr;
    IResolver* myResolver = nullptr;
    Statement* myStatement = nullptr;
    std::unique_ptr<Expression> myRewrite;
    std::function<std::unique_ptr<Expression>(std::unique_ptr<Expression>&)> myLazyRewrite;
};

    } // namespace ast
} // namespace kyfoo
