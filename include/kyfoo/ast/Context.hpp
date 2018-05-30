#pragma once

#include <functional>
#include <vector>

#include <kyfoo/Types.hpp>
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
class SymRes;

class Resolver
{
public:
    enum Options
    {
        None        = 0,
        Narrow      = 1 << 0,
        SkipImports = 1 << 1,
    };

    using options_t = Options;

public:
    explicit Resolver(DeclarationScope& scope, Options opts = None);
    explicit Resolver(DeclarationScope const& scope, Options opts = None);

    Resolver(Resolver&& rhs);
    Resolver& operator = (Resolver&& rhs);

    ~Resolver();

    void swap(Resolver& rhs);

    // Resolver
public:
    DeclarationScope const& scope() const;
    DeclarationScope& scope();

    LookupHit matchEquivalent(SymbolReference const& symbol) const;
    LookupHit matchOverload(Module& endModule,
                            Diagnostics& dgn,
                            SymbolReference const& symbol);

public:
    void addSupplementaryPrototype(PatternsPrototype& proto);
    LookupHit matchSupplementary(SymbolReference const& symbol) const;

private:
    DeclarationScope* myScope = nullptr;
    Options myOptions = None;
    std::vector<PatternsPrototype*> mySupplementaryPrototypes;
};

class Context
{
public:
    enum Options
    {
        DisableCacheTemplateInstantiations = 1 << 0,
    };

    using options_t = kyfoo::u32;

public:
    Context(Module& module, Diagnostics& dgn, Resolver& resolver, options_t options);
    Context(Module& module, Diagnostics& dgn, Resolver& resolver);
    ~Context();

public:
    AxiomsModule const& axioms() const;
    Module& module();
    Module const& module() const;

    Diagnostics& diagnostics();
    Diagnostics const& diagnostics() const;

    Resolver& resolver();
    Resolver const& resolver() const;

    Statement& statement();
    Statement const& statement() const;

public:
    Error& error(lexer::Token const& token);
    Error& error(Expression const& expr);
    Error& error(Declaration const& decl);
    uz errorCount() const;

    LookupHit matchOverload(SymbolReference const& sym) const;

    Resolver* changeResolver(Resolver& resolver);
    Statement* changeStatement(Statement* statement);

    SymRes rewrite(Box<Expression> expr);
    SymRes rewrite(std::function<Box<Expression>(Box<Expression>&)> func);

    SymRes resolveDeclaration(Declaration& declaration);

    SymRes resolveExpression(Expression& expression);
    SymRes resolveExpression(Box<Expression>& expression);
    SymRes resolveExpressions(std::vector<Box<Expression>>::iterator left,
                              std::vector<Box<Expression>>::iterator right);
    SymRes resolveExpressions(std::vector<Box<Expression>>& expressions);

    SymRes resolveStatement(Statement& stmt);
    SymRes resolveStatements(std::vector<Statement>::iterator left,
                             std::vector<Statement>::iterator right);
    SymRes resolveStatements(std::vector<Statement>& stmts);

    bool isTopLevel() const;

private:
    Module* myModule = nullptr;
    Diagnostics* myDiagnostics = nullptr;
    Resolver* myResolver = nullptr;
    options_t myOptions = 0;
    Statement* myStatement = nullptr;
    Box<Expression> myRewrite;
    std::function<Box<Expression>(Box<Expression>&)> myLazyRewrite;
    int myExpressionDepth = -1;
};

    } // namespace ast
} // namespace kyfoo
