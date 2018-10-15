#pragma once

#include <functional>
#include <vector>

#include <kyfoo/Diagnostics.hpp>
#include <kyfoo/Types.hpp>
#include <kyfoo/ast/Symbol.hpp>

namespace kyfoo {

    namespace lexer {
        class Token;
    }

    namespace ast {

class AxiomsModule;
class Scope;
class Statement;
class Declaration;
class Expression;
class Lookup;
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
    explicit Resolver(Scope& scope, Options opts = None);
    explicit Resolver(Scope const& scope, Options opts = None);

    ~Resolver();

    void swap(Resolver& rhs) noexcept;

    // Resolver
public:
    Scope const& scope() const;
    Scope& scope();

    Lookup matchEquivalent(SymbolReference const& symbol) const;
    Lookup matchOverload(Context& ctx, SymbolReference const& symbol);

public:
    void addSupplementaryPrototype(PatternsPrototype& proto);
    Lookup matchSupplementary(SymbolReference const& symbol) const;

private:
    Scope* myScope = nullptr;
    Options myOptions = None;
    std::vector<PatternsPrototype*> mySupplementaryPrototypes;
};

class Context
{
public:
    friend class ResolverReverter;

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
    Error& error(Statement const& stmt);
    Error& error(Junction const& junc);
    Error& error(Declaration const& decl);
    uz errorCount() const;

    Lookup matchOverload(Scope const& scope,
                         Resolver::Options options,
                         SymbolReference const& sym);
    Lookup matchOverload(SymbolReference const& sym);
    Lookup matchOverloadUsingImplicitConversions(Scope const& scope,
                                                 Resolver::Options options,
                                                 stringv name,
                                                 Slice<Box<Expression>> args);
    Lookup matchOverloadUsingImplicitConversions(stringv name,
                                                 Slice<Box<Expression>> args);

    Resolver* changeResolver(Resolver& resolver);
    ResolverReverter pushResolver(Resolver& resolver);
    Statement* changeStatement(Statement* stmt);

    SymRes rewrite(Box<Expression> expr);
    SymRes rewrite(std::function<Box<Expression>(Box<Expression>&)> func);

    SymRes resolveDeclaration(Declaration& decl);
    SymRes resolveScopeDeclarations(Scope& scope);
    SymRes resolveScopeDefinitions(Scope& scope);
    SymRes resolveScope(Scope& scope);
    SymRes resolveScopeAttributes(Scope& scope);

    SymRes resolveExpression(Expression& expr);
    SymRes resolveExpression(Box<Expression>& expr);
    SymRes resolveExpressions(std::vector<Box<Expression>>::iterator left,
                              std::vector<Box<Expression>>::iterator right);
    SymRes resolveExpressions(std::vector<Box<Expression>>& exprs);

    SymRes resolveStatement(Statement& stmt);
    SymRes resolveStatements(std::vector<Statement>::iterator left,
                             std::vector<Statement>::iterator right);
    SymRes resolveStatements(std::vector<Statement>& stmts);

    bool isTopLevel() const;

    void appendInstantiatedDefinition(Scope& defn);
    std::vector<Scope*>&& takeInstantiatedDefinitions();

public:
    operator DiagnosticsContext();

protected:
    SymRes resolveSymbols(Expression& expr);
    SymRes resolveSymbols(Statement& stmt);
    Lookup trackForModule(Lookup&& hit);

private:
    Module* myModule = nullptr;
    Diagnostics* myDiagnostics = nullptr;
    Resolver* myResolver = nullptr;
    options_t myOptions = 0;
    Statement* myStatement = nullptr;
    Box<Expression> myRewrite;
    std::function<Box<Expression>(Box<Expression>&)> myLazyRewrite;
    int myExpressionDepth = -1;
    std::vector<Declaration*> myInstantiatedDeclarations;
    std::vector<Scope*> myInstantiatedDefinitions;
};

class [[nodiscard]] ResolverReverter
{
public:
    ResolverReverter(Context& ctx, Resolver* old)
        : myCtx(ctx)
        , myOld(old)
    {
    }

    ~ResolverReverter()
    {
        if ( myOld )
            myCtx.changeResolver(*myOld);
    }

public:
    Resolver* resolver()
    {
        return myOld;
    }

private:
    Context& myCtx;
    Resolver* myOld = nullptr;
};

    } // namespace ast
} // namespace kyfoo
