#pragma once

#include <functional>
#include <variant>

#include <kyfoo/Array.hpp>
#include <kyfoo/Diagnostics.hpp>
#include <kyfoo/Types.hpp>
#include <kyfoo/ast/Symbol.hpp>

namespace kyfoo {

    namespace lexer {
        class Token;
    }

    namespace ast {

class AxiomsModule;
class BasicBlock;
class Binder;
class Junction;
class Scope;
class Statement;
class Declaration;
class Expression;
class Lookup;
class Module;
class SymRes;
class Viability;

class Resolver
{
public:
    
    using Options = u32;
    enum Option : Options
    {
        None                  = 0,
        Narrow                = 1 << 0,
        SkipImports           = 1 << 1,
        NoImplicitConversions = 1 << 2,
    };

public:
    explicit Resolver(Scope& scope, Options opts = None);
    explicit Resolver(Scope const& scope, Options opts = None);

    ~Resolver();

    void swap(Resolver& rhs) noexcept;

    // Resolver
public:
    Scope const& scope() const;
    Scope& scope();

    Options options() const;

    Lookup matchEquivalent(SymbolReference const& symbol) const;
    Lookup matchOverload(Context& ctx, SymbolReference const& symbol);

public:
    void addSupplementaryPrototype(PatternsPrototype& proto);
    Lookup matchSupplementary(SymbolReference const& symbol) const;

private:
    Scope* myScope = nullptr;
    Options myOptions = None;
    ab<PatternsPrototype*> mySupplementaryPrototypes;
};

class ResolverReverter;

class Context
{
public:
    friend class ResolverReverter;

    using Options = u32;
    enum Option : Options
    {
        None                               = 0,
        DisableCacheTemplateInstantiations = 1 << 0,
    };

public:
    Context(Module& module, Diagnostics& dgn, Resolver& resolver, Options options);
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
    ReportProxy error(diag code, Report::Subject gen);
    uz errorCount() const;

    Lookup matchOverload(Scope const& scope,
                         Resolver::Options options,
                         SymbolReference const& sym);
    Lookup matchOverload(SymbolReference const& sym);
    Lookup matchOverloadUsingImplicitConversions(stringv name,
                                                 Slice<Box<Expression>> args);
    Lookup matchOverloadUsingImplicitConversions(Scope const& scope,
                                                 Resolver::Options options,
                                                 stringv name,
                                                 Slice<Box<Expression>> args);
    Lookup matchOverloadUsingImplicitConversions(Resolver& resolver,
                                                 stringv name,
                                                 Slice<Box<Expression>> args);

    void shimConversion(Box<Expression>& expr, Viability const& v);

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
    SymRes resolveExpressions(ab<Box<Expression>>::Iterator left,
                              ab<Box<Expression>>::Iterator right);
    SymRes resolveExpressions(ab<Box<Expression>>& exprs);

    SymRes resolveStatement(Statement& stmt);
    SymRes resolveStatements(ab<Statement>::Iterator left,
                             ab<Statement>::Iterator right);
    SymRes resolveStatements(ab<Statement>& stmts);

    SymRes resolveJunction(Junction& junc, BasicBlock& bb);

    bool isTopLevel() const;

    void appendInstantiatedDefinition(Scope& defn);
    ab<Scope*>&& takeInstantiatedDefinitions();

public:
    operator DiagnosticsContext();

protected:
    SymRes resolveSymbols(Expression& expr);
    SymRes resolveSymbols(Statement& stmt);
    Lookup trackForModule(Lookup hit);

private:
    Module* myModule = nullptr;
    Diagnostics* myDiagnostics = nullptr;
    Resolver* myResolver = nullptr;
    Options myOptions = None;
    Statement* myStatement = nullptr;
    Box<Expression> myRewrite;
    std::function<Box<Expression>(Box<Expression>&)> myLazyRewrite;
    int myExpressionDepth = -1;
    ab<Declaration*> myInstantiatedDeclarations;
    ab<Scope*> myInstantiatedDefinitions;
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
