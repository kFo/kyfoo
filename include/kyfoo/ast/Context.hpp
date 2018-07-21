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
class DeclarationScope;
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
        None                    = 0,
        Narrow                  = 1 << 0,
        SkipImports             = 1 << 1,
    };

    using options_t = Options;

public:
    explicit Resolver(DeclarationScope& scope, Options opts = None);
    explicit Resolver(DeclarationScope const& scope, Options opts = None);

    Resolver(Resolver&& rhs);
    Resolver& operator = (Resolver&& rhs);

    ~Resolver();

    void swap(Resolver& rhs) noexcept;

    // Resolver
public:
    DeclarationScope const& scope() const;
    DeclarationScope& scope();

    Lookup matchEquivalent(SymbolReference const& symbol) const;
    Lookup matchOverload(Context& ctx, SymbolReference const& symbol);

public:
    void addSupplementaryPrototype(PatternsPrototype& proto);
    Lookup matchSupplementary(SymbolReference const& symbol) const;

private:
    DeclarationScope* myScope = nullptr;
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
    Error& error(Declaration const& decl);
    uz errorCount() const;

    Lookup matchOverload(DeclarationScope const& scope,
                         Resolver::Options options,
                         SymbolReference const& sym);
    Lookup matchOverload(SymbolReference const& sym);
    Lookup matchOverloadUsingImplicitConversions(DeclarationScope const& scope,
                                                 Resolver::Options options,
                                                 std::string_view name,
                                                 Slice<Box<Expression>> args);
    Lookup matchOverloadUsingImplicitConversions(std::string_view name,
                                                 Slice<Box<Expression>> args);

    Resolver* changeResolver(Resolver& resolver);
    ResolverReverter pushResolver(Resolver& resolver);
    Statement* changeStatement(Statement* statement);

    SymRes rewrite(Box<Expression> expr);
    SymRes rewrite(std::function<Box<Expression>(Box<Expression>&)> func);

    SymRes resolveDeclaration(Declaration& declaration);
    SymRes resolveScopeDeclarations(DeclarationScope& scope);
    SymRes resolveScopeDefinitions(DeclarationScope& scope);
    SymRes resolveScope(DeclarationScope& scope);
    SymRes resolveScopeAttributes(DeclarationScope& scope);

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

    void appendInstantiatedDefinition(DeclarationScope& defn);
    std::vector<DeclarationScope*>&& takeInstantiatedDefinitions();

public:
    operator DiagnosticsContext();

protected:
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
    std::vector<DeclarationScope*> myInstantiatedDefinitions;
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
