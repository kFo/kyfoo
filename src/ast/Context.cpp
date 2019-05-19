#include <kyfoo/ast/Context.hpp>

#include <kyfoo/Diagnostics.hpp>
#include <kyfoo/Utilities.hpp>
#include <kyfoo/ast/ControlFlow.hpp>
#include <kyfoo/ast/Fabrication.hpp>
#include <kyfoo/ast/Module.hpp>
#include <kyfoo/ast/Overloading.hpp>
#include <kyfoo/ast/Scopes.hpp>

namespace kyfoo::ast {

//
// Resolver

Resolver::Resolver(Scope& scope, options_t opts)
    : myScope(&scope)
    , myOptions(opts)
{
}

Resolver::Resolver(Scope const& scope, options_t opts)
    : myScope(const_cast<Scope*>(&scope))
    , myOptions(opts)
{
}

Resolver::~Resolver() = default;

void Resolver::swap(Resolver& rhs) noexcept
{
    using kyfoo::swap;
    swap(myScope, rhs.myScope);
    swap(myOptions, rhs.myOptions);
    swap(mySupplementaryPrototypes, rhs.mySupplementaryPrototypes);
}

Scope const& Resolver::scope() const
{
    return *myScope;
}

Scope& Resolver::scope()
{
    return *myScope;
}

Resolver::options_t Resolver::options() const
{
    return myOptions;
}

Lookup Resolver::matchEquivalent(SymbolReference const& symbol) const
{
    Lookup hit = matchSupplementary(symbol);
    if ( hit )
        return hit;

    for ( auto scope = myScope; scope; scope = scope->parent() ) {
        if ( hit.append(scope->findEquivalent(symbol)) )
            return hit;

        if ( !symbol.pattern() )
            if ( auto decl = scope->declaration() )
                if ( auto s = decl->symbol().prototype().findVariable(symbol.name()) )
                    return std::move(hit.resolveTo(*s));

        if ( myOptions & Narrow )
            break;
    }

    if ( !(myOptions & SkipImports) ) {
        for ( auto m : myScope->module().imports() )
            if ( hit.append(m->scope()->findEquivalent(symbol)) )
                return hit;
    }

    return hit;
}

Lookup Resolver::matchOverload(Context& ctx, SymbolReference const& symbol)
{
    Lookup hit = matchSupplementary(symbol);

    if ( hit )
        return hit;

    for ( auto scope = myScope; scope; scope = scope->parent() ) {
        if ( hit.append(scope->findOverload(ctx, symbol)) )
            return hit;

        if ( !symbol.pattern() ) {
            if ( auto decl = scope->declaration() ) {
                if ( auto s = decl->symbol().prototype().findVariable(symbol.name()) )
                    return std::move(hit.resolveTo(*s));

                if ( auto proc = decl->as<ProcedureDeclaration>() )
                    if ( auto p = proc->findParameter(symbol.name()) )
                        return std::move(hit.resolveTo(*p));
            }
        }

        if ( myOptions & Narrow )
            break;
    }

    if ( !(myOptions & SkipImports) ) {
        for ( auto m : myScope->module().imports() )
            if ( hit.append(m->scope()->findOverload(ctx, symbol)) )
                return hit;
    }

    return hit;
}

void Resolver::addSupplementaryPrototype(PatternsPrototype& proto)
{
    mySupplementaryPrototypes.push_back(&proto);
}

Lookup Resolver::matchSupplementary(SymbolReference const& symbol) const
{
    Lookup hit(symbol);
    if ( !symbol.pattern() )
        for ( auto& proto : mySupplementaryPrototypes )
            if ( auto symVar = proto->findVariable(symbol.name()) )
                return std::move(hit.resolveTo(*symVar));

    return hit;
}

//
// Context

Context::Context(Module& module, Diagnostics& dgn, Resolver& resolver, options_t options)
    : myModule(&module)
    , myDiagnostics(&dgn)
    , myResolver(&resolver)
    , myOptions(options)
{
}

Context::Context(Module& module, Diagnostics& dgn, Resolver& resolver)
    : Context(module, dgn, resolver, 0)
{
}

Context::~Context() = default;

AxiomsModule const& Context::axioms() const
{
    return module().axioms();
}

Module& Context::module()
{
    return *myModule;
}

Module const& Context::module() const
{
    return *myModule;
}

Diagnostics& Context::diagnostics()
{
    return *myDiagnostics;
}

Diagnostics const& Context::diagnostics() const
{
    return *myDiagnostics;
}

Resolver& Context::resolver()
{
    return *myResolver;
}

Resolver const& Context::resolver() const
{
    return *myResolver;
}

Statement& Context::statement()
{
    ENFORCE(myStatement, "no active statement in this context");

    return *myStatement;
}

Statement const& Context::statement() const
{
    return const_cast<Statement const&>(const_cast<Context*>(this)->statement());
}

Error& Context::error(lexer::Token const& token)
{
    return myDiagnostics->error(resolver().scope().module(), token);
}

Error& Context::error(Expression const& expr)
{
    return myDiagnostics->error(resolver().scope().module(), expr);
}

Error& Context::error(Statement const& stmt)
{
    return myDiagnostics->error(resolver().scope().module(), stmt);
}

Error& Context::error(Junction const& junc)
{
    return myDiagnostics->error(resolver().scope().module(), junc);
}

Error& Context::error(Declaration const& decl)
{
    return myDiagnostics->error(resolver().scope().module(), decl);
}

Error& Context::error(Error::context_t gen)
{
    return std::visit([this](auto const* e) -> Error& { return error(*e); }, gen);
}

uz Context::errorCount() const
{
    return myDiagnostics->errorCount();
}

Lookup Context::matchOverload(Scope const& scope,
                              Resolver::options_t options,
                              SymbolReference const& sym)
{
    Resolver resolver(scope, options);
    REVERT = pushResolver(resolver);
    return matchOverload(sym);
}

Lookup Context::matchOverload(SymbolReference const& sym)
{
    return trackForModule(myResolver->matchOverload(*this, sym));
}

Lookup Context::matchOverloadUsingImplicitConversions(stringv name,
                                                      Slice<Box<Expression>> args)
{
    return matchOverloadUsingImplicitConversions(*myResolver, name, args);
}

Lookup Context::matchOverloadUsingImplicitConversions(Scope const& scope,
                                                      Resolver::options_t options,
                                                      stringv name,
                                                      Slice<Box<Expression>> args)
{
    Resolver resolver(scope, options);
    return matchOverloadUsingImplicitConversions(resolver, name, args);
}

Lookup Context::matchOverloadUsingImplicitConversions(Resolver& resolver,
                                                      stringv name,
                                                      Slice<Box<Expression>> args)
{
    auto hit = resolver.matchOverload(*this, SymbolReference(name, args));
    if ( !hit )
        return hit;

    if ( hit.viable().result() == ViableSet::NeedsConversion ) {
        auto const& v = hit.viable().best().viability();
        for ( uz i = 0, arity = args.card(); i != arity; ++i )
            shimConversion(args[i], v[i]);

        if ( auto decl = hit.viable().best().instantiate(*this) )
            hit.resolveTo(*decl);
    }

    return trackForModule(std::move(hit));
}

void Context::shimConversion(Box<Expression>& expr, Viability const& v)
{
    if ( auto proc = v.conversion() ) {
        expr = createApply(createIdentifier(*proc), std::move(expr));
        ENFORCE(resolveExpression(expr), "invalid implicit conversion");
    }
}

Resolver* Context::changeResolver(Resolver& resolver)
{
    auto ret = myResolver;
    myResolver = &resolver;
    return ret;
}

ResolverReverter Context::pushResolver(Resolver& resolver)
{
    return { *this, changeResolver(resolver) };
}

Statement* Context::changeStatement(Statement* stmt)
{
    auto ret = myStatement;
    myStatement = stmt;
    return ret;
}

SymRes Context::rewrite(Box<Expression> expr)
{
    myRewrite = std::move(expr);
    return SymRes::Rewrite;
}

SymRes Context::rewrite(std::function<Box<Expression>(Box<Expression>&)> func)
{
    myLazyRewrite = func;
    return SymRes::Rewrite;
}

SymRes Context::resolveDeclaration(Declaration& decl)
{
    Resolver resolver(decl.scope());
    REVERT = pushResolver(resolver);
    switch (decl.kind()) {
#define X(a,b,c) case DeclKind::a: return static_cast<c&>(decl).resolveSymbols(*this);
        DECLARATION_KINDS(X)
#undef X
    }

    ENFORCEU("unhandled declaration");
}

SymRes Context::resolveScopeDeclarations(Scope& scope)
{
    Resolver resolver(scope);
    REVERT = pushResolver(resolver);
    SymRes ret;
    switch (scope.kind()) {
#define X(a,b) case Scope::Kind::a: ret = static_cast<b&>(scope).resolveDeclarations(*this); break;
    SCOPE_KINDS(X)
#undef X
    default:
        ENFORCEU("unhandled scope");
    }

    ENFORCE(myInstantiatedDeclarations.empty(), "instantiated declarations not elaborated");
    return ret;
}

SymRes Context::resolveScopeDefinitions(Scope& scope)
{
    Resolver resolver(scope);
    REVERT = pushResolver(resolver);
    SymRes ret;
    switch (scope.kind()) {
#define X(a,b) case Scope::Kind::a: ret = static_cast<b&>(scope).resolveDefinitions(*this); break;
    SCOPE_KINDS(X)
#undef X
    default:
        ENFORCEU("unhandled scope");
    }
    
    // Resolve any accrued definitions
    for ( auto defns = takeInstantiatedDefinitions(); !defns.empty(); defns = takeInstantiatedDefinitions() )
        for ( auto d : defns )
            if ( d->declaration()->symbol().prototype().isConcrete() )
                ret |= resolveScopeDefinitions(*d);

    return ret;
}

SymRes Context::resolveScope(Scope& scope)
{
    auto ret = resolveScopeDeclarations(scope);
    ret |= resolveScopeDefinitions(scope);
    return ret;
}

SymRes Context::resolveScopeAttributes(Scope& scope)
{
    Resolver resolver(scope);
    REVERT = pushResolver(resolver);
    return scope.resolveAttributes(*this);
}

SymRes Context::resolveExpression(Expression& expr)
{
    auto originalResolver = myResolver;
    myRewrite.reset();
    ++myExpressionDepth;
    auto ret = resolveSymbols(expr);
    --myExpressionDepth;
    if ( ret )
        ENFORCE(expr.type(), "successful elaboration did not type an expression");

    ENFORCE(ret != SymRes::Rewrite && !myRewrite && !myLazyRewrite,
            "expression cannot be rewritten in this context");

    myResolver = originalResolver;

    ret |= resolveExpressions(expr.myConstraints);
    return ret;
}

SymRes Context::resolveExpression(Box<Expression>& expr)
{
    auto originalResolver = myResolver;
    myRewrite.reset();
    ++myExpressionDepth;
    auto ret = resolveSymbols(*expr);
    --myExpressionDepth;
    if ( ret )
        ENFORCE(expr->type(), "successful elaboration did not type an expression");

    while ( myRewrite || myLazyRewrite ) {
        ENFORCE(ret == SymRes::Rewrite, "inconsistent rewrite request");

        auto c = std::move(expr->myConstraints);

        if ( myLazyRewrite ) {
            ENFORCE(!myRewrite, "cannot have both a rewrite and lazy rewrite");

            myRewrite = myLazyRewrite(expr);
            myLazyRewrite = nullptr;
        }

        if ( expr && myDiagnostics->errorCount() )
            myDiagnostics->bunkExpression(std::move(expr));

        expr = std::move(myRewrite);
        expr->addConstraints(std::move(c));
        myResolver = originalResolver;
        ret = resolveSymbols(*expr);
    }

    myResolver = originalResolver;

    ENFORCE(ret != SymRes::Rewrite, "unhandled rewrite request");

    ret |= resolveExpressions(expr->myConstraints);
    return ret;
}

SymRes Context::resolveExpressions(std::vector<Box<Expression>>::iterator left,
                                   std::vector<Box<Expression>>::iterator right)
{
    myRewrite.reset();
    SymRes ret;
    for ( ; left != right; ++left )
        ret |= resolveExpression(*left);

    return ret;
}

SymRes Context::resolveExpressions(std::vector<Box<Expression>>& exprs)
{
    return resolveExpressions(begin(exprs), end(exprs));
}

SymRes Context::resolveStatement(Statement& stmt)
{
    changeStatement(&stmt);
    SymRes ret = resolveSymbols(stmt);
    changeStatement(nullptr);

    return ret;
}

SymRes Context::resolveStatements(std::vector<Statement>::iterator left,
                                  std::vector<Statement>::iterator right)
{
    SymRes ret = SymRes::Success;
    for ( ; left != right; ++left ) {
        changeStatement(&*left);
        ret |= resolveSymbols(*left);
    }
    changeStatement(nullptr);

    return ret;
}

SymRes Context::resolveStatements(std::vector<Statement>& stmts)
{
    return resolveStatements(begin(stmts), end(stmts));
}

SymRes Context::resolveJunction(Junction& junc, BasicBlock& bb)
{
    switch ( junc.kind() ) {
#define X(a,b) case Junction::Kind::a: return static_cast<b&>(junc).resolveSymbols(*this, bb);
        JUNCTION_KINDS(X)
#undef X
    }

    ENFORCEU("unhandled junction");
}

bool Context::isTopLevel() const
{
    return myExpressionDepth == 0;
}

void Context::appendInstantiatedDefinition(Scope& defn)
{
    myInstantiatedDefinitions.push_back(&defn);
}

std::vector<Scope*>&& Context::takeInstantiatedDefinitions()
{
    return std::move(myInstantiatedDefinitions);
}

Context::operator DiagnosticsContext()
{
    return { *myDiagnostics, *myModule };
}

SymRes Context::resolveSymbols(Expression& expr)
{
    switch (expr.kind()) {
#define X(a,b) case Expression::Kind::a: return static_cast<b&>(expr).resolveSymbols(*this);
    EXPRESSION_KINDS(X)
#undef X
    }

    ENFORCEU("unhandled expression");
}

SymRes Context::resolveSymbols(Statement& stmt)
{
    switch ( stmt.kind() ) {
#define X(a,b) case Statement::Kind::a: return static_cast<b&>(stmt).resolveSymbols(*this);
        STATEMENT_KINDS(X)
#undef X
    }

    ENFORCEU("unhandled statement");
}

Lookup Context::trackForModule(Lookup&& hit)
{
    if ( !(myOptions & DisableCacheTemplateInstantiations) ) {
        auto decl = hit.single();
        if ( decl && decl->symbol().prototypeParent() && hasSubstitutions(decl->symbol()) )
            myModule->appendTemplateInstance(decl);
    }

    return std::move(hit);
}

} // namespace kyfoo::ast
