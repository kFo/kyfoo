#include <kyfoo/ast/Context.hpp>

#include <kyfoo/Diagnostics.hpp>
#include <kyfoo/ast/ControlFlow.hpp>
#include <kyfoo/ast/Module.hpp>
#include <kyfoo/ast/Scopes.hpp>

namespace kyfoo {
    namespace ast {

//
// ScopeResolver

ScopeResolver::ScopeResolver(DeclarationScope& scope)
    : myScope(&scope)
{
}

ScopeResolver::ScopeResolver(DeclarationScope const& scope)
    : myScope(const_cast<DeclarationScope*>(&scope))
{
}

ScopeResolver::ScopeResolver(ScopeResolver&& rhs)
    : myScope(rhs.myScope)
    , mySupplementaryPrototypes(std::move(rhs.mySupplementaryPrototypes))
{
    rhs.myScope = nullptr;
}

ScopeResolver& ScopeResolver::operator = (ScopeResolver&& rhs)
{
    ScopeResolver(std::move(rhs)).swap(*this);
    return *this;
}

ScopeResolver::~ScopeResolver() = default;

void ScopeResolver::swap(ScopeResolver& rhs)
{
    using std::swap;
    swap(myScope, rhs.myScope);
    swap(mySupplementaryPrototypes, rhs.mySupplementaryPrototypes);
}

DeclarationScope const& ScopeResolver::scope() const
{
    return *myScope;
}

LookupHit ScopeResolver::matchEquivalent(SymbolReference const& symbol) const
{
    LookupHit hit = matchSupplementary(symbol);
    if ( hit )
        return hit;

    for ( auto scope = myScope; scope; scope = scope->parent() ) {
        if ( hit.append(scope->findEquivalent(symbol)) )
            return hit;

        if ( symbol.pattern().empty() )
            if ( auto decl = scope->declaration() )
                if ( auto s = decl->symbol().prototype().findVariable(symbol.name()) )
                    return std::move(hit.lookup(s));
    }

    for ( auto m : myScope->module().imports() )
        if ( hit.append(m->scope()->findEquivalent(symbol)) )
            return hit;

    return hit;
}

LookupHit ScopeResolver::matchOverload(Module& endModule, Diagnostics& dgn, SymbolReference const& symbol)
{
    LookupHit hit = matchSupplementary(symbol);

    if ( hit )
        return hit;

    for ( auto scope = myScope; scope; scope = scope->parent() ) {
        if ( hit.append(scope->findOverload(endModule, dgn, symbol)) )
            return hit;

        if ( symbol.pattern().empty() ) {
            if ( auto decl = scope->declaration() ) {
                if ( auto s = decl->symbol().prototype().findVariable(symbol.name()) )
                    return std::move(hit.lookup(s));

                if ( auto proc = decl->as<ProcedureDeclaration>() )
                    if ( auto p = proc->findParameter(symbol.name()) )
                        return std::move(hit.lookup(p));
            }
        }
    }

    for ( auto m : myScope->module().imports() )
        if ( hit.append(m->scope()->findOverload(endModule, dgn, symbol)) )
            return hit;

    return hit;
}

void ScopeResolver::addSupplementaryPrototype(PatternsPrototype& proto)
{
    mySupplementaryPrototypes.push_back(&proto);
}

LookupHit ScopeResolver::matchSupplementary(SymbolReference const& symbol) const
{
    LookupHit hit;
    if ( symbol.pattern().empty() )
        for ( auto& proto : mySupplementaryPrototypes )
            if ( auto symVar = proto->findVariable(symbol.name()) )
                return std::move(hit.lookup(symVar));

    return hit;
}

//
// Context

Context::Context(Module& module, Diagnostics& dgn, IResolver& resolver, options_t options)
    : myModule(&module)
    , myDiagnostics(&dgn)
    , myResolver(&resolver)
    , myOptions(options)
{
}

Context::Context(Module& module, Diagnostics& dgn, IResolver& resolver)
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

IResolver& Context::resolver()
{
    return *myResolver;
}

IResolver const& Context::resolver() const
{
    return *myResolver;
}

Statement& Context::statement()
{
    if ( !myStatement )
        throw std::runtime_error("no active statement in this context");

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

Error& Context::error(Declaration const& decl)
{
    return myDiagnostics->error(resolver().scope().module(), decl.symbol().token());
}

std::size_t Context::errorCount() const
{
    return myDiagnostics->errorCount();
}

LookupHit Context::matchOverload(SymbolReference const& sym) const
{
    auto hit = myResolver->matchOverload(*myModule, *myDiagnostics, sym);
    if ( !(myOptions & DisableCacheTemplateInstantiations) )
        if ( hit.decl() && hit.decl()->symbol().prototypeParent() )
            myModule->appendTemplateInstance(hit.decl());

    return hit;
}

IResolver* Context::changeResolver(IResolver& resolver)
{
    auto ret = myResolver;
    myResolver = &resolver;
    return ret;
}

Statement* Context::changeStatement(Statement* statement)
{
    auto ret = myStatement;
    myStatement = statement;
    return ret;
}

SymRes Context::rewrite(std::unique_ptr<Expression> expr)
{
    myRewrite = std::move(expr);
    return SymRes::Rewrite;
}

SymRes Context::rewrite(std::function<std::unique_ptr<Expression>(std::unique_ptr<Expression>&)> func)
{
    myLazyRewrite = func;
    return SymRes::Rewrite;
}

SymRes Context::resolveDeclaration(Declaration& declaration)
{
    ScopeResolver resolver(declaration.scope());
    auto oldResolver = changeResolver(resolver);
    auto ret = declaration.resolveSymbols(*this);
    if ( oldResolver )
        changeResolver(*oldResolver);
    return ret;
}

SymRes Context::resolveExpression(Expression& expression)
{
    auto originalResolver = myResolver;
    myRewrite.reset();
    ++myExpressionDepth;
    auto ret = expression.resolveSymbols(*this);
    --myExpressionDepth;
    if ( ret && !expression.type() )
        throw std::runtime_error("successful elaboration did not type an expression");

    if ( ret == SymRes::Rewrite || myRewrite || myLazyRewrite )
        throw std::runtime_error("expression cannot be rewritten in this context");

    myResolver = originalResolver;

    ret |= resolveExpressions(expression.myConstraints);
    return ret;
}

SymRes Context::resolveExpression(std::unique_ptr<Expression>& expression)
{
    auto originalResolver = myResolver;
    myRewrite.reset();
    ++myExpressionDepth;
    auto ret = expression->resolveSymbols(*this);
    --myExpressionDepth;
    if ( ret && !expression->type() )
        throw std::runtime_error("successful elaboration did not type an expression");

    while ( myRewrite || myLazyRewrite ) {
        if ( ret != SymRes::Rewrite )
            throw std::runtime_error("inconsistent rewrite request");

        auto c = std::move(expression->myConstraints);

        if ( myLazyRewrite ) {
            if ( myRewrite )
                throw std::runtime_error("cannot have both a rewrite and lazy rewrite");

            myRewrite = myLazyRewrite(expression);
            myLazyRewrite = nullptr;
        }

        expression = std::move(myRewrite);
        expression->addConstraints(std::move(c));
        myResolver = originalResolver;
        ret = expression->resolveSymbols(*this);
    }

    myResolver = originalResolver;

    if ( ret == SymRes::Rewrite )
        throw std::runtime_error("unhandled rewrite request");

    ret |= resolveExpressions(expression->myConstraints);
    return ret;
}

SymRes Context::resolveExpressions(std::vector<std::unique_ptr<Expression>>::iterator left,
                                   std::vector<std::unique_ptr<Expression>>::iterator right)
{
    myRewrite.reset();
    SymRes ret;
    for ( ; left != right; ++left )
        ret |= resolveExpression(*left);

    return ret;
}

SymRes Context::resolveExpressions(std::vector<std::unique_ptr<Expression>>& expressions)
{
    return resolveExpressions(begin(expressions), end(expressions));
}

SymRes Context::resolveStatement(Statement& stmt)
{
    changeStatement(&stmt);
    auto ret = stmt.resolveSymbols(*this);
    changeStatement(nullptr);

    return ret;
}

SymRes Context::resolveStatements(std::vector<Statement>::iterator left,
                                  std::vector<Statement>::iterator right)
{
    SymRes ret = SymRes::Success;
    for ( ; left != right; ++left ) {
        changeStatement(&*left);
        ret |= left->resolveSymbols(*this);
    }
    changeStatement(nullptr);

    return ret;
}

SymRes Context::resolveStatements(std::vector<Statement>& stmts)
{
    return resolveStatements(begin(stmts), end(stmts));
}

bool Context::isTopLevel() const
{
    return myExpressionDepth == 0;
}

    } // namespace ast
} // namespace kyfoo
