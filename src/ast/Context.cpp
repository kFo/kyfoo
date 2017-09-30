#include <kyfoo/ast/Context.hpp>

#include <kyfoo/Diagnostics.hpp>
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

LookupHit ScopeResolver::matchEquivalent(Diagnostics& dgn, SymbolReference const& symbol) const
{
    LookupHit hit = matchSupplementary(symbol);
    if ( hit )
        return hit;

    for ( auto scope = myScope; scope; scope = scope->parent() ) {
        if ( hit.append(scope->findEquivalent(dgn, symbol)) )
            return hit;

        if ( symbol.pattern().empty() )
            if ( auto decl = scope->declaration() )
                if ( auto s = decl->symbol().prototype().findVariable(symbol.name()) )
                    return std::move(hit.lookup(s));
    }

    for ( auto m : myScope->module().imports() )
        if ( hit.append(m->scope()->findEquivalent(dgn, symbol)) )
            return hit;

    return hit;
}

LookupHit ScopeResolver::matchOverload(Diagnostics& dgn, SymbolReference const& symbol)
{
    LookupHit hit = matchSupplementary(symbol);

    auto appendTemplate = [&hit, &module=myScope->module()] {
        if ( hit.decl() && hit.decl()->symbol().prototypeParent() )
            module.appendTemplateInstance(hit.decl());
    };

    if ( hit ) {
        appendTemplate();
        return hit;
    }

    for ( auto scope = myScope; scope; scope = scope->parent() ) {
        if ( hit.append(scope->findOverload(dgn, symbol)) ) {
            appendTemplate();
            return hit;
        }

        if ( symbol.pattern().empty() ) {
            if ( auto decl = scope->declaration() ) {
                if ( auto s = decl->symbol().prototype().findVariable(symbol.name()) ) {
                    appendTemplate();
                    return std::move(hit.lookup(s));
                }
            }
        }
    }

    for ( auto m : myScope->module().imports() ) {
        if ( hit.append(m->scope()->findOverload(dgn, symbol)) ) {
            appendTemplate();
            return hit;
        }
    }

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

Context::Context(Diagnostics& dgn, IResolver& resolver)
    : myDiagnostics(&dgn)
    , myResolver(&resolver)
{
}

Context::~Context() = default;

AxiomsModule const& Context::axioms() const
{
    return module().axioms();
}

Module const& Context::module() const
{
    return myResolver->scope().module();
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
    return myDiagnostics->error(module(), token);
}

Error& Context::error(Expression const& expr)
{
    return myDiagnostics->error(module(), expr);
}

Error& Context::error(Declaration const& decl)
{
    return myDiagnostics->error(module(), decl.symbol().identifier());
}

std::size_t Context::errorCount() const
{
    return myDiagnostics->errorCount();
}

LookupHit Context::matchOverload(SymbolReference const& sym) const
{
    return myResolver->matchOverload(*myDiagnostics, sym);
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

void Context::rewrite(std::unique_ptr<Expression> expr)
{
    myRewrite = std::move(expr);
}

void Context::rewrite(std::function<std::unique_ptr<Expression>(std::unique_ptr<Expression>&)> func)
{
    myLazyRewrite = func;
}

void Context::resolveExpression(std::unique_ptr<Expression>& expression)
{
    myRewrite.reset();
    expression->resolveSymbols(*this);
    while ( myRewrite ) {
        auto c = std::move(expression->myConstraints);
        expression = std::move(myRewrite);
        expression->myConstraints = std::move(c);
        expression->resolveSymbols(*this);

        if ( myLazyRewrite ) {
            if ( myRewrite )
                throw std::runtime_error("cannot have both a rewrite and lazy rewrite");

            myRewrite = myLazyRewrite(expression);
            myLazyRewrite = nullptr;
        }
    }

    resolveExpressions(expression->myConstraints);
}

void Context::resolveExpressions(std::vector<std::unique_ptr<Expression>>::iterator left,
                                 std::vector<std::unique_ptr<Expression>>::iterator right)
{
    myRewrite.reset();
    for ( auto i = left; i != right; ++i ) {
        (*i)->resolveSymbols(*this);
        while ( myRewrite ) {
            auto c = std::move((*i)->myConstraints);
            *i = std::move(std::move(myRewrite));
            (*i)->myConstraints = std::move(c);
            (*i)->resolveSymbols(*this);
        }
        resolveExpressions((*i)->myConstraints);
    }
}

void Context::resolveExpressions(std::vector<std::unique_ptr<Expression>>& expressions)
{
    return resolveExpressions(begin(expressions), end(expressions));
}

    } // namespace ast
} // namespace kyfoo
