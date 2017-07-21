#include <kyfoo/ast/Context.hpp>

#include <kyfoo/Diagnostics.hpp>
#include <kyfoo/ast/Module.hpp>
#include <kyfoo/ast/Scopes.hpp>

namespace kyfoo {
    namespace ast {

//
// ScopeResolver

ScopeResolver::ScopeResolver(DeclarationScope const& scope)
    : myScope(&scope)
{
}

ScopeResolver::ScopeResolver(ScopeResolver&& rhs)
    : myScope(rhs.myScope)
    , mySupplementarySymbols(std::move(rhs.mySupplementarySymbols))
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
    swap(mySupplementarySymbols, rhs.mySupplementarySymbols);
}

Module const& ScopeResolver::module() const
{
    return myScope->module();
}

LookupHit ScopeResolver::matchEquivalent(Diagnostics& dgn, SymbolReference const& symbol) const
{
    LookupHit hit = matchSupplementary(symbol);
    if ( hit )
        return hit;

    for ( auto scope = myScope; scope; scope = scope->parent() ) {
        if ( hit.append(scope->findEquivalent(dgn, symbol)) )
            return hit;

        if ( symbol.parameters().empty() )
            if ( auto decl = scope->declaration() )
                if ( auto s = decl->symbol().findVariable(symbol.name()) )
                    return std::move(hit.lookup(s));
    }

    for ( auto m : myScope->module().imports() )
        if ( hit.append(m->scope()->findEquivalent(dgn, symbol)) )
            return hit;

    return hit;
}

LookupHit ScopeResolver::matchValue(Diagnostics& dgn, SymbolReference const& symbol) const
{
    LookupHit hit = matchSupplementary(symbol);
    if ( hit )
        return hit;

    for ( auto scope = myScope; scope; scope = scope->parent() ) {
        if ( hit.append(scope->findValue(dgn, symbol)) )
            return hit;

        if ( symbol.parameters().empty() )
            if ( auto decl = scope->declaration() )
                if ( auto s = decl->symbol().findVariable(symbol.name()) )
                    return std::move(hit.lookup(s));
    }

    for ( auto m : myScope->module().imports() )
        if ( hit.append(m->scope()->findValue(dgn, symbol)) )
            return hit;

    return hit;
}

LookupHit ScopeResolver::matchProcedure(Diagnostics& dgn, SymbolReference const& procOverload) const
{
    LookupHit hit;
    for ( auto scope = myScope; scope; scope = scope->parent() )
        if ( hit.append(scope->findProcedureOverload(dgn, procOverload)) )
            return hit;
    
    for ( auto m : myScope->module().imports() )
        if ( hit.append(m->scope()->findProcedureOverload(dgn, procOverload)) )
            return hit;

    return hit;
}

void ScopeResolver::addSupplementarySymbol(Symbol const& sym)
{
    mySupplementarySymbols.push_back(&sym);
}

LookupHit ScopeResolver::matchSupplementary(SymbolReference const& symbol) const
{
    LookupHit hit;
    if ( symbol.parameters().empty() )
        for ( auto& s : mySupplementarySymbols )
            if ( auto symVar = s->findVariable(symbol.name()) )
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

//Module& Context::module()
//{
//    return myResovler->modile();
//}
//
//Module const& Context::module() const
//{
//    return myResolver->module();
//}
//
//LookupHit Context::matchEquivalent(Diagnostics& dgn, SymbolReference const& sym) const
//{
//    return myResolver->matchEquivalent(dgn, sym);
//}
//
//LookupHit Context::matchValue(Diagnostics& dgn, SymbolReference const& sym) const
//{
//    return myResolver->matchValue(dgn, sym);
//}
//
//LookupHit Context::matchProcedure(Diagnostics& dgn, SymbolReference const& sym) const
//{
//    return myResolver->matchProcedure(dgn, sym);
//}

AxiomsModule const& Context::axioms() const
{
    return module().axioms();
}

Module const& Context::module() const
{
    return myResolver->module();
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

LookupHit Context::matchValue(SymbolReference const& sym) const
{
    return myResolver->matchValue(*myDiagnostics, sym);
}

LookupHit Context::matchProcedure(SymbolReference const& sym) const
{
    return myResolver->matchProcedure(*myDiagnostics, sym);
}

IResolver* Context::changeResolver(IResolver& resolver)
{
    auto ret = myResolver;
    myResolver = &resolver;
    return ret;
}

void Context::rewrite(std::unique_ptr<Expression> expr)
{
    myRewrite = std::move(expr);
}

void Context::resolveExpression(std::unique_ptr<Expression>& expression)
{
    myRewrite.reset();
    expression->resolveSymbols(*this);
    while ( myRewrite ) {
        expression = std::move(myRewrite);
        expression->resolveSymbols(*this);
    }
}

void Context::resolveExpressions(std::vector<std::unique_ptr<Expression>>::iterator left,
                                 std::vector<std::unique_ptr<Expression>>::iterator right)
{
    myRewrite.reset();
    for ( auto i = left; i != right; ++i ) {
        (*i)->resolveSymbols(*this);
        while ( myRewrite ) {
            *i = std::move(std::move(myRewrite));
            (*i)->resolveSymbols(*this);
        }
    }
}

void Context::resolveExpressions(std::vector<std::unique_ptr<Expression>>& expressions)
{
    return resolveExpressions(begin(expressions), end(expressions));
}

    } // namespace ast
} // namespace kyfoo
