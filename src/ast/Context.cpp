#include <kyfoo/ast/Context.hpp>

#include <kyfoo/Diagnostics.hpp>
#include <kyfoo/ast/Module.hpp>
#include <kyfoo/ast/Scopes.hpp>

namespace kyfoo {
    namespace ast {

//
// ScopeResolver

ScopeResolver::ScopeResolver(DeclarationScope* scope)
    : myScope(scope)
{
    if ( !scope )
        throw std::runtime_error("scope resolver scope cannot be null");
}

Module const* ScopeResolver::module() const
{
    return myScope->module();
}

LookupHit ScopeResolver::inScope(SymbolReference const& symbol) const
{
    auto hit = myScope->findEquivalent(symbol);
    if ( hit )
        return hit;

    if ( symbol.parameters().empty() )
        for ( auto& s : mySupplementarySymbols )
            if ( auto symVar = s->findVariable(symbol.name()) )
                return std::move(hit.lookup(symVar));

    return hit;
}

LookupHit ScopeResolver::matchEquivalent(SymbolReference const& symbol) const
{
    LookupHit hit;
    if ( hit = inScope(symbol) )
        return hit;

    for ( auto scope = myScope->parent(); scope; scope = scope->parent() ) {
        if ( hit.append(scope->findEquivalent(symbol)) )
            return hit;

        if ( symbol.parameters().empty() )
            if ( auto decl = scope->declaration() )
                if ( auto s = decl->symbol().findVariable(symbol.name()) )
                    return std::move(hit.lookup(s));
    }

    for ( auto m : module()->imports() )
        if ( hit.append(m->scope()->findEquivalent(symbol)) )
            return hit;

    return hit;
}

LookupHit ScopeResolver::matchValue(SymbolReference const& symbol) const
{
    LookupHit hit;
    for ( auto scope = myScope; scope; scope = scope->parent() ) {
        if ( hit.append(scope->findValue(symbol)) )
            return hit;

        if ( symbol.parameters().empty() )
            if ( auto decl = scope->declaration() )
                if ( auto s = decl->symbol().findVariable(symbol.name()) )
                    return std::move(hit.lookup(s));
    }

    for ( auto m : module()->imports() )
        if ( hit.append(m->scope()->findValue(symbol)) )
            return hit;

    return hit;
}

LookupHit ScopeResolver::matchProcedure(SymbolReference const& procOverload) const
{
    LookupHit hit;
    for ( auto scope = myScope; scope; scope = scope->parent() ) {
        if ( hit.append(scope->findProcedureOverload(procOverload)) )
            return hit;
    }

    for ( auto m : module()->imports() )
        if ( hit.append(m->scope()->findProcedureOverload(procOverload)) )
            return hit;

    return hit;
}

void ScopeResolver::addSupplementarySymbol(Symbol const& sym)
{
    mySupplementarySymbols.push_back(&sym);
}

//
// SymbolVariableCreatorFailoverResolver

SymbolVariableCreatorFailoverResolver::SymbolVariableCreatorFailoverResolver(IResolver& resolver, Symbol& symbol)
    : myResolver(&resolver)
    , mySymbol(&symbol)
{
}

SymbolVariableCreatorFailoverResolver::~SymbolVariableCreatorFailoverResolver() = default;

Module const* SymbolVariableCreatorFailoverResolver::module() const
{
    return myResolver->module();
}

LookupHit SymbolVariableCreatorFailoverResolver::inScope(SymbolReference const& symbol) const
{
    return myResolver->inScope(symbol);
}

LookupHit SymbolVariableCreatorFailoverResolver::matchEquivalent(SymbolReference const& symbol) const
{
    if ( auto hit = myResolver->matchEquivalent(symbol) )
        return hit;

    if ( symbol.parameters().empty() )
        return LookupHit(mySymbol->createVariable(symbol.name()));

    return LookupHit();
}

LookupHit SymbolVariableCreatorFailoverResolver::matchValue(SymbolReference const& symbol) const
{
    if ( auto hit = myResolver->matchValue(symbol) )
        return hit;

    if ( symbol.parameters().empty() )
        return LookupHit(mySymbol->createVariable(symbol.name()));

    return LookupHit();
}

LookupHit SymbolVariableCreatorFailoverResolver::matchProcedure(SymbolReference const& procOverload) const
{
    return myResolver->matchProcedure(procOverload);
}

//
// Context

Context::Context(Diagnostics& dgn, IResolver& resolver)
    : myDiagnostics(&dgn)
    , myResolver(&resolver)
{
}

Context::~Context() = default;

Error& Context::error(lexer::Token const& token)
{
    return myDiagnostics->error(module(), token);
}

Error& Context::error(Expression const& expr)
{
    return myDiagnostics->error(module(), expr);
}

std::size_t Context::errorCount() const
{
    return myDiagnostics->errorCount();
}

Module const* Context::module() const
{
    return myResolver->module();
}

LookupHit Context::inScope(SymbolReference const& symbol) const
{
    return myResolver->inScope(symbol);
}

LookupHit Context::matchEquivalent(SymbolReference const& sym) const
{
    return myResolver->matchEquivalent(sym);
}

LookupHit Context::matchValue(SymbolReference const& sym) const
{
    return myResolver->matchValue(sym);
}

LookupHit Context::matchProcedure(SymbolReference const& sym) const
{
    return myResolver->matchProcedure(sym);
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

void Context::resolveExpressions(std::vector<std::unique_ptr<Expression>>& expressions)
{
    myRewrite.reset();
    for ( auto i = begin(expressions); i != end(expressions); ++i ) {
        (*i)->resolveSymbols(*this);
        while ( myRewrite ) {
            *i = std::move(std::move(myRewrite));
            (*i)->resolveSymbols(*this);
        }
    }
}

    } // namespace ast
} // namespace kyfoo
