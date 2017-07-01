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

LookupHit ScopeResolver::matchEquivalent(SymbolReference const& symbol) const
{
    LookupHit hit = matchSupplementary(symbol);
    if ( hit )
        return hit;

    for ( auto scope = myScope; scope; scope = scope->parent() ) {
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

    for ( auto m : module()->imports() )
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
    
    for ( auto m : module()->imports() )
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

LookupHit SymbolVariableCreatorFailoverResolver::matchEquivalent(SymbolReference const& symbol) const
{
    if ( auto hit = myResolver->matchEquivalent(symbol) )
        return hit;

    if ( symbol.parameters().empty() )
        return LookupHit(mySymbol->createVariable(symbol.name()));

    return LookupHit();
}

LookupHit SymbolVariableCreatorFailoverResolver::matchValue(Diagnostics& dgn, SymbolReference const& symbol) const
{
    if ( auto hit = myResolver->matchValue(dgn, symbol) )
        return hit;

    if ( symbol.parameters().empty() )
        return LookupHit(mySymbol->createVariable(symbol.name()));

    return LookupHit();
}

LookupHit SymbolVariableCreatorFailoverResolver::matchProcedure(Diagnostics& dgn, SymbolReference const& procOverload) const
{
    return myResolver->matchProcedure(dgn, procOverload);
}

//
// Context

Context::Context(Diagnostics& dgn, IResolver& resolver)
    : myDiagnostics(&dgn)
    , myResolver(&resolver)
{
}

Context::~Context() = default;

Module const* Context::module() const
{
    return myResolver->module();
}

LookupHit Context::matchEquivalent(SymbolReference const& sym) const
{
    return myResolver->matchEquivalent(sym);
}

LookupHit Context::matchValue(Diagnostics& dgn, SymbolReference const& sym) const
{
    return myResolver->matchValue(dgn, sym);
}

LookupHit Context::matchProcedure(Diagnostics& dgn, SymbolReference const& sym) const
{
    return myResolver->matchProcedure(dgn, sym);
}

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

LookupHit Context::matchValue(SymbolReference const& sym) const
{
    return myResolver->matchValue(*myDiagnostics, sym);
}

LookupHit Context::matchProcedure(SymbolReference const& sym) const
{
    return myResolver->matchProcedure(*myDiagnostics, sym);
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
