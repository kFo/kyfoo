#include <kyfoo/ast/Semantics.hpp>

#include <kyfoo/ast/Declarations.hpp>
#include <kyfoo/ast/Scopes.hpp>

namespace kyfoo {
    namespace ast {

//
// Resolver

Resolver::Resolver(DeclarationScope* scope)
    : myScope(scope)
{
}

Declaration* Resolver::inScope(std::string const& symbol)
{
    return myScope->find(symbol);
}

Declaration* Resolver::lookup(std::string const& symbol)
{
    for ( auto scope = myScope; scope; scope = scope->parent() ) {
        if ( auto d = scope->find(symbol) )
            return d;
    }

    return nullptr;
}

    } // namespace ast
} // namespace kyfoo
