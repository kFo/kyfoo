#pragma once

#include <string>

namespace kyfoo {
    namespace ast {

class Module;
class Declaration;
class DeclarationScope;

class Resolver
{
public:
    explicit Resolver(DeclarationScope* scope);

public:
    Module* module();
    Declaration* inScope(std::string const& symbol);
    Declaration* lookup(std::string const& symbol);

private:
    DeclarationScope* myScope = nullptr;
};

    } // namespace ast
} // namespace kyfoo
