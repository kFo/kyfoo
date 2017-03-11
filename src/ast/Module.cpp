#include <kyfoo/ast/Module.hpp>

#include <cassert>

#include <kyfoo/Error.hpp>

#include <kyfoo/lexer/Scanner.hpp>
#include <kyfoo/lexer/Token.hpp>

#include <kyfoo/ast/Declarations.hpp>
#include <kyfoo/ast/Scopes.hpp>

namespace kyfoo {
    namespace ast {

Module::Module(const char* name,
               std::unique_ptr<DeclarationScope> scope)
    : myName(name)
    , myScope(std::move(scope))
{
}

Module::~Module() = default;

    } // namespace ast
} // namespace kyfoo
