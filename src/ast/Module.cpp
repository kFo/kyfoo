#include <kyfoo/ast/Module.hpp>

#include <cassert>

#include <kyfoo/Diagnostics.hpp>

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

void Module::io(IStream& stream)
{
    stream.openGroup("module");
    stream.next("name", myName);
    stream.next("scope", myScope);
    stream.closeGroup();
}

std::string const& Module::name() const
{
    return myName;
}

void Module::semantics(Diagnostics& dgn)
{
    myScope->resolveSymbols(dgn);
}

    } // namespace ast
} // namespace kyfoo
