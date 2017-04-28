auto source = R"axioms(
type integer

wordSize = 32
type integer<n : integer>

type integer<wordSize>

type pointer<T>

staticSize(p : pointer T) => wordSize
ssize = staticSize

type array<T>
    ;ptr : pointer T
    ;count : integer wordSize
)axioms";

#include "Axioms.hpp"

#include <kyfoo/ast/Expressions.hpp>
#include <kyfoo/ast/Declarations.hpp>
#include <kyfoo/ast/Module.hpp>
#include <kyfoo/ast/Scopes.hpp>

namespace kyfoo {
    namespace ast {

Module* createAxiomsModule(ModuleSet* moduleSet)
{
    std::stringstream s(source);
    auto module = moduleSet->create(std::string("axioms"));
    Diagnostics dgn;
    try {
        module->parse(dgn, s);
        if ( !dgn.errorCount() )
            return module;
    }
    catch (Diagnostics*) {
        // fall through
    }
    catch (std::exception const&) {
        // fall through
    }

    throw std::runtime_error("axioms module contains errors");
}

    } // namespace ast
} // namespace kyfoo
