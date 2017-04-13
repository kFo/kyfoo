#pragma once

#include <memory>

namespace kyfoo {
    namespace ast {

class Module;

std::unique_ptr<Module> importAxioms();

    } // namespace ast
} // namespace kyfoo
