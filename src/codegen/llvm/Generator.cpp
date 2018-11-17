#include <kyfoo/codegen/llvm/Generator.hpp>

#include "Context.hpp"
#include "Visitors.hpp"

namespace kyfoo::codegen::llvm {

//
// Generator

Generator::Generator(Diagnostics& dgn, ast::ModuleSet& moduleSet)
    : myImpl(mk<Context>(dgn, moduleSet))
{
}

Generator::~Generator() = default;

void Generator::generate(ast::Module const& module)
{
    myImpl->generate(module);
}

void Generator::write(ast::Module const& module, std::filesystem::path const& path)
{
    myImpl->write(module, path);
}

void Generator::writeIR(ast::Module const& module, std::filesystem::path const& path)
{
    myImpl->writeIR(module, path);
}

} // namespace kyfoo::codegen::llvm
