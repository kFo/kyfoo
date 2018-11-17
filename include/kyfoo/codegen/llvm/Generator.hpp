#pragma once

#include <filesystem>

#include <kyfoo/Types.hpp>

namespace kyfoo {
    class Diagnostics;

    namespace ast {
        class ModuleSet;
        class Module;
    }
}

namespace kyfoo::codegen::llvm {

class Context;

class Generator
{
public:
    Generator(Diagnostics& dgn, ast::ModuleSet& moduleSet);
    ~Generator();

public:
    void generate(ast::Module const& module);

    void write(ast::Module const& module,
               std::filesystem::path const& path);
    void writeIR(ast::Module const& module,
                 std::filesystem::path const& path);

private:
    Box<Context> myImpl;
};

} // namespace kyfoo::codegen::llvm
