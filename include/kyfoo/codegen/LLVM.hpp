#pragma once

#include <memory>
#include <experimental/filesystem>

namespace kyfoo {

    class Diagnostics;

    namespace ast {
        class ModuleSet;
        class Module;
    }

    namespace codegen {

class LLVMGenerator
{
public:
    LLVMGenerator(Diagnostics& dgn, ast::ModuleSet& moduleSet);
    ~LLVMGenerator();

public:
    void generate(ast::Module const& module);
    void write(ast::Module const& module,
               std::experimental::filesystem::path const& path);
    void writeIR(ast::Module const& module,
                 std::experimental::filesystem::path const& path);

private:
    struct LLVMState;
    std::unique_ptr<LLVMState> myImpl;
};

    } // namespace codegen
} // namespace kyfoo
