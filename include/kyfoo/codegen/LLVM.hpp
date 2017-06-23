#pragma once

#include <memory>
#include <experimental/filesystem>

namespace kyfoo {

    class Diagnostics;
    class Error;

    namespace ast {
        class Module;
    }

    namespace codegen {

class LLVMGenerator
{
public:
    LLVMGenerator(Diagnostics& dgn, ast::Module* sourceModule);
    ~LLVMGenerator();

public:
    void generate();
    void write(std::experimental::filesystem::path const& path);

private:
    Error& error();

    Diagnostics& myDiagnostics;
    ast::Module* mySourceModule = nullptr;

    struct LLVMState;
    std::unique_ptr<LLVMState> myImpl;
};

    } // namespace codegen
} // namespace kyfoo
