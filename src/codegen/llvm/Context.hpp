#pragma once

#include <filesystem>
#include <optional>

#include <kyfoo/Box.hpp>
#include <kyfoo/String.hpp>

namespace llvm {
    class DataLayout;
    class LLVMContext;
    class TargetMachine;
    class Type;
}

namespace kyfoo {
    class Diagnostics;

    namespace ast {
        class AxiomsModule;
        class DataTypeDeclaration;
        class Declaration;
        class Expression;
        class Module;
        class ModuleSet;
        class Scope;
    }
}

namespace kyfoo::codegen::llvm {

//
// Context

class Context
{
public:
    Context(Diagnostics& dgn,
            ast::ModuleSet& moduleSet,
            stringv targetTriple = stringv());

    ~Context();

public:
    void write(ast::Module const& module, std::filesystem::path const& path);
    void writeIR(ast::Module const& module, std::filesystem::path const& path);

    void generate(ast::Module const& module);
    void generate(ast::Declaration const& decl);
    void generate(ast::DataTypeDeclaration const& dt);

    std::optional<uz> sizeOf(ast::Declaration const& decl);
    std::optional<uz> sizeOf(ast::Expression const& expr);

public:
    ::llvm::Type* toType(ast::Declaration const& decl);
    ::llvm::Type* toType(ast::Expression const& expr);
    ::llvm::Type* intrinsicType(ast::Declaration const& decl);

private:
    void init(std::string targetTriple);

    ast::AxiomsModule const& axioms() const;

private:
    Diagnostics& myDgn;
    ast::ModuleSet& myModuleSet;
    Box<::llvm::LLVMContext> myContext;

    std::string myTargetTriple;
    ::llvm::TargetMachine* myTargetMachine = nullptr;
    Box<::llvm::DataLayout> myDefaultDataLayout;
};

} // namespace kyfoo::codegen::llvm
