#pragma once

#include <filesystem>
#include <memory>
#include <string>

#include <kyfoo/ast/Node.hpp>

namespace kyfoo {

    class Diagnostics;

    namespace lexer {
        class Scanner;
    }

    namespace ast {

class DeclarationScope;
class Module;

class ModuleSet
{
public:
    ModuleSet();
    ~ModuleSet();

public:
    Module* create(std::string const& name);
    Module* create(std::experimental::filesystem::path const& path);

    Module* find(std::string const& name);
    Module* find(std::experimental::filesystem::path const& path);

private:
    std::vector<std::unique_ptr<Module>> myModules;
};

class Module : public INode
{
public:
    Module(ModuleSet* moduleSet,
           std::string const& name);
    Module(ModuleSet* moduleSet,
           std::experimental::filesystem::path const& path);
    ~Module();

    // IIO
public:
    void io(IStream& stream) const override;

public:
    std::string const& name() const;
    std::experimental::filesystem::path const& path() const;

public:
    void parse(Diagnostics& dgn);
    void parse(Diagnostics& dgn, std::istream& stream);

    void resolveImports(Diagnostics& dgn);
    void semantics(Diagnostics& dgn);

    void import(Diagnostics& dgn, lexer::Token const& token);
    std::vector<Module*> const& imports() const;
    DeclarationScope const* scope() const;
    bool imports(Module* module) const;
    bool parsed() const;

private:
    ModuleSet* myModuleSet = nullptr;
    std::experimental::filesystem::path myPath;
    std::string myName;
    std::unique_ptr<DeclarationScope> myScope;

    std::vector<Module*> myImports;
};

    } // namespace ast
} // namespace kyfoo
