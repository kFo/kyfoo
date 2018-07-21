#pragma once

#include <filesystem>
#include <string>

#include <kyfoo/Slice.hpp>
#include <kyfoo/Types.hpp>
#include <kyfoo/Utilities.hpp>
#include <kyfoo/ast/Node.hpp>
#include <kyfoo/codegen/Codegen.hpp>

namespace kyfoo {

    class Diagnostics;

    namespace lexer {
        class Scanner;
    }

    namespace ast {

class AxiomsModule;
class Declaration;
class Scope;
class Module;

class ModuleSet
{
public:
    ModuleSet();
    ~ModuleSet();

public:
    bool init(Diagnostics& dgn);
    void initBaseModules();

    Module* create(std::string name);
    Module* create(std::filesystem::path const& path);

    Module* createImplied(std::string name);

    Module* find(std::string_view name);
    Module* find(std::filesystem::path const& path);

    AxiomsModule& axioms();
    AxiomsModule const& axioms() const;

    Slice<Module*> modules();
    Slice<Module const*> modules() const;

    Slice<Module*> impliedImports();
    Slice<Module const*> impliedImports() const;

    std::filesystem::path const& path() const;

private:
    AxiomsModule* myAxioms;
    std::vector<Box<Module>> myModules;
    std::vector<Module*> myImpliedImports;
    std::filesystem::path myPath;
};

class Module : public INode
{
public:
    Module(ModuleSet* moduleSet,
           std::string name);
    Module(ModuleSet* moduleSet,
           std::filesystem::path const& path);
    ~Module();

    // IIO
public:
    void io(IStream& stream) const override;

public:
    std::string_view name() const;
    std::filesystem::path const& path() const;

public:
    void parse(Diagnostics& dgn);
    void parse(Diagnostics& dgn, std::istream& stream);

    void resolveImports(Diagnostics& dgn);
    void semantics(Diagnostics& dgn);

    Module const* import(Module* module);
    Module const* import(Diagnostics& dgn, lexer::Token const& token);

    void appendTemplateInstance(Declaration const* instance);

public:
    ModuleSet const& moduleSet() const;

    AxiomsModule& axioms();
    AxiomsModule const& axioms() const;

    Slice<Module*> imports();
    Slice<Module*> imports() const;

    Scope* scope();
    Scope const* scope() const;

    bool imports(Module* module) const;
    bool parsed() const;

    std::string_view interpretString(Diagnostics& dgn, lexer::Token const& token) const;
    Slice<Declaration const*> templateInstantiations() const;

    codegen::CustomData* codegenData() const;
    void setCodegenData(Box<codegen::CustomData> data) const;

protected:
    ModuleSet* myModuleSet = nullptr;
    std::filesystem::path myPath;
    std::string myName;
    Box<Scope> myScope;
    std::vector<Declaration const*> myTemplateInstantiations;

    mutable std::vector<Module*> myImports;
    mutable std::map<std::string, std::string, StringComp> myStrings; // todo: shared

    mutable Box<codegen::CustomData> myCodegenData;
};

    } // namespace ast
} // namespace kyfoo
