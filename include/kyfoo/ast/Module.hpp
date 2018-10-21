#pragma once

#include <filesystem>

#include <kyfoo/Slice.hpp>
#include <kyfoo/Stream.hpp>
#include <kyfoo/String.hpp>
#include <kyfoo/Types.hpp>
#include <kyfoo/Utilities.hpp>
#include <kyfoo/lexer/Scanner.hpp>
#include <kyfoo/ast/Clone.hpp>
#include <kyfoo/codegen/Codegen.hpp>

namespace kyfoo {

    class Diagnostics;

    namespace lexer {
        class Scanner;
        class Token;
    }

    namespace ast {

class AxiomsModule;
class Declaration;
class Scope;
class Module;

class ModuleSet
{
public:
    explicit ModuleSet(lexer::DefaultTokenFactory& tokenFactory);
    ~ModuleSet();

public:
    bool init(Diagnostics& dgn);
    void initBaseModules();

    Module* create(std::string name);
    Module* create(std::filesystem::path const& path);

    Module* createImplied(std::string name);

    Module* find(stringv name);
    Module* find(std::filesystem::path const& path);

    AxiomsModule& axioms();
    AxiomsModule const& axioms() const;

    Slice<Module*> modules();
    Slice<Module const*> modules() const;

    Slice<Module*> impliedImports();
    Slice<Module const*> impliedImports() const;

    std::filesystem::path const& path() const;

    lexer::DefaultTokenFactory& tokenFactory();

private:
    AxiomsModule* myAxioms;
    std::vector<Box<Module>> myModules;
    std::vector<Module*> myImpliedImports;
    std::filesystem::path myPath;
    lexer::DefaultTokenFactory& myTokenFactory;
};

class Module
{
public:
    Module(ModuleSet* moduleSet, std::string name);
    Module(ModuleSet* moduleSet, std::filesystem::path const& path);
    KYFOO_DEBUG_VIRTUAL ~Module();

public:
    stringv name() const;
    std::filesystem::path const& path() const;

public:
    void parse(Diagnostics& dgn);
    void parse(Diagnostics& dgn, Slice<char const> stream);

    void resolveImports(Diagnostics& dgn);
    void semantics(Diagnostics& dgn);

    Module const* import(Module* module);
    Module const* import(Diagnostics& dgn, lexer::Token const& token);

    void appendTemplateInstance(Declaration const* instance);

public:
    ModuleSet& moduleSet();
    ModuleSet const& moduleSet() const;

    AxiomsModule& axioms();
    AxiomsModule const& axioms() const;

    Slice<Module*> imports();
    Slice<Module*> imports() const;

    Scope* scope();
    Scope const* scope() const;

    bool imports(Module* module) const;
    bool parsed() const;

    stringv interpretString(Diagnostics& dgn, lexer::Token const& token) const;
    Slice<Declaration const*> templateInstantiations() const;

    codegen::CustomData* codegenData() const;
    void setCodegenData(Box<codegen::CustomData> data) const;

protected:
    ModuleSet* myModuleSet = nullptr;
    std::filesystem::path myPath;
    std::string myName;
    MMFile myFile;
    Box<Scope> myScope;
    std::vector<Declaration const*> myTemplateInstantiations;

    mutable std::vector<Module*> myImports;
    mutable std::map<std::string, std::string, StringComp> myStrings; // todo: shared

    mutable Box<codegen::CustomData> myCodegenData;
};

    } // namespace ast
} // namespace kyfoo
