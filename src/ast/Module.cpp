#include <kyfoo/ast/Module.hpp>

#include <cassert>

#include <fstream>
#include <filesystem>

#include <kyfoo/Diagnostics.hpp>

#include <kyfoo/lexer/Scanner.hpp>
#include <kyfoo/lexer/Token.hpp>

#include <kyfoo/parser/Parse.hpp>

#include <kyfoo/ast/Axioms.hpp>
#include <kyfoo/ast/Declarations.hpp>
#include <kyfoo/ast/Scopes.hpp>

namespace fs = std::experimental::filesystem;

namespace kyfoo {
    namespace ast {

//
// ModuleSet

ModuleSet::ModuleSet()
    : myAxioms(new AxiomsModule(this, "axioms"))
{
    if ( !myAxioms->init() )
        myAxioms.reset();
}

ModuleSet::~ModuleSet() = default;

Module* ModuleSet::create(std::string const& name)
{
    auto m = find(name);
    if ( m )
        return m;

    myModules.emplace_back(std::make_unique<Module>(this, name));
    m = myModules.back().get();

    for ( auto& i : myImpliedImports )
        m->import(i);

    return m;
}

Module* ModuleSet::create(fs::path const& path)
{
    auto m = find(path);
    if ( m )
        return m;

    myModules.emplace_back(std::make_unique<Module>(this, path));
    m = myModules.back().get();

    for ( auto& i : myImpliedImports )
        m->import(i);

    return m;
}

Module* ModuleSet::createImplied(std::string const& name)
{
    auto m = find(name);
    if ( m )
        return m;

    myModules.emplace_back(std::make_unique<Module>(this, name));
    myImpliedImports.push_back(myModules.back().get());
    return myImpliedImports.back();
}

Module* ModuleSet::find(std::string const& name)
{
    for ( auto& m : myModules )
        if ( m->name() == name )
            return m.get();

    return nullptr;
}

Module* ModuleSet::find(std::experimental::filesystem::path const& path)
{
    auto normalPath = canonical(path).make_preferred();
    for ( auto& m : myModules )
        if ( m->path() == normalPath )
            return m.get();

    return nullptr;
}

AxiomsModule* ModuleSet::axioms()
{
    return myAxioms.get();
}

AxiomsModule const* ModuleSet::axioms() const
{
    return myAxioms.get();
}

//
// Module

Module::Module(ModuleSet* moduleSet,
               std::string const& name)
    : myModuleSet(moduleSet)
    , myName(name)
{
    if ( moduleSet->axioms() )
        myImports.push_back(moduleSet->axioms());
}

Module::Module(ModuleSet* moduleSet,
               fs::path const& path)
    : myModuleSet(moduleSet)
    , myPath(canonical(path).make_preferred())
{
    myName = path.filename().replace_extension("").string();
    if ( moduleSet->axioms() )
        myImports.push_back(moduleSet->axioms());
}

Module::~Module() = default;

void Module::io(IStream& stream) const
{
    stream.openGroup("module");
    stream.next("name", myName);
    stream.next("scope", myScope);
    stream.closeGroup();
}

std::string const& Module::name() const
{
    return myName;
}

fs::path const& Module::path() const
{
    return myPath;
}

void Module::parse(Diagnostics& dgn)
{
    std::ifstream fin(path());
    if ( !fin ) {
        dgn.error(this) << "failed to open source file";
        dgn.die();
    }

    parse(dgn, fin);
}

void Module::parse(Diagnostics& dgn, std::istream& stream)
{
    lexer::Scanner scanner(stream);

    using lexer::TokenKind;

    myScope = std::make_unique<ast::DeclarationScope>(this);
    
    std::vector<std::unique_ptr<parser::DeclarationScopeParser>> scopeStack;
    scopeStack.emplace_back(std::make_unique<parser::DeclarationScopeParser>(myScope.get()));

    while ( scanner ) {
        auto nextScope = scopeStack.back()->next(dgn, scanner);
        if ( nextScope ) {
            scopeStack.push_back(std::move(nextScope));
        }
        else {
            switch (scanner.peek().kind()) {
            case TokenKind::EndOfFile:
                if ( !scopeStack.empty() )
                    scopeStack.resize(1);

            case TokenKind::IndentLT:
                break;

            case TokenKind::IndentGT:
            {
                dgn.error(myScope->module(), scanner.peek()) << "unexpected scope opening";
                dgn.die();
                return;
            }

            default:
                dgn.error(myScope->module(), scanner.peek()) << "expected end of scope";
                dgn.die();
            }

            while ( scanner.peek().kind() == TokenKind::IndentLT ) {
                if ( scopeStack.empty() ) {
                    dgn.error(this, scanner.peek()) << "indentation doesn't match an existing scope";
                    dgn.die();
                }

                scopeStack.pop_back();
                scanner.next();
            }
        }
    }

    if ( scanner.hasError() ) {
        dgn.error(this, scanner.peek()) << "lexical error";
        dgn.die();
    }

    if ( scopeStack.size() != 1 )
        throw std::runtime_error("parser scope imbalance");
}

void Module::resolveImports(Diagnostics& dgn)
{
    myScope->resolveImports(dgn);
}

void Module::semantics(Diagnostics& dgn)
{
    myScope->resolveSymbols(dgn);
}

Module const* Module::import(Module* module)
{
    auto m = find(begin(myImports), end(myImports), module);
    if ( m != end(myImports) )
        return *m;

    myImports.push_back(module);
    return myImports.back();
}

Module const* Module::import(Diagnostics& dgn, lexer::Token const& token)
{
    auto mod = myModuleSet->create(token.lexeme());
    if ( !mod ) {
        fs::path importPath = myPath;
        importPath.replace_filename(token.lexeme());
        importPath.replace_extension(".kf");

        if ( !exists(importPath) ) {
            dgn.error(this, token) << "import does not exist: " << importPath.string();
            return nullptr;
        }

        mod = myModuleSet->create(importPath);
        if ( !mod )
            throw std::runtime_error("failed to create module");
    }

    for ( auto& m : myImports )
        if ( m == mod )
            return m;

    myImports.push_back(mod);
    return myImports.back();
}

void Module::appendTemplateInstance(Declaration const* instance)
{
    myTemplateInstantiations.push_back(instance);
}

AxiomsModule* Module::axioms()
{
    return myModuleSet->axioms();
}

AxiomsModule const* Module::axioms() const
{
    return myModuleSet->axioms();
}

Slice<Module*> Module::imports()
{
    return myImports;
}

Slice<Module*> Module::imports() const
{
    return myImports;
}

DeclarationScope* Module::scope()
{
    return myScope.get();
}

DeclarationScope const* Module::scope() const
{
    return myScope.get();
}

bool Module::imports(Module* module) const
{
    return find(begin(myImports), end(myImports), module) != end(myImports);
}

bool Module::parsed() const
{
    return myScope.get() != nullptr;
}

Slice<Declaration const*> Module::templateInstantiations() const
{
    return myTemplateInstantiations;
}

    } // namespace ast
} // namespace kyfoo
