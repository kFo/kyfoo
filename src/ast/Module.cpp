#include <kyfoo/ast/Module.hpp>

#include <cassert>

#include <filesystem>

#include <kyfoo/Algorithms.hpp>
#include <kyfoo/Diagnostics.hpp>
#include <kyfoo/Stream.hpp>

#include <kyfoo/lexer/Scanner.hpp>
#include <kyfoo/lexer/Token.hpp>

#include <kyfoo/parser/Parse.hpp>

#include <kyfoo/ast/Axioms.hpp>
#include <kyfoo/ast/Context.hpp>
#include <kyfoo/ast/Declarations.hpp>
#include <kyfoo/ast/Scopes.hpp>

namespace kyfoo::ast {

//
// ModuleSet

ModuleSet::ModuleSet(lexer::DefaultTokenFactory& tokenFactory)
    : myAxioms(new AxiomsModule(this, "axioms"))
    , myTokenFactory(tokenFactory)
{
    myModules.append(myAxioms);
}

ModuleSet::~ModuleSet() = default;

bool ModuleSet::init(Diagnostics& dgn)
{
    if ( myAxioms->init(dgn) ) {
        myImpliedImports.append(myAxioms);
        return true;
    }

    return false;
}

void ModuleSet::initBaseModules()
{
    if ( !myModules )
        return;

    std::filesystem::path commonPath;
    for ( auto const& m : myModules) {
        if ( m->path().empty() )
            continue;

        if ( commonPath.empty() )
            commonPath = m->path().parent_path();
        else if ( m->path().parent_path() != commonPath )
            return;
    }

    myPath = commonPath;
}

Module* ModuleSet::create(std::string name)
{
    auto m = find(stringv(name));
    if ( m )
        return m;

    myModules.append(mk<Module>(this, std::move(name)));
    m = myModules.back().get();

    for ( auto& i : myImpliedImports )
        m->import(i);

    return m;
}

Module* ModuleSet::create(std::filesystem::path const& path)
{
    auto m = find(path);
    if ( m )
        return m;

    myModules.append(mk<Module>(this, path));
    m = myModules.back().get();

    for ( auto& i : myImpliedImports )
        m->import(i);

    return m;
}

Module* ModuleSet::createImplied(std::string name)
{
    auto m = find(stringv(name));
    if ( m )
        return m;

    myModules.append(mk<Module>(this, std::move(name)));
    myImpliedImports.append(myModules.back().get());
    return myImpliedImports.back();
}

Module* ModuleSet::find(stringv name)
{
    for ( auto& m : myModules )
        if ( m->name() == name )
            return m.get();

    return nullptr;
}

Module* ModuleSet::find(std::filesystem::path const& path)
{
    auto normalPath = canonical(path).make_preferred();
    for ( auto& m : myModules )
        if ( m->path() == normalPath )
            return m.get();

    return nullptr;
}

AxiomsModule& ModuleSet::axioms()
{
    return *myAxioms;
}

AxiomsModule const& ModuleSet::axioms() const
{
    return *myAxioms;
}

Slice<Module*> ModuleSet::modules()
{
    return myModules;
}

Slice<Module const*> ModuleSet::modules() const
{
    return myModules;
}

Slice<Module*> ModuleSet::impliedImports()
{
    return myImpliedImports;
}

Slice<Module const*> ModuleSet::impliedImports() const
{
    return myImpliedImports;
}

std::filesystem::path const& ModuleSet::path() const
{
    return myPath;
}

lexer::DefaultTokenFactory& ModuleSet::tokenFactory()
{
    return myTokenFactory;
}

//
// Module

Module::Module(ModuleSet* moduleSet, std::string name)
    : myModuleSet(moduleSet)
    , myName(std::move(name))
{
}

Module::Module(ModuleSet* moduleSet, std::filesystem::path const& path)
    : myModuleSet(moduleSet)
    , myPath(canonical(path).make_preferred())
{
    myName = path.filename().replace_extension("").string();
}

Module::~Module() = default;

stringv Module::name() const
{
    return myName;
}

std::filesystem::path const& Module::path() const
{
    return myPath;
}

void Module::parse(Diagnostics& dgn)
{
    if ( auto err = MMFile::open(path()) )
        throw SystemException(err);
    else
        myFile = unwrap(err);

    parse(dgn, myFile.view());
}

void Module::parse(Diagnostics& dgn, Slice<char const> stream)
{
    lexer::Scanner scanner(moduleSet().tokenFactory(), stream);

    if ( !myScope )
        myScope = mk<ast::Scope>(*this);

    parseScope(mk<parser::DeclarationScopeParser>(dgn, scanner, *myScope));
}

void Module::resolveImports(Diagnostics& dgn)
{
    myScope->resolveImports(dgn);
}

void Module::semantics(Diagnostics& dgn)
{
    Resolver resolver(*myScope);
    Context ctx(*this, dgn, resolver);
    ctx.resolveScope(*myScope);
    ctx.resolveScopeAttributes(*myScope);
}

Module const* Module::import(Module* module)
{
    auto m = scan(myImports, module);
    if ( m )
        return m.front();

    myImports.append(module);
    return myImports.back();
}

Module const* Module::import(Diagnostics& dgn, lexer::Token const& token)
{
    auto mod = myModuleSet->create(mkString(token.lexeme()));
    if ( !mod ) {
        std::filesystem::path importPath = myPath;
        importPath.replace_filename(mkString(token.lexeme()));
        importPath.replace_extension(".kf");

        if ( !exists(importPath) ) {
            dgn.error(*this, diag::no_import, token);
                //.see(importPath.string()); todo
            return nullptr;
        }

        mod = myModuleSet->create(importPath);
        ENFORCE(mod, "failed to create module");
    }

    for ( auto& m : myImports )
        if ( m == mod )
            return m;

    myImports.append(mod);
    return myImports.back();
}

void Module::appendTemplateInstance(Declaration const* instance)
{
    auto e = scan(myTemplateInstantiations, instance);
    if ( e )
        myTemplateInstantiations.append(instance);
}

ModuleSet& Module::moduleSet()
{
    return *myModuleSet;
}

ModuleSet const& Module::moduleSet() const
{
    return *myModuleSet;
}

AxiomsModule& Module::axioms()
{
    return myModuleSet->axioms();
}

AxiomsModule const& Module::axioms() const
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

Scope* Module::scope()
{
    return myScope.get();
}

Scope const* Module::scope() const
{
    return myScope.get();
}

bool Module::imports(Module* module) const
{
    return bool(scan(myImports, module));
}

bool Module::parsed() const
{
    return myScope.get() != nullptr;
}

stringv Module::interpretString(Diagnostics& dgn, lexer::Token const& token) const
{
    auto e = myStrings.lower_bound(token.lexeme());
    if ( e != end(myStrings) && e->first == token.lexeme() )
        return e->second;

    auto toHex = [](char c) {
        if ( '0' <= c && c <= '9' )
            return c - '0';
        else if ( 'a' <= c && c <= 'f' )
            return c - 'a';
        else if ( 'A' <= c && c <= 'F' )
            return c - 'A';

        return -1;
    };

    auto const& in = token.lexeme();
    std::string out;
    out.reserve(in.card());

    ENFORCE(in[0] == '"', "unhandled string kind");

    for ( uz i = 1; i < in.card() - 1; ++i ) {
        if ( in[i] != '\\' ) {
            out.push_back(in[i]);
        }
        else {
            ++i;
            if ( i == in.card() - 1 ) {
                dgn.error(*this, diag::expected_string_escape, token);
                    //.see(i); // todo
                return token.lexeme();
            }

            switch ( in[i] ) {
            case  '0': out.push_back(0x00); break;
            case  'a': out.push_back(0x07); break;
            case  'b': out.push_back(0x08); break;
            case  't': out.push_back(0x09); break;
            case  'n': out.push_back(0x0a); break;
            case  'v': out.push_back(0x0b); break;
            case  'f': out.push_back(0x0c); break;
            case  'r': out.push_back(0x0d); break;
            case  '"': out.push_back(0x22); break;
            case '\'': out.push_back(0x27); break;
            case  '?': out.push_back(0x3f); break;
            case '\\': out.push_back(0x5c); break;
            case 'x': {
                if ( i + 2 >= in.card() - 1 ) {
                    dgn.error(*this, diag::expected_string_escape_two_hex, token);
                        //.see(i) // todo
                    return token.lexeme();
                }

                int digit[2] = { toHex(in[i + 1]), toHex(in[i + 2]) };
                if ( digit[0] < 0 || digit[1] < 0 ) {
                    dgn.error(*this, diag::expected_string_escape_two_hex, token);
                        // .see(i) // todo
                    return token.lexeme();
                }

                out.push_back(static_cast<char>(digit[0] * 16 + digit[1]));
                break;
            }

            case 'u': {
                dgn.error(*this, diag::not_implemented, token);
                return token.lexeme();
            }

            default:
                dgn.error(*this, diag::expected_string_escape, token);
                    // .see(i); // todo
                return token.lexeme();
            }
        }
    }

    return myStrings.insert_or_assign(e, mkString(in), std::move(out))->second;
}

Slice<Declaration const*> Module::templateInstantiations() const
{
    return myTemplateInstantiations;
}

Declaration* Module::fabricate(Box<Declaration> decl)
{
    myFabDeclarations.append(std::move(decl));
    return myFabDeclarations.back().get();
}

Expression* Module::fabricate(Box<Expression> expr)
{
    myFabExpressions.append(std::move(expr));
    return myFabExpressions.back().get();
}

codegen::CustomData* Module::codegenData() const
{
    return myCodegenData.get();
}

void Module::setCodegenData(Box<codegen::CustomData> data) const
{
    myCodegenData = std::move(data);
}

} // namespace kyfoo::ast
