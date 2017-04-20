#include <kyfoo/ast/Module.hpp>

#include <cassert>

#include <fstream>
#include <filesystem>

#include <kyfoo/Diagnostics.hpp>

#include <kyfoo/lexer/Scanner.hpp>
#include <kyfoo/lexer/Token.hpp>

#include <kyfoo/parser/Parse.hpp>

#include <kyfoo/ast/Declarations.hpp>
#include <kyfoo/ast/Scopes.hpp>

namespace fs = std::experimental::filesystem;

namespace kyfoo {
    namespace ast {

Module::Module(std::string const& name)
    : myName(name)
{
}

Module::Module(fs::path const& path)
    : myPath(canonical(path).make_preferred())
{
    myName = path.filename().string();
}

Module::~Module() = default;

void Module::io(IStream& stream)
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

    lexer::Scanner scanner(fin);

    using lexer::TokenKind;

    myScope = std::make_unique<ast::DeclarationScope>(this);
    
    std::vector<std::unique_ptr<parser::DeclarationScopeParser>> scopeStack;
    scopeStack.emplace_back(std::make_unique<parser::DeclarationScopeParser>(myScope.get(), 0));

    while ( scanner ) {
        auto nextScope = scopeStack.back()->next(dgn, scanner);
        if ( nextScope ) {
            scopeStack.push_back(std::move(nextScope));
        }
        else {
            if ( scanner.peek().kind() == TokenKind::EndOfFile )
                break;

            lexer::indent_width_t nextIndentWidth = 0;
            if ( scanner.peek().kind() == TokenKind::Indent )
                nextIndentWidth = scanner.next().lexeme().size();

            while ( !scopeStack.empty() && scopeStack.back()->indent() != nextIndentWidth ) {
                scopeStack.pop_back();
            }

            if ( scopeStack.empty() ) {
                dgn.error(this, scanner.peek()) << "indentation doesn't match an existing scope";
                dgn.die();
            }
        }
    }

    return ;
}

void Module::semantics(Diagnostics& dgn)
{
    myScope->resolveSymbols(dgn);
}

    } // namespace ast
} // namespace kyfoo
