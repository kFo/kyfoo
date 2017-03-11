#pragma once

#include <memory>

#include <kyfoo/lexer/Scanner.hpp>
#include <kyfoo/parser/Productions.hpp>

namespace kyfoo {
    namespace lexer {
        class Scanner;
    }

    namespace ast {
        class Module;
    }

    namespace parser {


class DeclarationScopeParser
{
public:
    DeclarationScopeParser(ast::DeclarationScope* scope, lexer::indent_width_t indent);
    ~DeclarationScopeParser();

public:
    std::unique_ptr<DeclarationScopeParser> next(lexer::Scanner& scanner);
    lexer::indent_width_t indent() const;

    std::unique_ptr<ast::ProcedureDeclaration> parseProcedureDeclaration(lexer::Scanner& scanner);
    std::tuple<std::unique_ptr<ast::ProcedureScope>, lexer::indent_width_t> parseProcedureDefinition(lexer::Scanner& scanner);

protected:
    virtual std::tuple<bool, std::unique_ptr<DeclarationScopeParser>> parseNext(lexer::Scanner& scanner);

    enum IndentChange { Same, Increase, Decrease };
    IndentChange indentChange(lexer::indent_width_t indent) const;

protected:
    ast::DeclarationScope* myScope = nullptr;
    lexer::indent_width_t myIndent = 0;
};

class ProcedureScopeParser : public DeclarationScopeParser
{
public:
    ProcedureScopeParser(ast::ProcedureScope* scope,
                         lexer::indent_width_t indent);
    ~ProcedureScopeParser();

protected:
    std::tuple<bool, std::unique_ptr<DeclarationScopeParser>> parseNext(lexer::Scanner& scanner) override;

private:
    ast::ProcedureScope* scope();
};

std::unique_ptr<ast::Module> parseModule(const char* moduleName,
                                         lexer::Scanner& scanner);

    } // namespace parser
} // namespace kyfoo
