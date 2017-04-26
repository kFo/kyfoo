#pragma once

#include <memory>

#include <kyfoo/lexer/Scanner.hpp>
#include <kyfoo/parser/Productions.hpp>

namespace kyfoo {
    class Diagnostics;

    namespace lexer {
        class Scanner;
    }

    namespace ast {
        class Module;
        class DeclarationScope;
        class TypeScope;
        class ProcedureScope;
    }

    namespace parser {

class TypeScopeParser;
class ProcedureScopeParser;

class DeclarationScopeParser
{
public:
    DeclarationScopeParser(ast::DeclarationScope* scope);
    ~DeclarationScopeParser();

public:
    std::unique_ptr<DeclarationScopeParser> next(Diagnostics& dgn, lexer::Scanner& scanner);

    std::unique_ptr<ast::TypeDeclaration> parseTypeDeclaration(lexer::Scanner& scanner);
    std::unique_ptr<ast::ProcedureDeclaration> parseProcedureDeclaration(lexer::Scanner& scanner);

    std::unique_ptr<TypeScopeParser> parseTypeDefinition(Diagnostics& dgn,
                                                         lexer::Scanner& scanner,
                                                         ast::TypeDeclaration& declaration);
    std::unique_ptr<ProcedureScopeParser> parseProcedureDefinition(Diagnostics& dgn,
                                                                   lexer::Scanner& scanner,
                                                                   ast::ProcedureDeclaration& declaration);

protected:
    virtual std::tuple<bool, std::unique_ptr<DeclarationScopeParser>> parseNext(Diagnostics& dgn, lexer::Scanner& scanner);

protected:
    ast::DeclarationScope* myScope = nullptr;
};

class TypeScopeParser : public DeclarationScopeParser
{
public:
    explicit TypeScopeParser(ast::TypeScope* scope);
    ~TypeScopeParser();

protected:
    std::tuple<bool, std::unique_ptr<DeclarationScopeParser>> parseNext(Diagnostics& dgn, lexer::Scanner& scanner) override;

private:
    ast::TypeScope* scope();
};

class ProcedureScopeParser : public DeclarationScopeParser
{
public:
    explicit ProcedureScopeParser(ast::ProcedureScope* scope);
    ~ProcedureScopeParser();

protected:
    std::tuple<bool, std::unique_ptr<DeclarationScopeParser>> parseNext(Diagnostics& dgn, lexer::Scanner& scanner) override;

private:
    ast::ProcedureScope* scope();
};

    } // namespace parser
} // namespace kyfoo
