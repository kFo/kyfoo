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
        class DataSumScope;
        class ProcedureScope;
    }

    namespace parser {

class DataSumScopeParser;
class DataProductScopeParser;
class ProcedureScopeParser;

class DeclarationScopeParser
{
public:
    DeclarationScopeParser(ast::DeclarationScope* scope);
    ~DeclarationScopeParser();

public:
    std::unique_ptr<DeclarationScopeParser> next(Diagnostics& dgn, lexer::Scanner& scanner);

    std::unique_ptr<DataSumScopeParser> parseDataSumDefinition(Diagnostics& dgn,
                                                               lexer::Scanner& scanner,
                                                               ast::DataSumDeclaration& declaration);
    std::unique_ptr<DataProductScopeParser> parseDataProductDefinition(Diagnostics& dgn,
                                                                       lexer::Scanner& scanner,
                                                                       ast::DataProductDeclaration& declaration);
    std::unique_ptr<ProcedureScopeParser> parseProcedureDefinition(Diagnostics& dgn,
                                                                   lexer::Scanner& scanner,
                                                                   ast::ProcedureDeclaration& declaration);

    std::tuple<bool, std::unique_ptr<DeclarationScopeParser>> parseNonProcedural(Diagnostics& dgn, lexer::Scanner& scanner);
    std::tuple<bool, std::unique_ptr<DeclarationScopeParser>> parseProcedural(Diagnostics& dgn, lexer::Scanner& scanner);

protected:
    void append(std::unique_ptr<ast::Declaration> decl);
    void parseAttributes(Diagnostics& dgn, lexer::Scanner& scanner);
    virtual std::tuple<bool, std::unique_ptr<DeclarationScopeParser>> parseNext(Diagnostics& dgn, lexer::Scanner& scanner);

protected:
    ast::DeclarationScope* myScope = nullptr;
    std::vector<std::unique_ptr<ast::Expression>> myAttributes;
};

class DataSumScopeParser : public DeclarationScopeParser
{
public:
    explicit DataSumScopeParser(ast::DataSumScope* scope);
    ~DataSumScopeParser();

protected:
    std::tuple<bool, std::unique_ptr<DeclarationScopeParser>> parseNext(Diagnostics& dgn, lexer::Scanner& scanner) override;

private:
    ast::DataSumScope* scope();
};

class DataProductScopeParser : public DeclarationScopeParser
{
public:
    explicit DataProductScopeParser(ast::DataProductScope* scope);
    ~DataProductScopeParser();

protected:
    std::tuple<bool, std::unique_ptr<DeclarationScopeParser>> parseNext(Diagnostics& dgn, lexer::Scanner& scanner) override;

private:
    ast::DataProductScope* scope();
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
