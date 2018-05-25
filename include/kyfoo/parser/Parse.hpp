#pragma once

#include <memory>
#include <tuple>
#include <vector>

namespace kyfoo {
    class Diagnostics;

    namespace lexer {
        class Scanner;
    }

    namespace ast {
        class Module;
        class Declaration;
        class DeclarationScope;
        class DataSumScope;
        class DataProductScope;
        class ProcedureScope;
        class DataSumDeclaration;
        class DataProductDeclaration;
        class ProcedureDeclaration;
        class Expression;
    }

    namespace parser {

class DataSumScopeParser;
class DataProductScopeParser;
class ProcedureScopeParser;

class DeclarationScopeParser
{
public:
    DeclarationScopeParser(Diagnostics& dgn,
                           lexer::Scanner& scanner,
                           ast::DeclarationScope& scope);
    virtual ~DeclarationScopeParser();

public:
    std::unique_ptr<DeclarationScopeParser> next();

    std::unique_ptr<DataSumScopeParser> parseDataSumDefinition(ast::DataSumDeclaration& declaration);
    std::unique_ptr<DataProductScopeParser> parseDataProductDefinition(ast::DataProductDeclaration& declaration);
    std::unique_ptr<ProcedureScopeParser> parseProcedureDefinition(ast::ProcedureDeclaration& declaration);

    std::tuple<bool, std::unique_ptr<DeclarationScopeParser>> parseNonProcedural();
    std::tuple<bool, std::unique_ptr<DeclarationScopeParser>> parseProcedural();

    std::vector<std::unique_ptr<ast::Expression>> parameterContext() const;

protected:
    void append(std::unique_ptr<ast::Declaration> decl);
    void parseAttributes();
    virtual std::tuple<bool, std::unique_ptr<DeclarationScopeParser>> parseNext();

public:
    Diagnostics& diagnostics();
    Diagnostics const& diagnostics() const;

    lexer::Scanner& scanner();
    lexer::Scanner const& scanner() const;

    ast::DeclarationScope& scope();
    ast::DeclarationScope const& scope() const;

protected:
    Diagnostics* myDiagnostics = nullptr;
    lexer::Scanner* myScanner = nullptr;
    ast::DeclarationScope* myScope = nullptr;
    std::vector<std::unique_ptr<ast::Expression>> myAttributes;
    std::vector<std::unique_ptr<ast::Expression>> myParameterContext;
};

class DataSumScopeParser : public DeclarationScopeParser
{
public:
    DataSumScopeParser(Diagnostics& dgn,
                       lexer::Scanner& scanner,
                       ast::DataSumScope& scope);
    ~DataSumScopeParser() override;

protected:
    std::tuple<bool, std::unique_ptr<DeclarationScopeParser>> parseNext() override;
};

class DataProductScopeParser : public DeclarationScopeParser
{
public:
    DataProductScopeParser(Diagnostics& dgn,
                           lexer::Scanner& scanner,
                           ast::DataProductScope& scope);
    ~DataProductScopeParser();

protected:
    std::tuple<bool, std::unique_ptr<DeclarationScopeParser>> parseNext() override;
};

class ProcedureScopeParser : public DeclarationScopeParser
{
public:
    ProcedureScopeParser(Diagnostics& dgn,
                         lexer::Scanner& scanner,
                         ast::ProcedureScope& scope);
    ProcedureScopeParser(Diagnostics& dgn,
                         lexer::Scanner& scanner,
                         ast::ProcedureScope& scope,
                         bool isLoop);
    ~ProcedureScopeParser() override;

public:
    ast::ProcedureScope& scope();
    ast::ProcedureScope const& scope() const;

protected:
    std::tuple<bool, std::unique_ptr<DeclarationScopeParser>> parseNext() override;

private:
    bool myIsLoop = false;
};

template <typename T>
std::size_t parse(lexer::Scanner& scanner, T& production)
{
    std::size_t matches = 0;
    return production.match(scanner, matches);
}

template <typename P>
auto parse(DeclarationScopeParser& parser)
{
    P production;
    if ( parse(parser.scanner(), production) )
        return production.make(parser);

    return decltype(production.make(parser))();
}

void parseScope(std::unique_ptr<DeclarationScopeParser> parser);

    } // namespace parser
} // namespace kyfoo
