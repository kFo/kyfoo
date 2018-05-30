#pragma once

#include <tuple>
#include <vector>

#include <kyfoo/Types.hpp>

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
    Box<DeclarationScopeParser> next();

    Box<DataSumScopeParser> parseDataSumDefinition(ast::DataSumDeclaration& declaration);
    Box<DataProductScopeParser> parseDataProductDefinition(ast::DataProductDeclaration& declaration);
    Box<ProcedureScopeParser> parseProcedureDefinition(ast::ProcedureDeclaration& declaration);

    struct ParseResult
    {
        bool success;
        Box<DeclarationScopeParser> scope;
    };

    ParseResult parseNonProcedural();
    ParseResult parseProcedural();

    std::vector<Box<ast::Expression>> parameterContext() const;

protected:
    void append(Box<ast::Declaration> decl);
    void parseAttributes();
    virtual ParseResult parseNext();

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
    std::vector<Box<ast::Expression>> myAttributes;
    std::vector<Box<ast::Expression>> myParameterContext;
};

class DataSumScopeParser : public DeclarationScopeParser
{
public:
    DataSumScopeParser(Diagnostics& dgn,
                       lexer::Scanner& scanner,
                       ast::DataSumScope& scope);
    ~DataSumScopeParser() override;

protected:
    ParseResult parseNext() override;
};

class DataProductScopeParser : public DeclarationScopeParser
{
public:
    DataProductScopeParser(Diagnostics& dgn,
                           lexer::Scanner& scanner,
                           ast::DataProductScope& scope);
    ~DataProductScopeParser();

protected:
    ParseResult parseNext() override;
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
    ParseResult parseNext() override;

private:
    bool myIsLoop = false;
};

template <typename T>
uz parse(lexer::Scanner& scanner, T& production)
{
    uz matches = 0;
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

void parseScope(Box<DeclarationScopeParser> parser);

    } // namespace parser
} // namespace kyfoo
