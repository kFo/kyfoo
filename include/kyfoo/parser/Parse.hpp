#pragma once

#include <tuple>

#include <kyfoo/Array.hpp>
#include <kyfoo/Types.hpp>

namespace kyfoo {
    class Diagnostics;

    namespace lexer {
        class Scanner;
    }

    namespace ast {
        class BasicBlock;
        class DataTypeDeclaration;
        class DataTypeScope;
        class Declaration;
        class Expression;
        class ProcedureDeclaration;
        class ProcedureScope;
        class Scope;
        class Module;
    }

    namespace parser {

class DataTypeScopeParser;
class ProcedureScopeParser;

class DeclarationScopeParser
{
public:
    DeclarationScopeParser(Diagnostics& dgn,
                           lexer::Scanner& scanner,
                           ast::Scope& scope);
    virtual ~DeclarationScopeParser();

public:
    Box<DeclarationScopeParser> next();

    Box<DataTypeScopeParser> parseDataTypeDefinition(ast::DataTypeDeclaration& declaration);
    Box<ProcedureScopeParser> parseProcedureDefinition(ast::ProcedureDeclaration& declaration);

    struct ParseResult
    {
        bool success;
        Box<DeclarationScopeParser> scope;
    };

    ParseResult parseNonProcedural();
    ParseResult parseProcedural();

    ab<Box<ast::Expression>> parameterContext() const;

protected:
    void append(Box<ast::Declaration> decl);
    void parseAttributes();
    virtual ParseResult parseNext();

public:
    Diagnostics& diagnostics();
    Diagnostics const& diagnostics() const;

    lexer::Scanner& scanner();
    lexer::Scanner const& scanner() const;

    ast::Scope& scope();
    ast::Scope const& scope() const;

protected:
    Diagnostics* myDiagnostics = nullptr;
    lexer::Scanner* myScanner = nullptr;
    ast::Scope* myScope = nullptr;
    ab<Box<ast::Expression>> myAttributes;
    ab<Box<ast::Expression>> myParameterContext;
};

class DataTypeScopeParser : public DeclarationScopeParser
{
public:
    DataTypeScopeParser(Diagnostics& dgn,
                        lexer::Scanner& scanner,
                        ast::DataTypeScope& scope);
    ~DataTypeScopeParser() override;

public:
    ast::DataTypeScope& scope();
    ast::DataTypeScope const& scope() const;

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
                         ast::BasicBlock* loopBlock);
    ~ProcedureScopeParser() override;

public:
    ast::ProcedureScope& scope();
    ast::ProcedureScope const& scope() const;

protected:
    ParseResult parseNext() override;

private:
    ast::BasicBlock* myLoopBlock = nullptr;
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
