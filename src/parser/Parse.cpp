#include <kyfoo/parser/Parse.hpp>

#include <filesystem>
#include <fstream>

#include <kyfoo/Diagnostics.hpp>
#include <kyfoo/lexer/Token.hpp>
#include <kyfoo/ast/Module.hpp>
#include <kyfoo/ast/Scopes.hpp>

namespace fs = std::experimental::filesystem;

namespace kyfoo {
    namespace parser {

//
// DeclarationScopeParser

DeclarationScopeParser::DeclarationScopeParser(ast::DeclarationScope* scope)
    : myScope(scope)
{
}

DeclarationScopeParser::~DeclarationScopeParser() = default;

std::unique_ptr<ast::ImportDeclaration> parseImportDeclaration(lexer::Scanner& scanner)
{
    ImportDeclaration grammar;
    if ( parse(scanner, grammar) )
        return grammar.make();

    return nullptr;
}

std::unique_ptr<ast::VariableDeclaration> parseVariableDeclaration(lexer::Scanner& scanner)
{
    VariableDeclaration grammar;
    if ( parse(scanner, grammar) )
        return grammar.make();

    return nullptr;
}

std::unique_ptr<ast::SymbolDeclaration> parseSymbolDeclaration(lexer::Scanner& scanner)
{
    SymbolDeclaration grammar;
    if ( parse(scanner, grammar) )
        return grammar.make();

    return nullptr;
}

std::unique_ptr<ast::Expression> parseExpression(lexer::Scanner& scanner)
{
    Expression grammar;
    if ( parse(scanner, grammar) )
        return grammar.make();

    return nullptr;
}

std::unique_ptr<DataSumScopeParser>
DeclarationScopeParser::parseDataSumDefinition(Diagnostics& /*dgn*/,
                                               lexer::Scanner& scanner,
                                               ast::DataSumDeclaration& declaration)
{
    // Check if type definition follows
    if ( scanner.peek().kind() != TokenKind::IndentGT )
        return nullptr;

    scanner.next();

    declaration.define(std::make_unique<ast::DataSumScope>(myScope, declaration));
    return std::make_unique<DataSumScopeParser>(declaration.definition());
}

std::unique_ptr<DataProductScopeParser>
DeclarationScopeParser::parseDataProductDefinition(Diagnostics& /*dgn*/,
                                                   lexer::Scanner& scanner,
                                                   ast::DataProductDeclaration& declaration)
{
    // Check if type definition follows
    if ( scanner.peek().kind() != TokenKind::IndentGT )
        return nullptr;

    scanner.next();

    declaration.define(std::make_unique<ast::DataProductScope>(myScope, declaration));
    return std::make_unique<DataProductScopeParser>(declaration.definition());
}

std::unique_ptr<ProcedureScopeParser>
DeclarationScopeParser::parseProcedureDefinition(Diagnostics& dgn,
                                                 lexer::Scanner& scanner,
                                                 ast::ProcedureDeclaration& declaration)
{
    // Check if a procedure definition follows
    if ( scanner.peek().kind() == TokenKind::Yield ) {
        scanner.next(); // yield
        declaration.define(std::make_unique<ast::ProcedureScope>(myScope, declaration));
        if ( !isIndent(scanner.peek().kind()) ) {
            auto expr = parseExpression(scanner);
            if ( !expr ) {
                dgn.error(myScope->module(), scanner.peek()) << "expected expression following procedure declaration";
                dgn.die();
            }

            declaration.definition()->append(std::move(expr));
            return nullptr;
        }

        auto indent = scanner.next();
        if ( indent.kind() != TokenKind::IndentGT ) {
            dgn.error(myScope->module(), scanner.peek()) << "expected new scope for procedure definition";
            dgn.die();
        }

        return std::make_unique<ProcedureScopeParser>(declaration.definition());
    }

    return nullptr;
}

std::unique_ptr<ast::DataSumDeclaration>
parseDataSumDeclaration(lexer::Scanner& scanner)
{
    DataSumDeclaration grammar;
    if ( parse(scanner, grammar) )
        return grammar.make();

    return nullptr;
}

std::unique_ptr<ast::DataSumDeclaration::Constructor>
parseDataSumConstructor(lexer::Scanner& scanner)
{
    DataSumConstructor grammar;
    if ( parse(scanner, grammar) )
        return grammar.make();

    return nullptr;
}

std::unique_ptr<ast::DataProductDeclaration>
parseDataProductDeclaration(lexer::Scanner& scanner)
{
    DataProductDeclaration grammar;
    if ( parse(scanner, grammar) )
        return grammar.make();

    return nullptr;
}

std::unique_ptr<ast::DataProductDeclaration::Field>
parseDataProductDeclarationField(lexer::Scanner& scanner)
{
    DataProductDeclarationField grammar;
    if ( parse(scanner, grammar) )
        return grammar.make();

    return nullptr;
}

std::unique_ptr<ast::ProcedureDeclaration>
parseProcedureDeclaration(lexer::Scanner& scanner)
{
    ProcedureDeclaration grammar;
    if ( parse(scanner, grammar) ) {
        return grammar.make();
    }

    return nullptr;
}

std::tuple<bool, std::unique_ptr<DeclarationScopeParser>>
DeclarationScopeParser::parseNext(Diagnostics& dgn, lexer::Scanner& scanner)
{
    if ( auto importDecl = parseImportDeclaration(scanner) ) {
        myScope->append(std::move(importDecl));
        return std::make_tuple(true, nullptr);
    }
    else if ( auto symDecl = parseSymbolDeclaration(scanner) ) {
        myScope->append(std::move(symDecl));
        return std::make_tuple(true, nullptr);
    }
    else if ( auto dsDecl = parseDataSumDeclaration(scanner) ) {
        auto newScopeParser = parseDataSumDefinition(dgn, scanner, *dsDecl);
        myScope->append(std::move(dsDecl));

        return std::make_tuple(true, std::move(newScopeParser));
    }
    else if ( auto dpDecl = parseDataProductDeclaration(scanner) ) {
        auto newScopeParser = parseDataProductDefinition(dgn, scanner, *dpDecl);
        myScope->append(std::move(dpDecl));

        return std::make_tuple(true, std::move(newScopeParser));
    }
    else if ( auto procDecl = parseProcedureDeclaration(scanner) ) {
        auto newScopeParser = parseProcedureDefinition(dgn, scanner, *procDecl);
        myScope->append(std::move(procDecl));

        return std::make_tuple(true, std::move(newScopeParser));
    }

    return std::make_tuple(false, nullptr);
}

std::unique_ptr<DeclarationScopeParser> DeclarationScopeParser::next(Diagnostics& dgn, lexer::Scanner& scanner)
{
    while ( scanner ) {
        bool success;
        std::unique_ptr<DeclarationScopeParser> newScopeParser;
        std::tie(success, newScopeParser) = parseNext(dgn, scanner);

        if ( !success ) {
            dgn.error(myScope->module(), scanner.peek()) << "grammar at this point is not recognized";
            dgn.die();
        }
        
        if ( newScopeParser )
            return newScopeParser;

        if ( scanner.peek().kind() != TokenKind::IndentEQ )
            return nullptr;

        scanner.next();
    }

    return nullptr;
}

//
// DataSumScopeParser

DataSumScopeParser::DataSumScopeParser(ast::DataSumScope* scope)
    : DeclarationScopeParser(scope)
{
}

DataSumScopeParser::~DataSumScopeParser() = default;

std::tuple<bool, std::unique_ptr<DeclarationScopeParser>>
DataSumScopeParser::parseNext(Diagnostics& /*dgn*/, lexer::Scanner& scanner)
{
    if ( auto dsCtor = parseDataSumConstructor(scanner) ) {
        dsCtor->setParent(scope()->declaration()->as<ast::DataSumDeclaration>());
        myScope->append(std::move(dsCtor));
        return std::make_tuple(true, nullptr);
    }

    return std::make_tuple(false, nullptr);
}

ast::DataSumScope* DataSumScopeParser::scope()
{
    return static_cast<ast::DataSumScope*>(myScope);
}

//
// DataProductScopeParser

DataProductScopeParser::DataProductScopeParser(ast::DataProductScope* scope)
    : DeclarationScopeParser(scope)
{
}

DataProductScopeParser::~DataProductScopeParser() = default;

std::tuple<bool, std::unique_ptr<DeclarationScopeParser>>
DataProductScopeParser::parseNext(Diagnostics& dgn, lexer::Scanner& scanner)
{
    if ( auto field = parseDataProductDeclarationField(scanner) ) {
        field->setParent(scope()->declaration()->as<ast::DataProductDeclaration>());
        myScope->append(std::move(field));
        return std::make_tuple(true, nullptr);
    }
    else if ( auto dsDecl = parseDataSumDeclaration(scanner) ) {
        auto newScopeParser = parseDataSumDefinition(dgn, scanner, *dsDecl);
        myScope->append(std::move(dsDecl));

        return std::make_tuple(true, std::move(newScopeParser));
    }

    return std::make_tuple(false, nullptr);
}

ast::DataProductScope* DataProductScopeParser::scope()
{
    return static_cast<ast::DataProductScope*>(myScope);
}

//
// ProcedureScopeParser

ProcedureScopeParser::ProcedureScopeParser(ast::ProcedureScope* scope)
    : DeclarationScopeParser(scope)
{
}

ProcedureScopeParser::~ProcedureScopeParser() = default;

std::tuple<bool, std::unique_ptr<DeclarationScopeParser>>
ProcedureScopeParser::parseNext(Diagnostics& dgn, lexer::Scanner& scanner)
{
    // Allow declarations
    {
        auto declParse = DeclarationScopeParser::parseNext(dgn, scanner);
        if ( std::get<0>(declParse) )
            return declParse;
    }

    if ( auto varDecl = parseVariableDeclaration(scanner) ) {
        static_cast<ast::DeclarationScope*>(scope())->append(std::move(varDecl));
        return std::make_tuple(true, nullptr);
    }
    else if ( auto expr = parseExpression(scanner) ) {
        scope()->append(std::move(expr));
        return std::make_tuple(true, nullptr);
    }

    return std::make_tuple(false, nullptr);
}

ast::ProcedureScope* ProcedureScopeParser::scope()
{
    return static_cast<ast::ProcedureScope*>(myScope);
}

    } // namespace parser
} // namespace kyfoo
