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

std::unique_ptr<TypeScopeParser>
DeclarationScopeParser::parseTypeDefinition(Diagnostics& /*dgn*/,
                                            lexer::Scanner& scanner,
                                            ast::TypeDeclaration& declaration)
{
    // Check if type definition follows
    if ( scanner.peek().kind() != TokenKind::IndentGT )
        return nullptr;

    declaration.define(std::make_unique<ast::TypeScope>(myScope, declaration));
    return std::make_unique<TypeScopeParser>(declaration.definition());
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

std::unique_ptr<ast::TypeDeclaration>
DeclarationScopeParser::parseTypeDeclaration(lexer::Scanner& scanner)
{
    TypeDeclaration grammar;
    if ( parse(scanner, grammar) )
        return grammar.make();

    return nullptr;
}

std::unique_ptr<ast::ProcedureDeclaration>
DeclarationScopeParser::parseProcedureDeclaration(lexer::Scanner& scanner)
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
    else if ( auto typeDecl = parseTypeDeclaration(scanner) ) {
        auto newScopeParser = parseTypeDefinition(dgn, scanner, *typeDecl);
        myScope->append(std::move(typeDecl));

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
// TypeScopeParser

TypeScopeParser::TypeScopeParser(ast::TypeScope* scope)
    : DeclarationScopeParser(scope)
{
}

TypeScopeParser::~TypeScopeParser() = default;

std::tuple<bool, std::unique_ptr<DeclarationScopeParser>>
TypeScopeParser::parseNext(Diagnostics& dgn, lexer::Scanner& scanner)
{
    return DeclarationScopeParser::parseNext(dgn, scanner);
}

ast::TypeScope* TypeScopeParser::scope()
{
    return static_cast<ast::TypeScope*>(myScope);
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

    auto expr = parseExpression(scanner);
    if ( expr ) {
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
