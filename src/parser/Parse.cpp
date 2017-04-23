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

void discardEmptyLines(lexer::Scanner& scanner)
{
    using lexer::TokenKind;

    for (;;) {
        if (scanner.peek().kind() == TokenKind::LineBreak) {
            scanner.next();
            continue;
        }
        else if (scanner.peek(0).kind() == TokenKind::Indent
              && scanner.peek(1).kind() == TokenKind::LineBreak )
        {
            scanner.next();
            scanner.next();
            continue;
        }

        return;
    }
}

//
// DeclarationScopeParser

DeclarationScopeParser::DeclarationScopeParser(ast::DeclarationScope* scope,
                                               lexer::indent_width_t indent)
    : myScope(scope)
    , myIndent(indent)
{
}

DeclarationScopeParser::~DeclarationScopeParser() = default;

DeclarationScopeParser::IndentChange DeclarationScopeParser::indentChange(lexer::indent_width_t indent) const
{
    if ( indent == myIndent )
        return Same;
    else if ( indent > myIndent )
        return Increase;

    return Decrease;
}

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

std::unique_ptr<ast::ValueExpression> parseExpression(lexer::Scanner& scanner)
{
    ValueExpression grammar;
    if ( parse(scanner, grammar) )
        return grammar.make();

    return nullptr;
}

std::tuple<std::unique_ptr<ast::TypeScope>, lexer::indent_width_t>
DeclarationScopeParser::parseTypeDefinition(Diagnostics& /*dgn*/,
                                            lexer::Scanner& scanner,
                                            ast::TypeDeclaration& declaration)
{
    // Check if type definition follows
    if ( scanner.peek().kind() != TokenKind::LineBreak ||
        scanner.peek(1).kind() != TokenKind::Indent )
    {
        return std::make_tuple(nullptr, 0);
    }

    auto nextIndent = scanner.peek(1);
    auto nextIndentWidth = nextIndent.lexeme().size();
    switch ( indentChange(nextIndentWidth) ) {
    case Same:
    case Decrease:
        return std::make_tuple(nullptr, 0);
    }

    scanner.next(); // LineBreak
    scanner.next(); // Indent
    return std::make_tuple(std::make_unique<ast::TypeScope>(myScope, declaration), nextIndentWidth);
}

std::tuple<std::unique_ptr<ast::ProcedureScope>, lexer::indent_width_t>
DeclarationScopeParser::parseProcedureDefinition(Diagnostics& dgn,
                                                 lexer::Scanner& scanner,
                                                 ast::ProcedureDeclaration& declaration)
{
    // Check if a procedure definition follows
    if ( scanner.peek().kind() == TokenKind::Yield ) {
        scanner.next(); // yield
        auto scope = std::make_unique<ast::ProcedureScope>(myScope, declaration);
        if ( scanner.peek().kind() != TokenKind::LineBreak ) {
            auto expr = parseExpression(scanner);
            if ( !expr ) {
                dgn.error(myScope->module(), scanner.peek()) << "expected expression following procedure declaration";
                dgn.die();
            }

            scope->append(std::move(expr));
            return std::make_tuple(std::move(scope), 0);
        }

        scanner.next(); // linebreak
        if ( scanner.peek().kind() != TokenKind::Indent ) {
            dgn.error(myScope->module(), scanner.peek()) << "expected new scope for procedure definition";
            dgn.die();
        }

        auto nextIndent = scanner.next();
        auto nextIndentWidth = nextIndent.lexeme().size();
        switch ( indentChange(nextIndentWidth) ) {
        case Same:
        case Decrease:
            dgn.error(myScope->module(), nextIndent) << "expected new scope for procedure definition", dgn.die();
        case Increase:
            return make_tuple(std::move(scope), nextIndentWidth);
        }

        throw std::runtime_error("error parsing procedure definition");
    }

    return std::make_tuple(nullptr, 0);
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
        std::unique_ptr<ast::DeclarationScope> defn;
        lexer::indent_width_t indent;
        std::tie(defn, indent) = parseTypeDefinition(dgn, scanner, *typeDecl);
        if ( defn )
            typeDecl->define(std::move(defn));

        std::unique_ptr<DeclarationScopeParser> newScopeParser;
        if ( indent )
            newScopeParser = std::make_unique<DeclarationScopeParser>(typeDecl->definition(), indent);

        myScope->append(std::move(typeDecl));

        return std::make_tuple(true, std::move(newScopeParser));
    }
    else if ( auto procDecl = parseProcedureDeclaration(scanner) ) {
        std::unique_ptr<ast::ProcedureScope> defn;
        lexer::indent_width_t indent;
        std::tie(defn, indent) = parseProcedureDefinition(dgn, scanner, *procDecl);
        if ( defn )
            procDecl->define(std::move(defn));

        std::unique_ptr<DeclarationScopeParser> newScopeParser;
        if ( indent )
            newScopeParser = std::make_unique<ProcedureScopeParser>(procDecl->definition(), indent);

        myScope->append(std::move(procDecl));

        return std::make_tuple(true, std::move(newScopeParser));
    }

    return std::make_tuple(false, nullptr);
}

std::unique_ptr<DeclarationScopeParser> DeclarationScopeParser::next(Diagnostics& dgn, lexer::Scanner& scanner)
{
    if ( scanner )
        discardEmptyLines(scanner);

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

        switch ( scanner.peek().kind() ) {
        case TokenKind::EndOfFile:
            break;

        case TokenKind::LineBreak:
        {
            scanner.next();
            discardEmptyLines(scanner);

            lexer::indent_width_t nextIndentWidth = 0;
            if ( scanner.peek().kind() == TokenKind::Indent )
                nextIndentWidth = scanner.next().lexeme().size();

            switch ( indentChange(nextIndentWidth) ) {
            case Same:
                break;
            case Increase:
                dgn.error(myScope->module(), scanner.peek()) << "unexpected scope opening", dgn.die();
            case Decrease:
                return nullptr;
            }

            break;
        }

        default:
            dgn.error(myScope->module(), scanner.peek()) << "expected end of line", dgn.die();
        }
    }

    return nullptr;
}

lexer::indent_width_t DeclarationScopeParser::indent() const
{
    return myIndent;
}

//
// ProcedureScopeParser

ProcedureScopeParser::ProcedureScopeParser(ast::ProcedureScope* scope,
                                           lexer::indent_width_t indent)
    : DeclarationScopeParser(scope, indent)
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
