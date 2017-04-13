#include <kyfoo/parser/Parse.hpp>

#include <kyfoo/lexer/Token.hpp>
#include <kyfoo/ast/Module.hpp>
#include <kyfoo/ast/Scopes.hpp>

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

DeclarationScopeParser::DeclarationScopeParser(ast::DeclarationScope* scope, lexer::indent_width_t indent)
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

std::unique_ptr<ast::Expression> parseExpression(lexer::Scanner& scanner)
{
    Compound grammar;
    if ( parse(scanner, grammar) )
        return grammar.make();

    return nullptr;
}

std::tuple<std::unique_ptr<ast::ProcedureScope>, lexer::indent_width_t>
DeclarationScopeParser::parseProcedureDefinition(lexer::Scanner& scanner)
{
    // Check if a procedure definition follows
    if ( scanner.peek().kind() == TokenKind::Yield ) {
        scanner.next(); // yield
        auto scope = std::make_unique<ast::ProcedureScope>(myScope);
        if ( scanner.peek().kind() != TokenKind::LineBreak ) {
            auto expr = parseExpression(scanner);
            if ( !expr )
                throw Error(scanner.peek()) << "expected expression following procedure declaration";

            scope->append(std::move(expr));
            return std::make_tuple(std::move(scope), 0);
        }

        scanner.next(); // linebreak
        if ( scanner.peek().kind() != TokenKind::Indent )
            throw Error(scanner.peek()) << "expected new scope for procedure definition";

        auto nextIndent = scanner.next();
        auto nextIndentWidth = nextIndent.lexeme().size();
        switch ( indentChange(nextIndentWidth) ) {
        case Same:
        case Decrease:
            throw Error(nextIndent) << "expected new scope for procedure definition";
        case Increase:
            return make_tuple(std::move(scope), nextIndentWidth);
        }

        throw std::runtime_error("error parsing procedure definition");
    }

    return std::make_tuple(nullptr, 0);
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
DeclarationScopeParser::parseNext(lexer::Scanner& scanner)
{
    if ( auto importDecl = parseImportDeclaration(scanner) ) {
        myScope->append(std::move(importDecl));
        return std::make_tuple(true, nullptr);
    }
    else if ( auto symDecl = parseSymbolDeclaration(scanner) ) {
        myScope->append(std::move(symDecl));
        return std::make_tuple(true, nullptr);
    }
    else if ( auto procDecl = parseProcedureDeclaration(scanner) ) {
        std::unique_ptr<ast::ProcedureScope> defn;
        lexer::indent_width_t indent;
        std::tie(defn, indent) = parseProcedureDefinition(scanner);
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

std::unique_ptr<DeclarationScopeParser> DeclarationScopeParser::next(lexer::Scanner& scanner)
{
    if ( scanner )
        discardEmptyLines(scanner);

    while ( scanner ) {
        bool success;
        std::unique_ptr<DeclarationScopeParser> newScopeParser;
        std::tie(success, newScopeParser) = parseNext(scanner);

        if ( !success )
            throw Error(scanner.peek()) << "grammar at this point is not recognized";
        
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
                throw Error(scanner.peek()) << "unexpected scope opening";
            case Decrease:
                return nullptr;
            }

            break;
        }

        default:
            throw Error(scanner.peek()) << "expected end of line";
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
ProcedureScopeParser::parseNext(lexer::Scanner& scanner)
{
    // Allow declarations
    {
        auto declParse = DeclarationScopeParser::parseNext(scanner);
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

std::unique_ptr<ast::Module> parseModule(const char* moduleName,
                                         lexer::Scanner& scanner)
{
    using lexer::TokenKind;

    auto moduleScope = std::make_unique<ast::DeclarationScope>(nullptr);
    
    std::vector<std::unique_ptr<DeclarationScopeParser>> scopeStack;
    scopeStack.emplace_back(std::make_unique<DeclarationScopeParser>(moduleScope.get(), 0));

    while ( scanner ) {
        auto nextScope = scopeStack.back()->next(scanner);
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

            if ( scopeStack.empty() )
                throw Error(scanner.peek()) << "indentation doesn't match an existing scope";
        }
    }

    return std::make_unique<ast::Module>(moduleName, std::move(moduleScope));
}

    } // namespace parser
} // namespace kyfoo
