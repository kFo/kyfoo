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

std::unique_ptr<DataSumScopeParser>
DeclarationScopeParser::parseDataSumDefinition(Diagnostics& /*dgn*/,
                                               lexer::Scanner& scanner,
                                               ast::DataSumDeclaration& declaration)
{
    // Check if type definition follows
    if ( scanner.peek().kind() != TokenKind::IndentGT )
        return nullptr;

    scanner.next();

    auto dsDefn = std::make_unique<ast::DataSumScope>(*myScope, declaration);
    declaration.define(dsDefn.get());
    scope()->append(std::move(dsDefn));
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

    auto dpDefn = std::make_unique<ast::DataProductScope>(*myScope, declaration);
    declaration.define(dpDefn.get());
    scope()->append(std::move(dpDefn));
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
        auto p = std::make_unique<ast::ProcedureScope>(*myScope, declaration);
        declaration.define(p.get());
        scope()->append(std::move(p));
        if ( !isIndent(scanner.peek().kind()) ) {
            auto expr = parse<Expression>(scanner);
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

std::tuple<ast::Symbol, std::unique_ptr<ast::ProcedureDeclaration>>
parseImplicitTemplateProcedureDeclaration(lexer::Scanner& scanner)
{
    ImplicitProcedureTemplateDeclaration grammar;
    if ( parse(scanner, grammar) )
        return grammar.make();

    return std::make_tuple(ast::Symbol(lexer::Token(lexer::TokenKind::Identifier, 0, 0, "")), nullptr);
}

std::tuple<bool, std::unique_ptr<DeclarationScopeParser>>
DeclarationScopeParser::parseNonProcedural(Diagnostics& dgn, lexer::Scanner& scanner)
{
    if ( auto importDecl = parse<ImportDeclaration>(scanner) ) {
        append(std::move(importDecl));
        return std::make_tuple(true, nullptr);
    }
    else if ( auto symDecl = parse<SymbolDeclaration>(scanner) ) {
        append(std::move(symDecl));
        return std::make_tuple(true, nullptr);
    }
    else if ( auto dsDecl = parse<DataSumDeclaration>(scanner) ) {
        auto newScopeParser = parseDataSumDefinition(dgn, scanner, *dsDecl);
        append(std::move(dsDecl));

        return std::make_tuple(true, std::move(newScopeParser));
    }
    else if ( auto dpDecl = parse<DataProductDeclaration>(scanner) ) {
        auto newScopeParser = parseDataProductDefinition(dgn, scanner, *dpDecl);
        append(std::move(dpDecl));

        return std::make_tuple(true, std::move(newScopeParser));
    }

    return std::make_tuple(false, nullptr);
}

std::tuple<bool, std::unique_ptr<DeclarationScopeParser>>
DeclarationScopeParser::parseProcedural(Diagnostics& dgn, lexer::Scanner& scanner)
{
    if ( auto procDecl = parse<ProcedureDeclaration>(scanner) ) {
        auto newScopeParser = parseProcedureDefinition(dgn, scanner, *procDecl);
        append(std::move(procDecl));

        return std::make_tuple(true, std::move(newScopeParser));
    }

    auto templProcDecl = parseImplicitTemplateProcedureDeclaration(scanner);
    if ( std::get<1>(templProcDecl) ) {
        auto templDecl = std::make_unique<ast::TemplateDeclaration>(std::move(std::get<0>(templProcDecl)));
        auto templDefn = std::make_unique<ast::TemplateScope>(*myScope, *templDecl);
        templDecl->define(templDefn.get());
        scope()->append(std::move(templDefn));

        auto newScopeParser = parseProcedureDefinition(dgn, scanner, *std::get<1>(templProcDecl));
        templDecl->definition()->append(std::move(std::get<1>(templProcDecl)));

        append(std::move(templDecl));
        return std::make_tuple(true, std::move(newScopeParser));
    }

    return std::make_tuple(false, nullptr);
}

void DeclarationScopeParser::append(std::unique_ptr<ast::Declaration> decl)
{
    decl->setAttributes(std::move(myAttributes));
    myScope->append(std::move(decl));
}

void DeclarationScopeParser::parseAttributes(Diagnostics& dgn, lexer::Scanner& scanner)
{
    while ( auto attr = parse<Attribute>(scanner) ) {
        myAttributes.emplace_back(std::move(attr));
        if ( scanner.peek().kind() != lexer::TokenKind::IndentEQ ) {
            dgn.error(myScope->module(), scanner.peek()) << "expected declaration to follow attribute";
            return;
        }
        scanner.next();
    }
}

std::tuple<bool, std::unique_ptr<DeclarationScopeParser>>
DeclarationScopeParser::parseNext(Diagnostics& dgn, lexer::Scanner& scanner)
{
    parseAttributes(dgn, scanner);
    if ( dgn.errorCount() )
        return std::make_tuple(false, nullptr);

    auto ret = parseNonProcedural(dgn, scanner);
    if ( std::get<0>(ret) )
        return ret;

    return parseProcedural(dgn, scanner);
}

ast::DeclarationScope* DeclarationScopeParser::scope()
{
    return myScope;
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
    if ( auto dsCtor = parse<DataSumConstructor>(scanner) ) {
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
    if ( auto field = parse<DataProductDeclarationField>(scanner) ) {
        field->setParent(scope()->declaration()->as<ast::DataProductDeclaration>());
        myScope->append(std::move(field));
        return std::make_tuple(true, nullptr);
    }
    else if ( auto dsDecl = parse<DataSumDeclaration>(scanner) ) {
        auto newScopeParser = parseDataSumDefinition(dgn, scanner, *dsDecl);
        myScope->append(std::move(dsDecl));

        return std::make_tuple(true, std::move(newScopeParser));
    }

    return DeclarationScopeParser::parseNext(dgn, scanner);
}

ast::DataProductScope* DataProductScopeParser::scope()
{
    return static_cast<ast::DataProductScope*>(myScope);
}

//
// ProcedureScopeParser

ProcedureScopeParser::ProcedureScopeParser(ast::ProcedureScope* scope)
    : ProcedureScopeParser(scope, false)
{
}

ProcedureScopeParser::ProcedureScopeParser(ast::ProcedureScope* scope,
                                           bool isLoop)
    : DeclarationScopeParser(scope)
    , myIsLoop(isLoop)
{
}

ProcedureScopeParser::~ProcedureScopeParser()
{
    auto bb = scope()->basicBlocks().back();
    if ( !bb->junction() ) {
        if ( myIsLoop )
            bb->setJunction(std::make_unique<ast::JumpJunction>(ast::JumpJunction::JumpKind::Loop, scope()->basicBlocks().front()));
        else if ( auto m = scope()->mergeBlock() )
            bb->setJunction(std::make_unique<ast::JumpJunction>(ast::JumpJunction::JumpKind::Break, m));
    }
}

std::tuple<bool, std::unique_ptr<DeclarationScopeParser>>
ProcedureScopeParser::parseNext(Diagnostics& dgn, lexer::Scanner& scanner)
{
    // Allow declarations
    {
        auto declParse = DeclarationScopeParser::parseNonProcedural(dgn, scanner);
        if ( std::get<0>(declParse) )
            return declParse;
    }

    {
        BlockDeclaration grammar;
        if ( parse(scanner, grammar) ) {
            auto bdecl = grammar.make();
            auto b = scope()->basicBlocks().back();
            auto m = scope()->createBasicBlock();
            auto s = scope()->createChildScope(m, bdecl.open, bdecl.id);
            b->setJunction(std::make_unique<ast::JumpJunction>(
                ast::JumpJunction::JumpKind::Break, s->basicBlocks().front()));

            bool isLoop = false;
            if ( bdecl.expr ) {
                isLoop = true;
                auto br = std::make_unique<ast::BranchJunction>(bdecl.open, std::move(bdecl.expr));
                br->setBranch(0, s->createBasicBlock());
                br->setBranch(1, m);
                s->basicBlocks().front()->setJunction(std::move(br));
            }

            if ( scanner.peek().kind() == lexer::TokenKind::IndentGT ) {
                scanner.next();
                return std::make_tuple(true, std::make_unique<ProcedureScopeParser>(s, isLoop));
            }

            return std::make_tuple(false, nullptr);
        }
    }

    auto lastBranch = [s = scope(), &dgn](ast::Junction& j) -> ast::BranchJunction* {
        if ( s->basicBlocks().size() >= 2 ) {
            auto lastJunc = s->basicBlocks()[s->basicBlocks().size() - 2]->junction();
            if ( lastJunc ) {
                auto ret = lastJunc->as<ast::BranchJunction>();
                if ( ret )
                    return ret;
            }
        }

        dgn.error(s->module(), j) << "expected preceding branch-statement";
        return nullptr;
    };

    if ( auto elseJunc = parse<BranchElseJunction>(scanner) ) {
        auto br = lastBranch(*elseJunc);
        if ( !br )
            return std::make_tuple(false, nullptr);

        // todo: lifetime of transient parse objects in diagnostics
        if ( !br->branch(0) ) {
            dgn.error(scope()->module(), *elseJunc) << "else-branch-statement must proceed a branch-statement";
            return std::make_tuple(false, nullptr);
        }

        while ( br->branch(1) ) {
            if ( !br->branch(1)->junction() ) {
                dgn.error(scope()->module(), *elseJunc) << "is missing preceding branch-statement";
                return std::make_tuple(false, nullptr);
            }

            br = br->branch(1)->junction()->as<ast::BranchJunction>();
            if ( !br || !br->branch(0) ) {
                dgn.error(scope()->module(), *elseJunc) << "else-branch-statement must proceed a branch-statement";
                return std::make_tuple(false, nullptr);
            }
        }

        if ( br->branch(1) ) {
            dgn.error(scope()->module(), *elseJunc) << "preceding branch-statement already has an else-branch-statement";
            return std::make_tuple(false, nullptr);
        }

        if ( scanner.peek().kind() == lexer::TokenKind::IndentGT ) {
            scanner.next();
            auto m = scope()->basicBlocks().back();
            auto s = scope()->createChildScope(m);
            br->setBranch(1, s->basicBlocks().front());
            if ( elseJunc->condition() ) {
                auto ss = s->createChildScope(m);
                elseJunc->setBranch(0, ss->basicBlocks().front());
                s->basicBlocks().front()->setJunction(std::move(elseJunc));
                return std::make_tuple(true, std::make_unique<ProcedureScopeParser>(ss));
            }

            return std::make_tuple(true, std::make_unique<ProcedureScopeParser>(s));
        }

        return std::make_tuple(false, nullptr);
    }

    if ( auto branchJunc = parse<BranchJunction>(scanner) ) {
        auto br = branchJunc.get();
        scope()->basicBlocks().back()->setJunction(std::move(branchJunc));

        if ( scanner.peek().kind() == lexer::TokenKind::IndentGT ) {
            scanner.next();
            auto m = scope()->createBasicBlock();
            auto s = scope()->createChildScope(m);
            br->setBranch(0, s->basicBlocks().front());
            return std::make_tuple(true, std::make_unique<ProcedureScopeParser>(s));
        }

        return std::make_tuple(false, nullptr);
    }
    else if ( auto retJunc = parse<ReturnJunction>(scanner) ) {
        auto b = scope()->basicBlocks().back();
        if ( b->junction() ) {
            dgn.error(scope()->module(), *retJunc) << "statement is unreachable";
            return std::make_tuple(false, nullptr);
        }
        b->setJunction(std::move(retJunc));
        return std::make_tuple(true, nullptr);
    }
    else if ( auto jmpJunc = parse<JumpJunction>(scanner) ) {
        auto b = scope()->basicBlocks().back();
        if ( b->junction() ) {
            dgn.error(scope()->module(), *jmpJunc) << "statement is unreachable";
            return std::make_tuple(false, nullptr);
        }
        b->setJunction(std::move(jmpJunc));
        return std::make_tuple(true, nullptr);
    }

    {
        VariableDeclaration varGrammar;
        if ( parse(scanner, varGrammar) ) {
            auto v = varGrammar.make();
            auto var = std::make_unique<ast::VariableDeclaration>(ast::Symbol(v.token), *scope(), std::move(v.constraint));
            auto p = std::make_unique<ast::PrimaryExpression>(v.token);
            p->setDeclaration(*var);
            static_cast<ast::DeclarationScope*>(scope())->append(std::move(var));
            auto expr = std::make_unique<ast::VarExpression>(std::move(p), std::move(v.expression));
            scope()->appendConstruction(std::move(expr));
            return std::make_tuple(true, nullptr);
        }
        else if ( auto expr = parse<Expression>(scanner) ) {
            scope()->append(std::move(expr));
            return std::make_tuple(true, nullptr);
        }
    }

    return std::make_tuple(false, nullptr);
}

ast::ProcedureScope* ProcedureScopeParser::scope()
{
    return static_cast<ast::ProcedureScope*>(myScope);
}

    } // namespace parser
} // namespace kyfoo
