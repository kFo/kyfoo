#include <kyfoo/parser/Parse.hpp>

#include <filesystem>
#include <fstream>

#include <kyfoo/Diagnostics.hpp>

#include <kyfoo/lexer/Scanner.hpp>
#include <kyfoo/lexer/Token.hpp>

#include <kyfoo/parser/Parse.hpp>
#include <kyfoo/parser/Productions.hpp>

#include <kyfoo/ast/Declarations.hpp>
#include <kyfoo/ast/Expressions.hpp>
#include <kyfoo/ast/Module.hpp>
#include <kyfoo/ast/Scopes.hpp>

namespace kyfoo::parser {

//
// DeclarationScopeParser

DeclarationScopeParser::DeclarationScopeParser(Diagnostics& dgn,
                                               lexer::Scanner& scanner,
                                               ast::DeclarationScope& scope)
    : myDiagnostics(&dgn)
    , myScanner(&scanner)
    , myScope(&scope)
{
}

DeclarationScopeParser::~DeclarationScopeParser() = default;

std::unique_ptr<DataSumScopeParser>
DeclarationScopeParser::parseDataSumDefinition(ast::DataSumDeclaration& declaration)
{
    // Check if type definition follows
    if ( scanner().peek().kind() != lexer::TokenKind::IndentGT )
        return nullptr;

    scanner().next();

    auto dsDefn = std::make_unique<ast::DataSumScope>(*myScope, declaration);
    declaration.define(*dsDefn);
    scope().append(std::move(dsDefn));
    return std::make_unique<DataSumScopeParser>(diagnostics(), scanner(), *declaration.definition());
}

std::unique_ptr<DataProductScopeParser>
DeclarationScopeParser::parseDataProductDefinition(ast::DataProductDeclaration& declaration)
{
    // Check if type definition follows
    if ( scanner().peek().kind() != lexer::TokenKind::IndentGT )
        return nullptr;

    scanner().next();

    auto dpDefn = std::make_unique<ast::DataProductScope>(*myScope, declaration);
    declaration.define(*dpDefn);
    scope().append(std::move(dpDefn));
    return std::make_unique<DataProductScopeParser>(diagnostics(), scanner(), *declaration.definition());
}

std::unique_ptr<ProcedureScopeParser>
DeclarationScopeParser::parseProcedureDefinition(ast::ProcedureDeclaration& declaration)
{
    // Check if a procedure definition follows
    if ( scanner().peek().kind() == lexer::TokenKind::Yield ) {
        scanner().next(); // yield
        auto p = std::make_unique<ast::ProcedureScope>(*myScope, declaration);
        declaration.define(*p);
        scope().append(std::move(p));
        if ( !isIndent(scanner().peek().kind()) ) {
            auto expr = parse<Expression>(*this);
            if ( !expr ) {
                diagnostics().error(myScope->module(), scanner().peek()) << "expected expression following procedure declaration";
                diagnostics().die();
            }

            declaration.definition()->append(std::move(expr));
            return nullptr;
        }

        auto indent = scanner().next();
        if ( indent.kind() != lexer::TokenKind::IndentGT ) {
            diagnostics().error(myScope->module(), scanner().peek()) << "expected new scope for procedure definition";
            diagnostics().die();
        }

        return std::make_unique<ProcedureScopeParser>(diagnostics(), scanner(), *declaration.definition());
    }

    return nullptr;
}

std::tuple<ast::Symbol, std::unique_ptr<ast::ProcedureDeclaration>>
parseImplicitTemplateProcedureDeclaration(DeclarationScopeParser& parser)
{
    ImplicitProcedureTemplateDeclaration grammar;
    if ( parse(parser.scanner(), grammar) )
        return grammar.make(parser);

    return {ast::Symbol(lexer::Token(lexer::TokenKind::Identifier, 0, 0, "")), nullptr};
}

DeclarationScopeParser::ParseResult
DeclarationScopeParser::parseNonProcedural()
{
    if ( auto importDecl = parse<ImportDeclaration>(*this) ) {
        append(std::move(importDecl));
        return {true, nullptr};
    }
    else if ( auto symDecl = parse<SymbolDeclaration>(*this) ) {
        append(std::move(symDecl));
        return {true, nullptr};
    }
    else if ( auto dsDecl = parse<DataSumDeclaration>(*this) ) {
        auto newScopeParser = parseDataSumDefinition(*dsDecl);
        append(std::move(dsDecl));

        return {true, std::move(newScopeParser)};
    }
    else if ( auto dpDecl = parse<DataProductDeclaration>(*this) ) {
        auto newScopeParser = parseDataProductDefinition(*dpDecl);
        append(std::move(dpDecl));

        return {true, std::move(newScopeParser)};
    }

    return {false, nullptr};
}

DeclarationScopeParser::ParseResult
DeclarationScopeParser::parseProcedural()
{
    if ( auto procDecl = parse<ProcedureDeclaration>(*this) ) {
        auto newScopeParser = parseProcedureDefinition(*procDecl);
        append(std::move(procDecl));

        return {true, std::move(newScopeParser)};
    }

    auto templProcDecl = parseImplicitTemplateProcedureDeclaration(*this);
    if ( std::get<1>(templProcDecl) ) {
        std::get<1>(templProcDecl)->setAttributes(std::move(myAttributes));
        auto templDecl = std::make_unique<ast::TemplateDeclaration>(std::move(std::get<0>(templProcDecl)));
        auto templDefn = std::make_unique<ast::TemplateScope>(*myScope, *templDecl);
        templDecl->define(*templDefn);
        scope().append(std::move(templDefn));

        auto newScopeParser = parseProcedureDefinition(*std::get<1>(templProcDecl));
        templDecl->definition()->append(std::move(std::get<1>(templProcDecl)));

        append(std::move(templDecl));
        return {true, std::move(newScopeParser)};
    }

    return {false, nullptr};
}

std::vector<std::unique_ptr<ast::Expression>> DeclarationScopeParser::parameterContext() const
{
    return ast::clone(myParameterContext);
}

void DeclarationScopeParser::append(std::unique_ptr<ast::Declaration> decl)
{
    decl->setAttributes(std::move(myAttributes));
    myScope->append(std::move(decl));
}

void DeclarationScopeParser::parseAttributes()
{
    while ( auto attr = parse<Attribute>(*this) ) {
        myAttributes.emplace_back(std::move(attr));
        if ( scanner().peek().kind() != lexer::TokenKind::IndentEQ ) {
            diagnostics().error(myScope->module(), scanner().peek()) << "expected declaration to follow attribute";
            return;
        }
        scanner().next();
    }
}

DeclarationScopeParser::ParseResult
DeclarationScopeParser::parseNext()
{
    parseAttributes();
    if ( diagnostics().errorCount() )
        return {false, nullptr};

    auto ret = parseNonProcedural();
    if ( ret.success )
        return ret;

    return parseProcedural();
}

Diagnostics& DeclarationScopeParser::diagnostics()
{
    return *myDiagnostics;
}

Diagnostics const& DeclarationScopeParser::diagnostics() const
{
    return *myDiagnostics;
}

lexer::Scanner& DeclarationScopeParser::scanner()
{
    return *myScanner;
}

lexer::Scanner const& DeclarationScopeParser::scanner() const
{
    return *myScanner;
}

ast::DeclarationScope& DeclarationScopeParser::scope()
{
    return *myScope;
}

ast::DeclarationScope const& DeclarationScopeParser::scope() const
{
    return *myScope;
}

std::unique_ptr<DeclarationScopeParser> DeclarationScopeParser::next()
{
    while ( scanner() ) {
        auto [success, newScopeParser] = parseNext();

        if ( !success ) {
            diagnostics().error(myScope->module(), scanner().peek()) << "grammar at this point is not recognized";
            diagnostics().die();
        }
        
        if ( newScopeParser )
            return newScopeParser;

        if ( scanner().peek().kind() != lexer::TokenKind::IndentEQ )
            return nullptr;

        scanner().next();
    }

    return nullptr;
}

//
// DataSumScopeParser

DataSumScopeParser::DataSumScopeParser(Diagnostics& dgn,
                                       lexer::Scanner& scanner,
                                       ast::DataSumScope& scope)
    : DeclarationScopeParser(dgn, scanner, scope)
{
}

DataSumScopeParser::~DataSumScopeParser() = default;

DeclarationScopeParser::ParseResult
DataSumScopeParser::parseNext()
{
    if ( auto dsCtor = parse<DataSumConstructor>(*this) ) {
        dsCtor->setParent(scope().declaration()->as<ast::DataSumDeclaration>());
        myScope->append(std::move(dsCtor));
        return {true, nullptr};
    }

    return {false, nullptr};
}

//
// DataProductScopeParser

DataProductScopeParser::DataProductScopeParser(Diagnostics& dgn,
                                               lexer::Scanner& scanner,
                                               ast::DataProductScope& scope)
    : DeclarationScopeParser(dgn, scanner, scope)
{
    std::size_t const line = 0;
    std::size_t const col = 0;
    myParameterContext.emplace_back(ast::createIdentifier(ast::makeToken("this", line, col)));
    myParameterContext.back()->addConstraint(
        createRefType(line, col,
            ast::createIdentifier(
                lexer::Token(lexer::TokenKind::Identifier, line, col, scope.declaration()->symbol().token().lexeme()),
                *scope.declaration())));
}

DataProductScopeParser::~DataProductScopeParser() = default;

DeclarationScopeParser::ParseResult
DataProductScopeParser::parseNext()
{
    if ( auto field = parse<DataProductDeclarationField>(*this) ) {
        field->setParent(scope().declaration()->as<ast::DataProductDeclaration>());
        myScope->append(std::move(field));
        return {true, nullptr};
    }
    else if ( auto dsDecl = parse<DataSumDeclaration>(*this) ) {
        auto newScopeParser = parseDataSumDefinition(*dsDecl);
        myScope->append(std::move(dsDecl));

        return {true, std::move(newScopeParser)};
    }

    return DeclarationScopeParser::parseNext();
}

//
// ProcedureScopeParser

ProcedureScopeParser::ProcedureScopeParser(Diagnostics& dgn,
                                           lexer::Scanner& scanner,
                                           ast::ProcedureScope& scope)
    : ProcedureScopeParser(dgn, scanner, scope, false)
{
}

ProcedureScopeParser::ProcedureScopeParser(Diagnostics& dgn,
                                           lexer::Scanner& scanner,
                                           ast::ProcedureScope& scope,
                                           bool isLoop)
    : DeclarationScopeParser(dgn, scanner, scope)
    , myIsLoop(isLoop)
{
}

ProcedureScopeParser::~ProcedureScopeParser()
{
    auto bb = scope().basicBlocks().back();
    if ( !bb->junction() ) {
        if ( myIsLoop )
            bb->setJunction(std::make_unique<ast::JumpJunction>(ast::JumpJunction::JumpKind::Loop, scope().basicBlocks().front()));
        else if ( auto m = scope().mergeBlock() )
            bb->setJunction(std::make_unique<ast::JumpJunction>(ast::JumpJunction::JumpKind::Break, m));
    }
}

ast::ProcedureScope& ProcedureScopeParser::scope()
{
    return static_cast<ast::ProcedureScope&>(*myScope);
}

ast::ProcedureScope const& ProcedureScopeParser::scope() const
{
    return static_cast<ast::ProcedureScope const&>(*myScope);
}

DeclarationScopeParser::ParseResult
ProcedureScopeParser::parseNext()
{
    // Allow declarations
    {
        auto declParse = DeclarationScopeParser::parseNonProcedural();
        if ( declParse.success )
            return declParse;
    }

    {
        BlockDeclaration grammar;
        if ( parse(scanner(), grammar) ) {
            auto bdecl = grammar.make(*this);
            auto b = scope().basicBlocks().back();
            auto m = scope().createBasicBlock();
            auto s = scope().createChildScope(m, bdecl.open, bdecl.id);
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

            if ( scanner().peek().kind() == lexer::TokenKind::IndentGT ) {
                scanner().next();
                return {true, std::make_unique<ProcedureScopeParser>(diagnostics(), scanner(), *s, isLoop)};
            }

            return {false, nullptr};
        }
    }

    auto lastBranch = [this](ast::Junction& j) -> ast::BranchJunction* {
        auto& s = scope();
        if ( s.basicBlocks().size() >= 2 ) {
            auto lastJunc = s.basicBlocks()[s.basicBlocks().size() - 2]->junction();
            if ( lastJunc ) {
                auto ret = lastJunc->as<ast::BranchJunction>();
                if ( ret )
                    return ret;
            }
        }

        diagnostics().error(s.module(), j) << "expected preceding branch-statement";
        return nullptr;
    };

    if ( auto elseJunc = parse<BranchElseJunction>(*this) ) {
        auto br = lastBranch(*elseJunc);
        if ( !br )
            return {false, nullptr};

        // todo: lifetime of transient parse objects in diagnostics
        if ( !br->branch(0) ) {
            diagnostics().error(scope().module(), *elseJunc) << "else-branch-statement must proceed a branch-statement";
            return {false, nullptr};
        }

        while ( br->branch(1) ) {
            if ( !br->branch(1)->junction() ) {
                diagnostics().error(scope().module(), *elseJunc) << "is missing preceding branch-statement";
                return {false, nullptr};
            }

            br = br->branch(1)->junction()->as<ast::BranchJunction>();
            if ( !br || !br->branch(0) ) {
                diagnostics().error(scope().module(), *elseJunc) << "else-branch-statement must proceed a branch-statement";
                return {false, nullptr};
            }
        }

        if ( br->branch(1) ) {
            diagnostics().error(scope().module(), *elseJunc) << "preceding branch-statement already has an else-branch-statement";
            return {false, nullptr};
        }

        if ( scanner().peek().kind() == lexer::TokenKind::IndentGT ) {
            scanner().next();
            auto m = scope().basicBlocks().back();
            auto s = scope().createChildScope(m);
            br->setBranch(1, s->basicBlocks().front());
            if ( elseJunc->condition() ) {
                auto ss = s->createChildScope(m);
                elseJunc->setBranch(0, ss->basicBlocks().front());
                s->basicBlocks().front()->setJunction(std::move(elseJunc));
                return {true, std::make_unique<ProcedureScopeParser>(diagnostics(), scanner(), *ss)};
            }

            return {true, std::make_unique<ProcedureScopeParser>(diagnostics(), scanner(), *s)};
        }

        return {false, nullptr};
    }

    if ( auto branchJunc = parse<BranchJunction>(*this) ) {
        auto br = branchJunc.get();
        scope().basicBlocks().back()->setJunction(std::move(branchJunc));

        if ( scanner().peek().kind() == lexer::TokenKind::IndentGT ) {
            scanner().next();
            auto m = scope().createBasicBlock();
            auto s = scope().createChildScope(m);
            br->setBranch(0, s->basicBlocks().front());
            return {true, std::make_unique<ProcedureScopeParser>(diagnostics(), scanner(), *s)};
        }

        return {false, nullptr};
    }
    else if ( auto retJunc = parse<ReturnJunction>(*this) ) {
        auto b = scope().basicBlocks().back();
        if ( b->junction() ) {
            diagnostics().error(scope().module(), *retJunc) << "statement is unreachable";
            return {false, nullptr};
        }
        b->setJunction(std::move(retJunc));
        return {true, nullptr};
    }
    else if ( auto jmpJunc = parse<JumpJunction>(*this) ) {
        auto b = scope().basicBlocks().back();
        if ( b->junction() ) {
            diagnostics().error(scope().module(), *jmpJunc) << "statement is unreachable";
            return {false, nullptr};
        }
        b->setJunction(std::move(jmpJunc));
        return {true, nullptr};
    }

    {
        VariableDeclaration varGrammar;
        if ( parse(scanner(), varGrammar) ) {
            auto v = varGrammar.make(*this);
            auto var = std::make_unique<ast::VariableDeclaration>(ast::Symbol(v.token),
                                                                  scope(),
                                                                  std::move(v.constraints));
            static_cast<ast::DeclarationScope&>(scope()).append(std::move(var));
            if ( v.initializer ) {
                auto p = std::make_unique<ast::IdentifierExpression>(v.token, *scope().childDeclarations().back());
                auto expr = std::make_unique<ast::AssignExpression>(std::move(p), std::move(v.initializer));
                scope().append(std::move(expr));
            }
            return {true, nullptr};
        }
        else if ( auto expr = parse<Expression>(*this) ) {
            scope().append(std::move(expr));
            return {true, nullptr};
        }
    }

    return {false, nullptr};
}

void parseScope(std::unique_ptr<DeclarationScopeParser> parser)
{
    auto& dgn = parser->diagnostics();
    auto& scanner = parser->scanner();
    auto const& mod = parser->scope().module();

    std::vector<std::unique_ptr<parser::DeclarationScopeParser>> scopeStack;
    scopeStack.emplace_back(std::move(parser));

    while ( scanner ) {
        if ( scanner.peek().kind() == lexer::TokenKind::IndentEQ )
            scanner.next();

        auto nextScope = scopeStack.back()->next();
        if ( nextScope ) {
            scopeStack.push_back(std::move(nextScope));
        }
        else {
            switch (scanner.peek().kind()) {
            case lexer::TokenKind::EndOfFile:
                if ( !scopeStack.empty() )
                    scopeStack.resize(1);

            case lexer::TokenKind::IndentLT:
                break;

            case lexer::TokenKind::IndentGT:
            {
                dgn.error(mod, scanner.peek()) << "unexpected scope opening";
                dgn.die();
                return;
            }

            default:
                dgn.error(mod, scanner.peek()) << "expected end of scope";
                dgn.die();
            }

            while ( scanner.peek().kind() == lexer::TokenKind::IndentLT ) {
                if ( scopeStack.empty() ) {
                    dgn.error(mod, scanner.peek()) << "indentation doesn't match an existing scope";
                    dgn.die();
                }

                scopeStack.pop_back();
                scanner.next();
            }
        }
    }

    if ( scanner.hasError() ) {
        dgn.error(mod, scanner.peek()) << "lexical error";
        dgn.die();
    }

    if ( scopeStack.size() != 1 )
        throw std::runtime_error("parser scope imbalance");
}

} // namespace kyfoo::parser
