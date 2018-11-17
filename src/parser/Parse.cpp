#include <kyfoo/parser/Parse.hpp>

#include <filesystem>

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
                                               ast::Scope& scope)
    : myDiagnostics(&dgn)
    , myScanner(&scanner)
    , myScope(&scope)
{
}

DeclarationScopeParser::~DeclarationScopeParser() = default;

Box<DataSumScopeParser>
DeclarationScopeParser::parseDataSumDefinition(ast::DataSumDeclaration& declaration)
{
    // Check if type definition follows
    if ( scanner().peek().kind() != lexer::TokenKind::IndentGT )
        return nullptr;

    scanner().next();

    auto dsDefn = mk<ast::DataSumScope>(*myScope, declaration);
    scope().append(std::move(dsDefn));
    return mk<DataSumScopeParser>(diagnostics(), scanner(), *declaration.definition());
}

Box<DataProductScopeParser>
DeclarationScopeParser::parseDataProductDefinition(ast::DataProductDeclaration& declaration)
{
    // Check if type definition follows
    if ( scanner().peek().kind() != lexer::TokenKind::IndentGT )
        return nullptr;

    scanner().next();

    auto dpDefn = mk<ast::DataProductScope>(*myScope, declaration);
    scope().append(std::move(dpDefn));
    return mk<DataProductScopeParser>(diagnostics(), scanner(), *declaration.definition());
}

Box<ProcedureScopeParser>
DeclarationScopeParser::parseProcedureDefinition(ast::ProcedureDeclaration& declaration)
{
    // Check if a procedure definition follows
    if ( scanner().peek().kind() == lexer::TokenKind::Yield ) {
        scanner().next(); // yield
        auto p = mk<ast::ProcedureScope>(*myScope, declaration);
        declaration.scope().append(std::move(p));
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

        return mk<ProcedureScopeParser>(diagnostics(), scanner(), *declaration.definition());
    }

    return nullptr;
}

ImplicitProcDecl
parseImplicitTemplateProcedureDeclaration(DeclarationScopeParser& parser)
{
    ImplicitProcedureTemplateDeclaration grammar;
    if ( parse(parser.scanner(), grammar) )
        return grammar.make(parser);

    return { ast::Symbol(lexer::Token(lexer::TokenKind::Identifier, "", lexer::SourceLocation())), nullptr };
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
        auto p = dsDecl.get();
        append(std::move(dsDecl));
        auto newScopeParser = parseDataSumDefinition(*p);

        return {true, std::move(newScopeParser)};
    }
    else if ( auto dpDecl = parse<DataProductDeclaration>(*this) ) {
        auto p = dpDecl.get();
        append(std::move(dpDecl));
        auto newScopeParser = parseDataProductDefinition(*p);

        return {true, std::move(newScopeParser)};
    }

    return {false, nullptr};
}

DeclarationScopeParser::ParseResult
DeclarationScopeParser::parseProcedural()
{
    if ( auto procDecl = parse<ProcedureDeclaration>(*this) ) {
        auto p = procDecl.get();
        append(std::move(procDecl));
        auto newScopeParser = parseProcedureDefinition(*p);

        return {true, std::move(newScopeParser)};
    }

    auto templProcDecl = parseImplicitTemplateProcedureDeclaration(*this);
    if ( templProcDecl.proc ) {
        templProcDecl.proc->setAttributes(std::move(myAttributes));
        auto templDecl = mk<ast::TemplateDeclaration>(std::move(templProcDecl.templSym));
        auto templDefn = mk<ast::TemplateScope>(*myScope, *templDecl);
        scope().append(std::move(templDefn));

        auto p = templProcDecl.proc.get();
        templDecl->definition()->append(std::move(templProcDecl.proc));
        auto newScopeParser = parseProcedureDefinition(*p);

        append(std::move(templDecl));
        return {true, std::move(newScopeParser)};
    }

    return {false, nullptr};
}

std::vector<Box<ast::Expression>> DeclarationScopeParser::parameterContext() const
{
    return ast::clone(myParameterContext);
}

void DeclarationScopeParser::append(Box<ast::Declaration> decl)
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

ast::Scope& DeclarationScopeParser::scope()
{
    return *myScope;
}

ast::Scope const& DeclarationScopeParser::scope() const
{
    return *myScope;
}

Box<DeclarationScopeParser> DeclarationScopeParser::next()
{
    while ( scanner() ) {
        auto [success, newScopeParser] = parseNext();

        if ( !success ) {
            diagnostics().error(myScope->module(), scanner().peek()) << "grammar at this point is not recognized";
            diagnostics().die();
        }
        
        if ( newScopeParser )
            return move(newScopeParser);

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
    if ( auto [decl, defn] = parse<DataSumConstructor>(*this); decl ) {
        myScope->append(std::move(decl));
        if ( defn )
            myScope->append(std::move(defn));
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
        auto p = dsDecl.get();
        myScope->append(std::move(dsDecl));
        auto newScopeParser = parseDataSumDefinition(*p);

        return {true, std::move(newScopeParser)};
    }

    return DeclarationScopeParser::parseNext();
}

//
// ProcedureScopeParser

ProcedureScopeParser::ProcedureScopeParser(Diagnostics& dgn,
                                           lexer::Scanner& scanner,
                                           ast::ProcedureScope& scope)
    : ProcedureScopeParser(dgn, scanner, scope, nullptr)
{
}

ProcedureScopeParser::ProcedureScopeParser(Diagnostics& dgn,
                                           lexer::Scanner& scanner,
                                           ast::ProcedureScope& scope,
                                           ast::BasicBlock* loopBlock)
    : DeclarationScopeParser(dgn, scanner, scope)
    , myLoopBlock(loopBlock)
{
}

ProcedureScopeParser::~ProcedureScopeParser()
{
    auto bb = scope().basicBlocks().back();
    if ( !bb->junction() ) {
        if ( myLoopBlock )
            bb->setJunction(mk<ast::JumpJunction>(myLoopBlock));
        else if ( auto m = scope().mergeBlock() )
            bb->setJunction(mk<ast::JumpJunction>(m));
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

    if ( BlockDeclaration grammar; parse(scanner(), grammar) ) {
        auto bdecl = grammar.make(*this);
        auto b = scope().basicBlocks().back();
        auto m = scope().createBasicBlock();
        auto s = scope().createChildScope(m, bdecl.open, bdecl.label);

        if ( bdecl.expr ) {
            auto br = mk<ast::BranchJunction>(bdecl.open, bdecl.label, std::move(bdecl.expr));
            br->setBranch(0, s->basicBlocks().front());
            br->setBranch(1, m);
            b->setJunction(std::move(br));
        }
        else {
            b->setJunction(mk<ast::JumpJunction>(s->basicBlocks().front()));
        }

        if ( scanner().peek().kind() == lexer::TokenKind::IndentGT ) {
            scanner().next();
            return {true, mk<ProcedureScopeParser>(diagnostics(), scanner(), *s)};
        }

        return {false, nullptr};
    }

    auto lastBranch = [this](ast::Junction& j) -> ast::BranchJunction* {
        auto& s = scope();
        if ( s.basicBlocks().card() >= 2 ) {
            auto lastJunc = s.basicBlocks()[$ - 2]->junction();
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
            auto s = scope().createChildScope(m, elseJunc->token(), elseJunc->label());
            br->setBranch(1, s->basicBlocks().front());
            if ( elseJunc->condition() ) {
                auto ss = s->createChildScope(m, lexer::Token(), lexer::Token());
                elseJunc->setBranch(0, ss->basicBlocks().front());
                s->basicBlocks().front()->setJunction(std::move(elseJunc));
                return {true, mk<ProcedureScopeParser>(diagnostics(), scanner(), *ss)};
            }

            return {true, mk<ProcedureScopeParser>(diagnostics(), scanner(), *s)};
        }

        return {false, nullptr};
    }

    if ( auto branchJunc = parse<BranchJunction>(*this) ) {
        auto br = branchJunc.get();
        auto s = &scope();
        auto b = s->basicBlocks().back();
        auto m = s->createBasicBlock();

        if ( branchJunc->label().lexeme() ) {
            s = s->createChildScope(m, branchJunc->token(), branchJunc->label());
            b->setJunction(mk<ast::JumpJunction>(s->basicBlocks().front()));
            b = s->basicBlocks().front();
        }

        b->setJunction(std::move(branchJunc));

        if ( scanner().peek().kind() == lexer::TokenKind::IndentGT ) {
            scanner().next();
            s = s->createChildScope(m, lexer::Token(), lexer::Token());
            br->setBranch(0, s->basicBlocks().front());
            return {true, mk<ProcedureScopeParser>(diagnostics(), scanner(), *s)};
        }

        return {false, nullptr};
    }

    if ( auto loopJunc = parse<LoopJunction>(*this) ) {
        auto br = loopJunc.get();
        auto b = scope().basicBlocks().back();

        auto m = scope().createBasicBlock();
        auto loopScope = scope().createChildScope(m, loopJunc->token(), loopJunc->label());
        b->setJunction(mk<ast::JumpJunction>(loopScope->basicBlocks().front()));
        b = loopScope->basicBlocks().front();

        b->setJunction(std::move(loopJunc));

        auto s = loopScope->createChildScope(m, lexer::Token(), lexer::Token());
        br->setBranch(0, s->basicBlocks().front());

        if ( scanner().peek().kind() == lexer::TokenKind::IndentGT ) {
            scanner().next();
            return {true, mk<ProcedureScopeParser>(diagnostics(), scanner(), *s, loopScope->basicBlocks().front())};
        }
        else if ( scanner().peek().kind() == lexer::TokenKind::IndentEQ
               || scanner().peek().kind() == lexer::TokenKind::IndentLT )
        {
            s->basicBlocks().front()->setJunction(mk<ast::JumpJunction>(loopScope->basicBlocks().front()));
            return {true, nullptr};
        }

        return {false, nullptr};
    }

    if ( auto retJunc = parse<ReturnJunction>(*this) ) {
        auto b = scope().basicBlocks().back();
        if ( b->junction() ) {
            diagnostics().error(scope().module(), *retJunc) << "statement is unreachable";
            return {false, nullptr};
        }
        b->setJunction(std::move(retJunc));
        return {true, nullptr};
    }

    if ( auto jmpJunc = parse<JumpJunction>(*this) ) {
        auto b = scope().basicBlocks().back();
        if ( b->junction() ) {
            diagnostics().error(scope().module(), *jmpJunc) << "statement is unreachable";
            return {false, nullptr};
        }
        b->setJunction(std::move(jmpJunc));
        return {true, nullptr};
    }

    if ( VariableDeclaration varGrammar; parse(scanner(), varGrammar) ) {
        auto v = varGrammar.make(*this);
        auto var = mk<ast::VariableDeclaration>(ast::Symbol(v.token),
                                                scope(),
                                                std::move(v.constraints));
        scope().append(std::move(var), std::move(v.initializer));
        return {true, nullptr};
    }

    if ( auto expr = parse<Expression>(*this) ) {
        scope().append(std::move(expr));
        return {true, nullptr};
    }

    return {false, nullptr};
}

void parseScope(Box<DeclarationScopeParser> parser)
{
    auto& dgn = parser->diagnostics();
    auto& scanner = parser->scanner();
    auto const& mod = parser->scope().module();

    std::vector<Box<parser::DeclarationScopeParser>> scopeStack;
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
            case lexer::TokenKind::EndOfInput:
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

    ENFORCE(scopeStack.size() == 1, "parser scope imbalance");
}

} // namespace kyfoo::parser
