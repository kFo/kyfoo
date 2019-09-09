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

Box<DataTypeScopeParser>
DeclarationScopeParser::parseDataTypeDefinition(ast::DataTypeDeclaration& declaration)
{
    // Check if type definition follows
    if ( scanner().peek().kind() != lexer::TokenKind::IndentGT )
        return nullptr;

    scanner().next();

    auto defn = mk<ast::DataTypeScope>(*myScope, declaration);
    scope().append(std::move(defn));
    return mk<DataTypeScopeParser>(diagnostics(), scanner(), *declaration.definition());
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
            auto junc = parse<ReturnJunction>(*this);
            if ( !junc ) {
                auto expr = parse<Expression>(*this);
                if ( !expr ) {
                    diagnostics().error(myScope->module(), diag::parser_expected_expression, scanner().peek());
                    diagnostics().die();
                }

                junc = mk<ast::ReturnJunction>(ast::mkToken(":.", lexer::TokenKind::ColonDot, front(*expr).location()),
                                               std::move(expr));
            }

            declaration.definition()->basicBlocks().back()->setJunction(std::move(junc));
            return nullptr;
        }

        auto indent = scanner().next();
        if ( indent.kind() != lexer::TokenKind::IndentGT ) {
            diagnostics().error(myScope->module(), diag::parser_expected_scope, scanner().peek());
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
    else if ( auto dtDecl = parse<DataTypeDeclaration>(*this) ) {
        auto p = dtDecl.get();
        append(std::move(dtDecl));
        return {true, parseDataTypeDefinition(*p) };
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

ab<Box<ast::Expression>> DeclarationScopeParser::parameterContext() const
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
        myAttributes.append(std::move(attr));
        if ( scanner().peek().kind() != lexer::TokenKind::IndentEQ ) {
            diagnostics().error(myScope->module(), diag::parser_expected_declaration, scanner().peek());
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
            diagnostics().error(myScope->module(), diag::parser_unexpected, scanner().peek());
            diagnostics().die();
        }
        
        if ( newScopeParser )
            return std::move(newScopeParser);

        if ( scanner().peek().kind() != lexer::TokenKind::IndentEQ )
            return nullptr;

        scanner().next();
    }

    return nullptr;
}

//
// DataTypeScopeParser

DataTypeScopeParser::DataTypeScopeParser(Diagnostics& dgn,
                                               lexer::Scanner& scanner,
                                               ast::DataTypeScope& scope)
    : DeclarationScopeParser(dgn, scanner, scope)
{
}

DataTypeScopeParser::~DataTypeScopeParser() = default;

ast::DataTypeScope& DataTypeScopeParser::scope()
{
    return *static_cast<ast::DataTypeScope*>(myScope);
}

ast::DataTypeScope const& DataTypeScopeParser::scope() const
{
    return *static_cast<ast::DataTypeScope*>(myScope);
}

DeclarationScopeParser::ParseResult
DataTypeScopeParser::parseNext()
{
    if ( DataTypeDeclarationField field; parse(scanner(), field) ) {
        auto [s, c, i] = field.make(*this);
        scope().appendField(std::move(s), std::move(c), std::move(i));
        return {true, nullptr};
    }
    else if ( auto sym = parse<DataVariation>(*this) ) {
        scope().appendVariation(std::move(*sym));
        return {true, parseDataTypeDefinition(*scope().variations().back())};
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

        diagnostics().error(s.module(), diag::parser_expected_preceding_branch, j);
        return nullptr;
    };

    if ( auto elseJunc = parse<BranchElseJunction>(*this) ) {
        auto br = lastBranch(*elseJunc);
        if ( !br )
            return {false, nullptr};

        // todo: lifetime of transient parse objects in diagnostics
        if ( !br->isMatch() && !br->branch(0) ) {
            diagnostics().error(scope().module(), diag::parser_expected_preceding_branch, *elseJunc);
            return {false, nullptr};
        }

        while ( br->branch(1) ) {
            if ( !br->branch(1)->junction() ) {
                diagnostics().error(scope().module(), diag::parser_expected_preceding_branch, *elseJunc);
                return {false, nullptr};
            }

            br = br->branch(1)->junction()->as<ast::BranchJunction>();
            if ( !br || !br->branch(0) ) {
                diagnostics().error(scope().module(), diag::parser_branch_else_proceeds, *elseJunc);
                return {false, nullptr};
            }
        }

        if ( br->branch(1) ) {
            diagnostics().error(scope().module(), diag::parser_branch_multiple_else, *elseJunc);
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
        auto s = &scope();
        auto b = s->basicBlocks().back();
        auto m = s->createBasicBlock();

        if ( branchJunc->label().lexeme() ) {
            s = s->createChildScope(m, branchJunc->token(), branchJunc->label());
            b->setJunction(mk<ast::JumpJunction>(s->basicBlocks().front()));
            b = s->basicBlocks().front();
        }

        b->setJunction(std::move(branchJunc));

        return {true, nullptr};
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
            diagnostics().error(scope().module(), diag::parser_unreachable, *retJunc);
            return {false, nullptr};
        }
        b->setJunction(std::move(retJunc));
        return {true, nullptr};
    }

    if ( auto jmpJunc = parse<JumpJunction>(*this) ) {
        auto b = scope().basicBlocks().back();
        if ( b->junction() ) {
            diagnostics().error(scope().module(), diag::parser_unreachable, *jmpJunc);
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

    ab<Box<parser::DeclarationScopeParser>> scopeStack;
    scopeStack.append(std::move(parser));

    while ( scanner ) {
        if ( scanner.peek().kind() == lexer::TokenKind::IndentEQ )
            scanner.next();

        auto nextScope = scopeStack.back()->next();
        if ( nextScope ) {
            scopeStack.append(std::move(nextScope));
        }
        else {
            switch (scanner.peek().kind()) {
            case lexer::TokenKind::EndOfInput:
                if ( scopeStack )
                    scopeStack.trunc(1);

            case lexer::TokenKind::IndentLT:
                break;

            case lexer::TokenKind::IndentGT:
            {
                dgn.error(mod, diag::parser_unexpected_scope, scanner.peek());
                dgn.die();
                return;
            }

            default:
                dgn.error(mod, diag::parser_expected_scope_end, scanner.peek());
                dgn.die();
            }

            while ( scanner.peek().kind() == lexer::TokenKind::IndentLT ) {
                if ( !scopeStack ) {
                    dgn.error(mod, diag::parser_indentation_mismatch, scanner.peek());
                    dgn.die();
                }

                scopeStack.pop();
                scanner.next();
            }
        }
    }

    if ( scanner.hasError() ) {
        dgn.error(mod, diag::lexer_error, scanner.peek());
        dgn.die();
    }

    ENFORCE(scopeStack.card() == 1, "parser scope imbalance");
}

} // namespace kyfoo::parser
