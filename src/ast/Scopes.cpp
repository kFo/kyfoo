#include <kyfoo/ast/Scopes.hpp>

#include <cassert>

#include <kyfoo/ast/Declarations.hpp>
#include <kyfoo/ast/ValueExpressions.hpp>
#include <kyfoo/ast/Module.hpp>
#include <kyfoo/ast/Semantics.hpp>

namespace kyfoo {
    namespace ast {

//
// DeclarationScope

DeclarationScope::DeclarationScope(Module* module)
    : myModule(module)
    , myParent(nullptr)
{
}

DeclarationScope::DeclarationScope(DeclarationScope* parent)
    : myModule(parent->module())
    , myParent(parent)
{
}

DeclarationScope::~DeclarationScope() = default;

void DeclarationScope::io(IStream& stream)
{
    stream.next("declarations", myDeclarations);
}

void DeclarationScope::resolveImports(Diagnostics& dgn)
{
    for ( auto& e : myDeclarations ) {
        if ( auto d = e->as<ImportDeclaration>() ) {
            module()->import(dgn, d->identifier());
        }
    }
}

void DeclarationScope::resolveSymbols(Diagnostics& dgn)
{
    for ( std::size_t i = 0; i < myDeclarations.size(); ++i ) {
        auto d = myDeclarations[i].get();

        // Ensure identifier is unique
        for ( std::size_t j = 0; j < i; ++j ) {
            if ( myDeclarations[j]->identifier().lexeme() == d->identifier().lexeme() ) {
                auto& err = dgn.error(module(), myDeclarations[i]->identifier()) << "identifier already declared";
                err.see(myDeclarations[i].get());
                dgn.die();
            }
        }

        // Index it
        switch (d->kind()) {
        case DeclKind::Type:
            myTypes[d->identifier().lexeme()] = static_cast<TypeDeclaration*>(d);
            break;

        case DeclKind::Symbol:
            mySymbols[d->identifier().lexeme()] = static_cast<SymbolDeclaration*>(d);
            break;

        case DeclKind::Procedure:
            myProcedures[d->identifier().lexeme()] = static_cast<ProcedureDeclaration*>(d);
            break;

        case DeclKind::Variable:
            myVariables[d->identifier().lexeme()] = static_cast<VariableDeclaration*>(d);
            break;

        case DeclKind::Import:
            myImports[d->identifier().lexeme()] = static_cast<ImportDeclaration*>(d);
            break;

        default:
            throw std::runtime_error("unhandled declaration kind");
        }
    }

    for ( auto&& e : myProcedures )
        e.second->resolveSymbols(dgn);
}

void DeclarationScope::append(std::unique_ptr<Declaration> declaration)
{
    myDeclarations.emplace_back(std::move(declaration));
    myDeclarations.back()->setScope(*this);
}

void DeclarationScope::import(Module& module)
{
    append(std::make_unique<ImportDeclaration>(lexer::Token(lexer::TokenKind::Identifier, 0, 0, module.name())));
}

Module* DeclarationScope::module()
{
    return myModule;
}

DeclarationScope* DeclarationScope::parent()
{
    return myParent;
}

Declaration* DeclarationScope::find(std::string const& identifier)
{
    for ( auto&& d : myDeclarations )
        if ( d->identifier().lexeme() == identifier )
            return d.get();

    return nullptr;
}

//
// ProcedureScope

ProcedureScope::ProcedureScope(DeclarationScope* parent,
                               ProcedureDeclaration& declaration)
    : DeclarationScope(parent)
    , myDeclaration(&declaration)
{
}

ProcedureScope::~ProcedureScope() = default;

void ProcedureScope::io(IStream& stream)
{
    DeclarationScope::io(stream);
    stream.next("expressions", myExpressions);
}

void ProcedureScope::resolveSymbols(Diagnostics& dgn)
{
    DeclarationScope::resolveSymbols(dgn);

    Resolver resolver(this);
    for ( auto&& expr : myExpressions )
        expr->resolveSymbols(dgn, resolver);
}

Declaration* ProcedureScope::find(std::string const& identifier)
{
    for ( auto&& p : myDeclaration->parameters() )
        if ( p->identifier().lexeme() == identifier )
            return p.get();

    return DeclarationScope::find(identifier);
}

void ProcedureScope::append(std::unique_ptr<ValueExpression> expression)
{
    myExpressions.emplace_back(std::move(expression));
}

    } // namespace ast
} // namespace kyfoo
