#include <kyfoo/ast/Declarations.hpp>

#include <kyfoo/Error.hpp>

#include <kyfoo/lexer/Scanner.hpp>

#include <kyfoo/ast/Expressions.hpp>
#include <kyfoo/ast/Scopes.hpp>
#include <kyfoo/ast/Types.hpp>

namespace kyfoo {
    namespace ast {

//
// Declaration

Declaration::Declaration() = default;

Declaration::Declaration(DeclarationScope const* parent)
    : myParent(parent)
{
}

Declaration::~Declaration() = default;

void Declaration::io(IStream& stream)
{
    std::string declkind = typeid(*this).name();
    stream.next("declkind", declkind);
}

void Declaration::setParent(DeclarationScope& parent)
{
    if ( myParent )
        throw std::logic_error("declaration parent set twice");

    myParent = &parent;
}

//
// SymbolDeclaration

SymbolDeclaration::SymbolDeclaration(lexer::Token identifier,
                                     std::unique_ptr<Expression> value)
    : myIdentifier(identifier)
    , myValue(std::move(value))
{
}

SymbolDeclaration::~SymbolDeclaration() = default;

void SymbolDeclaration::io(IStream& stream)
{
    Declaration::io(stream);
    stream.next("identifier", myIdentifier);
    stream.next("value", myValue);
}

//
// ProcedureDeclaration

ProcedureDeclaration::ProcedureDeclaration(lexer::Token identifier,
                                           std::vector<std::unique_ptr<ProcedureParameter>> parameters)
    : myIdentifier(identifier)
    , myParameters(std::move(parameters))
{
}

ProcedureDeclaration::~ProcedureDeclaration() = default;

void ProcedureDeclaration::io(IStream& stream)
{
    Declaration::io(stream);
    stream.next("identifier", myIdentifier);
    stream.next("parameters", myParameters);
    stream.next("definition", myDefinition);
}

ProcedureScope* ProcedureDeclaration::definition()
{
    return myDefinition.get();
}

void ProcedureDeclaration::define(std::unique_ptr<ProcedureScope> definition)
{
    if ( myDefinition )
        throw std::runtime_error("procedure " + myIdentifier.lexeme() + " is already defined");

    myDefinition = std::move(definition);
}

    } // namespace ast
} // namespace kyfoo
