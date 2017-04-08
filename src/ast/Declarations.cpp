#include <kyfoo/ast/Declarations.hpp>

#include <kyfoo/Error.hpp>

#include <kyfoo/lexer/Scanner.hpp>

#include <kyfoo/ast/Expressions.hpp>
#include <kyfoo/ast/Scopes.hpp>
#include <kyfoo/ast/Semantics.hpp>
#include <kyfoo/ast/Types.hpp>

namespace kyfoo {
    namespace ast {

//
// Declaration

Declaration::Declaration(DeclKind kind,
                         lexer::Token const& identifier,
                         DeclarationScope const* parent)
    : myParent(parent)
    , myKind(kind)
    , myIdentifier(identifier)
{
}

Declaration::~Declaration() = default;

void Declaration::io(IStream& stream)
{
    std::string declkind = typeid(*this).name();
    stream.next("declkind", declkind);
    stream.next("identifier", myIdentifier);
}

DeclKind Declaration::kind() const
{
    return myKind;
}

lexer::Token const& Declaration::identifier() const
{
    return myIdentifier;
}

void Declaration::setParent(DeclarationScope& parent)
{
    if ( myParent )
        throw std::logic_error("declaration parent set twice");

    myParent = &parent;
}

//
// TypeDeclaration

TypeDeclaration::TypeDeclaration(lexer::Token const& identifier)
    : Declaration(DeclKind::Type, identifier, nullptr)
{
}

TypeDeclaration::~TypeDeclaration() = default;

void TypeDeclaration::io(IStream& stream)
{
    Declaration::io(stream);
}

void TypeDeclaration::resolveSymbols(Semantics& semantics)
{
    if ( auto d = semantics.scope()->lookup(identifier().lexeme()) )
        throw Error(identifier()) << "already defined as " << d->identifier();

    semantics.scope()->append(this);
}

//
// SymbolDeclaration

SymbolDeclaration::SymbolDeclaration(lexer::Token const& identifier,
                                     std::unique_ptr<Expression> expression)
    : Declaration(DeclKind::Symbol, identifier, nullptr)
    , myKind(Kind::Expression)
    , myNode(std::move(expression))
{
}

SymbolDeclaration::SymbolDeclaration(lexer::Token const& identifier,
                                     std::unique_ptr<TypeExpression> typeExpression)
    : Declaration(DeclKind::Symbol, identifier, nullptr)
    , myKind(Kind::TypeExpression)
    , myNode(std::move(typeExpression))
{
}

SymbolDeclaration::~SymbolDeclaration() = default;

void SymbolDeclaration::io(IStream& stream)
{
    Declaration::io(stream);
    if ( auto p = expression() )
        stream.next("value", p);
    else
        stream.next("value", typeExpression());
}

void SymbolDeclaration::resolveSymbols(Semantics& semantics)
{
    if ( auto d = semantics.scope()->find(identifier().lexeme()) ) {
        throw Error(identifier()) << "already defined at " << d->identifier();
    }
    semantics.scope()->append(this);
    myNode->resolveSymbols(semantics);
}

Expression* SymbolDeclaration::expression()
{
    if ( myKind == Kind::Expression )
        return static_cast<Expression*>(myNode.get());

    return nullptr;
}

TypeExpression* SymbolDeclaration::typeExpression()
{
    if ( myKind == Kind::TypeExpression )
        return static_cast<TypeExpression*>(myNode.get());

    return nullptr;
}

//
// VariableDeclaration

VariableDeclaration::VariableDeclaration(lexer::Token const& identifier,
                                         std::unique_ptr<TypeExpression> typeExpression,
                                         std::unique_ptr<Expression> expression)
    : Declaration(DeclKind::Variable, identifier, nullptr)
    , myTypeExpression(std::move(typeExpression))
    , myExpression(std::move(expression))
{
}

VariableDeclaration::VariableDeclaration(lexer::Token const& identifier,
                                         std::unique_ptr<Expression> expression)
    : VariableDeclaration(identifier, nullptr, std::move(expression))
{
}

VariableDeclaration::VariableDeclaration(lexer::Token const& identifier)
    : VariableDeclaration(identifier, nullptr, nullptr)
{
}

VariableDeclaration::~VariableDeclaration() = default;

void VariableDeclaration::io(IStream& stream)
{
    Declaration::io(stream);
    stream.next("value", myExpression);
}

void VariableDeclaration::resolveSymbols(Semantics& semantics)
{
    if ( auto d = semantics.scope()->find(identifier().lexeme()) )
        throw Error(identifier()) << "already defined as " << d->identifier();

    semantics.scope()->append(this);
}

TypeExpression const* VariableDeclaration::typeExpression() const
{
    return myTypeExpression.get();
}

Expression const* VariableDeclaration::expression() const
{
    return myExpression.get();
}

//
// ProcedureParameter

ProcedureParameter::ProcedureParameter(lexer::Token const& identifier)
    : VariableDeclaration(identifier)
{
}

ProcedureParameter::ProcedureParameter(lexer::Token const& identifier,
                                       std::unique_ptr<TypeExpression> typeExpression)
    : VariableDeclaration(identifier, std::move(typeExpression), nullptr)
{
}

void ProcedureParameter::io(IStream& stream)
{
    VariableDeclaration::io(stream);
}

void ProcedureParameter::resolveSymbols(Semantics& semantics)
{
    if ( auto d = semantics.scope()->find(myIdentifier.lexeme()) )
        throw Error(myIdentifier) << "already defined at " << d->identifier();

    semantics.scope()->append(this);
}

//
// ProcedureDeclaration

ProcedureDeclaration::ProcedureDeclaration(lexer::Token identifier,
                                           std::vector<std::unique_ptr<ProcedureParameter>> parameters,
                                           std::unique_ptr<TypeExpression> returnTypeExpression)
    : Declaration(DeclKind::Procedure, identifier, nullptr)
    , myParameters(std::move(parameters))
    , myReturnTypeExpression(std::move(returnTypeExpression))
{
}

ProcedureDeclaration::~ProcedureDeclaration() = default;

void ProcedureDeclaration::io(IStream& stream)
{
    Declaration::io(stream);
    stream.next("identifier", myIdentifier);
    stream.next("parameters", myParameters);
    stream.next("return", myReturnTypeExpression);

    if ( myDefinition )
        stream.next("definition", myDefinition);
}

void ProcedureDeclaration::resolveSymbols(Semantics& semantics)
{
    if ( auto d = semantics.scope()->find(identifier().lexeme()) ) {
        throw Error(identifier()) << "already defined at " << d->identifier();
    }
    semantics.scope()->append(this);
    if ( definition() ) {
        semantics.pushScope();
        for ( auto&& e : myParameters )
            e->resolveSymbols(semantics);
        definition()->resolveSymbols(semantics);
        semantics.popScope();
    }
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
