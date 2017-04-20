#include <kyfoo/ast/Declarations.hpp>

#include <kyfoo/Diagnostics.hpp>

#include <kyfoo/lexer/Scanner.hpp>

#include <kyfoo/ast/ValueExpressions.hpp>
#include <kyfoo/ast/Scopes.hpp>
#include <kyfoo/ast/Semantics.hpp>
#include <kyfoo/ast/TypeExpressions.hpp>

namespace kyfoo {
    namespace ast {

//
// Declaration

const char* to_string(DeclKind kind)
{
    static const char* s[] = {
#define X(a,b) b,
        DECLARATION_KINDS(X)
#undef X
    };

    return s[static_cast<std::size_t>(kind)];
}

Declaration::Declaration(DeclKind kind,
                         lexer::Token const& identifier,
                         DeclarationScope* scope)
    : myScope(scope)
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

DeclarationScope* Declaration::scope()
{
    return myScope;
}

void Declaration::setScope(DeclarationScope& scope)
{
    if ( myScope )
        throw std::runtime_error("declaration parent set twice");

    myScope = &scope;
}

//
// TypeDeclaration

TypeDeclaration::TypeDeclaration(lexer::Token const& identifier)
    : TypeDeclaration(identifier, {})
{
}

TypeDeclaration::TypeDeclaration(lexer::Token const& identifier,
                                 std::vector<TypeParameter>&& parameters)
    : Declaration(DeclKind::Type, identifier, nullptr)
    , myParameters(std::move(parameters))
{
}

TypeDeclaration::~TypeDeclaration() = default;

void TypeDeclaration::io(IStream& stream)
{
    Declaration::io(stream);
}

void TypeDeclaration::resolveSymbols(Diagnostics&)
{
    // TODO
}

//
// SymbolDeclaration

SymbolDeclaration::SymbolDeclaration(lexer::Token const& identifier,
                                     std::unique_ptr<ValueExpression> expression)
    : Declaration(DeclKind::Symbol, identifier, nullptr)
    , myKind(Kind::ValueExpression)
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
    if ( auto p = valueExpression() )
        stream.next("value", p);
    else
        stream.next("value", typeExpression());
}

void SymbolDeclaration::resolveSymbols(Diagnostics& /*dgn*/)
{
    // Must resolve symbols in the scope in which they are instantiated
}

ValueExpression* SymbolDeclaration::valueExpression()
{
    if ( myKind == Kind::ValueExpression )
        return static_cast<ValueExpression*>(myNode.get());

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
                                         std::unique_ptr<ValueExpression> expression)
    : Declaration(DeclKind::Variable, identifier, nullptr)
    , myTypeExpression(std::move(typeExpression))
    , myValueExpression(std::move(expression))
{
}

VariableDeclaration::VariableDeclaration(lexer::Token const& identifier,
                                         std::unique_ptr<ValueExpression> expression)
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
    stream.next("value", myValueExpression);
}

void VariableDeclaration::resolveSymbols(Diagnostics& dgn)
{
    Resolver resolver(scope());

    if ( typeExpression() )
        typeExpression()->resolveSymbols(dgn, resolver);

    if ( valueExpression() )
        valueExpression()->resolveSymbols(dgn, resolver);
}

TypeExpression* VariableDeclaration::typeExpression()
{
    return myTypeExpression.get();
}

ValueExpression* VariableDeclaration::valueExpression()
{
    return myValueExpression.get();
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

void ProcedureParameter::resolveSymbols(Diagnostics& dgn)
{
    Resolver resolver(parent()->scope());
    typeExpression()->resolveSymbols(dgn, resolver);
}

void ProcedureParameter::setParent(ProcedureDeclaration* procDecl)
{
    if ( parent() )
        throw std::runtime_error("procedure parameter can only belong to one procedure");

    myParent = procDecl;
}

ProcedureDeclaration* ProcedureParameter::parent()
{
    return myParent;
}

//
// ProcedureDeclaration

ProcedureDeclaration::ProcedureDeclaration(lexer::Token const& identifier,
                                           std::vector<std::unique_ptr<ProcedureParameter>> parameters,
                                           std::unique_ptr<TypeExpression> returnTypeExpression)
    : Declaration(DeclKind::Procedure, identifier, nullptr)
    , myParameters(std::move(parameters))
    , myReturnTypeExpression(std::move(returnTypeExpression))
{
    for ( auto& p : myParameters )
        p->setParent(this);
}

ProcedureDeclaration::~ProcedureDeclaration() = default;

void ProcedureDeclaration::io(IStream& stream)
{
    Declaration::io(stream);
    stream.next("parameters", myParameters);
    stream.next("return", myReturnTypeExpression);

    if ( myDefinition )
        stream.next("definition", myDefinition);
}

void ProcedureDeclaration::resolveSymbols(Diagnostics& dgn)
{
    for ( auto&& p : myParameters )
        p->resolveSymbols(dgn);
    
    Resolver resolver(scope());
    if ( returnType() )
        returnType()->resolveSymbols(dgn, resolver);

    if ( definition() )
        definition()->resolveSymbols(dgn);
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

    for ( auto& p : myParameters )
        p->setScope(*myDefinition);
}

std::vector<std::unique_ptr<ProcedureParameter>>& ProcedureDeclaration::parameters()
{
    return myParameters;
}

TypeExpression* ProcedureDeclaration::returnType()
{
    return myReturnTypeExpression.get();
}

//
// ImportDeclaration

ImportDeclaration::ImportDeclaration(lexer::Token const& identifier)
    : Declaration(DeclKind::Import, identifier, nullptr)
{
}

ImportDeclaration::~ImportDeclaration()
{
}

void ImportDeclaration::io(IStream& stream)
{
    Declaration::io(stream);
}

void ImportDeclaration::resolveSymbols(Diagnostics&)
{
    // nop
}

    } // namespace ast
} // namespace kyfoo
