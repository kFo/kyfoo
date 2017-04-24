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
#define X(a,b,c) b,
        DECLARATION_KINDS(X)
#undef X
    };

    return s[static_cast<std::size_t>(kind)];
}

Declaration::Declaration(DeclKind kind,
                         Symbol&& symbol,
                         DeclarationScope* scope)
    : myScope(scope)
    , myKind(kind)
    , mySymbol(std::move(symbol))
{
}

Declaration::~Declaration() = default;

void Declaration::io(IStream& stream)
{
    std::string declkind = typeid(*this).name();
    stream.next("declkind", declkind);
    stream.next("symbol", mySymbol); // todo: full params
}

DeclKind Declaration::kind() const
{
    return myKind;
}

Symbol const& Declaration::symbol() const
{
    return mySymbol;
}

lexer::Token const& Declaration::identifier() const
{
    return mySymbol.identifier();
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

TypeDeclaration::TypeDeclaration(Symbol&& symbol)
    : Declaration(DeclKind::Type, std::move(symbol), nullptr)
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

void TypeDeclaration::define(std::unique_ptr<DeclarationScope> scope)
{
    if ( myDefinition )
        throw std::runtime_error("type declaration defined more than once");

    myDefinition = std::move(scope);
}

DeclarationScope* TypeDeclaration::definition()
{
    return myDefinition.get();
}

//
// SymbolDeclaration

SymbolDeclaration::SymbolDeclaration(Symbol&& symbol,
                                     std::unique_ptr<ValueExpression> expression)
    : Declaration(DeclKind::Symbol, std::move(symbol), nullptr)
    , myKind(Kind::ValueExpression)
    , myNode(std::move(expression))
{
}

SymbolDeclaration::SymbolDeclaration(Symbol&& symbol,
                                     std::unique_ptr<TypeExpression> typeExpression)
    : Declaration(DeclKind::Symbol, std::move(symbol), nullptr)
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

VariableDeclaration::VariableDeclaration(Symbol&& symbol,
                                         std::unique_ptr<TypeExpression> typeExpression,
                                         std::unique_ptr<ValueExpression> expression)
    : Declaration(DeclKind::Variable, std::move(symbol), nullptr)
    , myTypeExpression(std::move(typeExpression))
    , myValueExpression(std::move(expression))
{
}

VariableDeclaration::VariableDeclaration(Symbol&& symbol,
                                         std::unique_ptr<ValueExpression> expression)
    : VariableDeclaration(std::move(symbol), nullptr, std::move(expression))
{
}

VariableDeclaration::VariableDeclaration(Symbol&& symbol)
    : VariableDeclaration(std::move(symbol), nullptr, nullptr)
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

ProcedureParameter::ProcedureParameter(Symbol&& symbol)
    : VariableDeclaration(std::move(symbol))
{
}

ProcedureParameter::ProcedureParameter(Symbol&& symbol,
                                       std::unique_ptr<TypeExpression> typeExpression)
    : VariableDeclaration(std::move(symbol), std::move(typeExpression), nullptr)
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

ProcedureDeclaration::ProcedureDeclaration(Symbol&& symbol,
                                           std::vector<std::unique_ptr<ProcedureParameter>> parameters,
                                           std::unique_ptr<TypeExpression> returnTypeExpression)
    : Declaration(DeclKind::Procedure, std::move(symbol), nullptr)
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
        throw std::runtime_error("procedure " + mySymbol.name() + " is already defined");

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

ImportDeclaration::ImportDeclaration(Symbol&& symbol)
    : Declaration(DeclKind::Import, std::move(symbol), nullptr)
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
