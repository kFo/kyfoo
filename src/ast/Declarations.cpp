#include <kyfoo/ast/Declarations.hpp>

#include <kyfoo/Diagnostics.hpp>

#include <kyfoo/lexer/Scanner.hpp>

#include <kyfoo/ast/Expressions.hpp>
#include <kyfoo/ast/Scopes.hpp>
#include <kyfoo/ast/Semantics.hpp>

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

void Declaration::io(IStream& stream) const
{
    std::string declkind = typeid(*this).name();
    stream.next("declkind", declkind);
    stream.next("symbol", mySymbol);
}

DeclKind Declaration::kind() const
{
    return myKind;
}

Symbol& Declaration::symbol()
{
    return mySymbol;
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
// DataSumDeclaration

DataSumDeclaration::DataSumDeclaration(Symbol&& symbol)
    : Declaration(DeclKind::DataSum, std::move(symbol), nullptr)
{
}

DataSumDeclaration::~DataSumDeclaration() = default;

void DataSumDeclaration::io(IStream& stream) const
{
    Declaration::io(stream);
}

void DataSumDeclaration::resolveSymbols(Diagnostics& dgn)
{
    if ( definition() )
        definition()->resolveSymbols(dgn);
}

void DataSumDeclaration::define(std::unique_ptr<DataSumScope> scope)
{
    if ( myDefinition )
        throw std::runtime_error("type declaration defined more than once");

    myDefinition = std::move(scope);
    myDefinition->setDeclaration(this);
}

DataSumScope* DataSumDeclaration::definition()
{
    return myDefinition.get();
}

//
// DataSumDeclaration::Constructor

DataSumDeclaration::Constructor::Constructor(Symbol&& symbol,
                                             std::vector<std::unique_ptr<VariableDeclaration>>&& parameters)
    : Declaration(DeclKind::DataSumCtor, std::move(symbol), nullptr)
    , myParameters(std::move(parameters))
{
}

DataSumDeclaration::Constructor::~Constructor() = default;

void DataSumDeclaration::Constructor::io(IStream& stream) const
{
    Declaration::io(stream);
    stream.next("parameters", myParameters);
}

void DataSumDeclaration::Constructor::resolveSymbols(Diagnostics& dgn)
{
    for ( auto& e : myParameters )
        e->setScope(*scope());

    for ( auto& e : myParameters )
        e->resolveSymbols(dgn);
}

//
// DataProductDeclaration

DataProductDeclaration::DataProductDeclaration(Symbol&& symbol)
    : Declaration(DeclKind::DataProduct, std::move(symbol), nullptr)
{
}

DataProductDeclaration::~DataProductDeclaration() = default;

void DataProductDeclaration::io(IStream& stream) const
{
    Declaration::io(stream);
    if ( definition() )
        definition()->io(stream);
}

void DataProductDeclaration::resolveSymbols(Diagnostics& dgn)
{
    if ( definition() )
        definition()->resolveSymbols(dgn);
}

void DataProductDeclaration::define(std::unique_ptr<DataProductScope> scope)
{
    if ( myDefinition )
        throw std::runtime_error("type declaration defined more than once");

    myDefinition = std::move(scope);
    myDefinition->setDeclaration(this);
}

DataProductScope* DataProductDeclaration::definition()
{
    return myDefinition.get();
}

DataProductScope const* DataProductDeclaration::definition() const
{
    return myDefinition.get();
}

//
// SymbolDeclaration

SymbolDeclaration::SymbolDeclaration(Symbol&& symbol,
                                     std::unique_ptr<Expression> expression)
    : Declaration(DeclKind::Symbol, std::move(symbol), nullptr)
    , myExpression(std::move(expression))
{
}

SymbolDeclaration::~SymbolDeclaration() = default;

void SymbolDeclaration::io(IStream& stream) const
{
    Declaration::io(stream);
    myExpression->io(stream);
}

void SymbolDeclaration::resolveSymbols(Diagnostics& /*dgn*/)
{
    // Must resolve symbols in the scope in which they are instantiated
}

Expression* SymbolDeclaration::expression()
{
    return myExpression.get();
}

Expression const* SymbolDeclaration::expression() const
{
    return const_cast<SymbolDeclaration*>(this)->expression();
}

//
// VariableDeclaration

VariableDeclaration::VariableDeclaration(Symbol&& symbol,
                                         std::unique_ptr<Expression> constraint,
                                         std::unique_ptr<Expression> init)
    : Declaration(DeclKind::Variable, std::move(symbol), nullptr)
    , myConstraint(std::move(constraint))
    , myInitialization(std::move(init))
{
}

VariableDeclaration::~VariableDeclaration() = default;

void VariableDeclaration::io(IStream& stream) const
{
    Declaration::io(stream);
    stream.next("constraint", myConstraint);
    if ( myInitialization )
        stream.next("init", myInitialization);
}

void VariableDeclaration::resolveSymbols(Diagnostics& dgn)
{
    ScopeResolver resolver(scope());

    if ( myConstraint )
        myConstraint->resolveSymbols(dgn, resolver);

    if ( myInitialization )
        myInitialization->resolveSymbols(dgn, resolver);
}

Expression* VariableDeclaration::constraint()
{
    return myConstraint.get();
}

//
// ProcedureParameter

ProcedureParameter::ProcedureParameter(Symbol&& symbol,
                                       std::unique_ptr<Expression> constraint)
    : VariableDeclaration(std::move(symbol), std::move(constraint), nullptr)
{
}

ProcedureParameter::~ProcedureParameter() = default;

void ProcedureParameter::io(IStream& stream) const
{
    VariableDeclaration::io(stream);
}

void ProcedureParameter::resolveSymbols(Diagnostics& dgn)
{
    ScopeResolver resolver(parent()->scope());
    resolver.addSupplementarySymbol(parent()->symbol());
    if ( constraint() )
        constraint()->resolveSymbols(dgn, resolver);
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
                                           std::unique_ptr<Expression> returnExpression)
    : Declaration(DeclKind::Procedure, std::move(symbol), nullptr)
    , myParameters(std::move(parameters))
    , myReturnExpression(std::move(returnExpression))
{
    for ( auto& p : myParameters )
        p->setParent(this);
}

ProcedureDeclaration::~ProcedureDeclaration() = default;

void ProcedureDeclaration::io(IStream& stream) const
{
    Declaration::io(stream);
    stream.next("parameters", myParameters);
    stream.next("return", myReturnExpression);

    if ( myDefinition )
        stream.next("definition", myDefinition);
}

void ProcedureDeclaration::resolveSymbols(Diagnostics& dgn)
{
    for ( auto&& p : myParameters )
        p->resolveSymbols(dgn);
    
    ScopeResolver resolver(scope());
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
    myDefinition->setDeclaration(this);

    for ( auto& p : myParameters )
        p->setScope(*myDefinition);
}

std::vector<std::unique_ptr<ProcedureParameter>>& ProcedureDeclaration::parameters()
{
    return myParameters;
}

Expression* ProcedureDeclaration::returnType()
{
    return myReturnExpression.get();
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

void ImportDeclaration::io(IStream& stream) const
{
    Declaration::io(stream);
}

void ImportDeclaration::resolveSymbols(Diagnostics&)
{
    // nop
}

//
// SymbolVariable

SymbolVariable::SymbolVariable(Symbol& parent, std::string const& name)
    : Declaration(DeclKind::SymbolVariable, Symbol(lexer::Token(lexer::TokenKind::Identifier, parent.identifier().line(), parent.identifier().column(), name)), nullptr)
    , myParent(&parent)
    , myName(name)
{
}

SymbolVariable::~SymbolVariable() = default;

void SymbolVariable::io(IStream& stream) const
{
    Declaration::io(stream);
    stream.next("name", myName);
}

void SymbolVariable::resolveSymbols(Diagnostics&)
{
    throw std::runtime_error("symbol variables should not be resolved");
}

std::string const& SymbolVariable::name() const
{
    return myName;
}

Symbol const& SymbolVariable::parent() const
{
    return *myParent;
}

    } // namespace ast
} // namespace kyfoo
