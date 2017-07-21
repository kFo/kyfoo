#include <kyfoo/ast/Declarations.hpp>

#include <kyfoo/Diagnostics.hpp>

#include <kyfoo/lexer/Scanner.hpp>

#include <kyfoo/ast/Expressions.hpp>
#include <kyfoo/ast/Scopes.hpp>
#include <kyfoo/ast/Context.hpp>

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
    , mySymbol(std::make_unique<Symbol>(std::move(symbol)))
{
}

Declaration::Declaration(Declaration const& rhs)
    : myKind(rhs.myKind)
{
}

Declaration::~Declaration() = default;

void Declaration::swap(Declaration& rhs)
{
    using std::swap;
    swap(myKind, rhs.myKind);
    swap(mySymbol, rhs.mySymbol);
    swap(myScope, rhs.myScope);
    // myCodeGenData does not get copied
}

void Declaration::io(IStream& stream) const
{
    std::string declkind = typeid(*this).name();
    stream.next("declkind", declkind);
    stream.next("symbol", mySymbol);
}

void Declaration::cloneChildren(Declaration& c, clone_map_t& map) const
{
    c.mySymbol = ast::clone(mySymbol, map);
}

IMPL_CLONE_REMAP_NOBASE_BEGIN(Declaration)
IMPL_CLONE_REMAP(myScope)
IMPL_CLONE_REMAP(mySymbol)
IMPL_CLONE_REMAP_END

DeclKind Declaration::kind() const
{
    return myKind;
}

Symbol& Declaration::symbol()
{
    return *mySymbol;
}

Symbol const& Declaration::symbol() const
{
    return *mySymbol;
}

lexer::Token const& Declaration::identifier() const
{
    return mySymbol->identifier();
}

DeclarationScope* Declaration::scope()
{
    return myScope;
}

DeclarationScope const* Declaration::scope() const
{
    return myScope;
}

void Declaration::setScope(DeclarationScope& scope)
{
    if ( myScope )
        throw std::runtime_error("declaration parent set twice");

    myScope = &scope;
}

codegen::CustomData* Declaration::codegenData()
{
    return myCodeGenData.get();
}

codegen::CustomData* Declaration::codegenData() const
{
    return myCodeGenData.get();
}

void Declaration::setCodegenData(std::unique_ptr<codegen::CustomData> data)
{
    if ( codegenData() )
        throw std::runtime_error("codegen data can only be set once");

    myCodeGenData = std::move(data);
}

void Declaration::setCodegenData(std::unique_ptr<codegen::CustomData> data) const
{
    return const_cast<Declaration*>(this)->setCodegenData(std::move(data));
}

//
// DataSumDeclaration

DataSumDeclaration::DataSumDeclaration(Symbol&& symbol)
    : Declaration(DeclKind::DataSum, std::move(symbol), nullptr)
{
}

DataSumDeclaration::DataSumDeclaration(DataSumDeclaration const& rhs)
    : Declaration(rhs)
{
}

DataSumDeclaration::~DataSumDeclaration() = default;

void DataSumDeclaration::swap(DataSumDeclaration& rhs)
{
    Declaration::swap(rhs);
    using std::swap;
    swap(myDefinition, rhs.myDefinition);
}

void DataSumDeclaration::io(IStream& stream) const
{
    Declaration::io(stream);
}

IMPL_CLONE_BEGIN(DataSumDeclaration, Declaration, Declaration)
IMPL_CLONE_CHILD(myDefinition)
IMPL_CLONE_END
IMPL_CLONE_REMAP_BEGIN(DataSumDeclaration, Declaration)
IMPL_CLONE_REMAP(myDefinition)
IMPL_CLONE_REMAP_END

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

DataSumScope const* DataSumDeclaration::definition() const
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

DataSumDeclaration::Constructor::Constructor(Constructor const& rhs)
    : Declaration(rhs)
    , myParent(rhs.myParent)
    , myParameters(ast::clone(rhs.myParameters))
{
}

DataSumDeclaration::Constructor& DataSumDeclaration::Constructor::operator = (Constructor const& rhs)
{
    Constructor(rhs).swap(*this);
    return *this;
}

DataSumDeclaration::Constructor::~Constructor() = default;

void DataSumDeclaration::Constructor::swap(Constructor& rhs)
{
    Declaration::swap(rhs);
    using std::swap;
    swap(myParent, rhs.myParent);
    swap(myParameters, rhs.myParameters);
}

void DataSumDeclaration::Constructor::io(IStream& stream) const
{
    Declaration::io(stream);
    stream.next("parameters", myParameters);
}

IMPL_CLONE_BEGIN(DataSumDeclaration::Constructor, Declaration, Declaration)
IMPL_CLONE_CHILD(myParameters)
IMPL_CLONE_END
IMPL_CLONE_REMAP_BEGIN(DataSumDeclaration::Constructor, Declaration)
IMPL_CLONE_REMAP(myParent)
IMPL_CLONE_REMAP(myParameters)
IMPL_CLONE_REMAP_END

void DataSumDeclaration::Constructor::resolveSymbols(Diagnostics& dgn)
{
    for ( auto& e : myParameters )
        e->setScope(*scope());

    for ( auto& e : myParameters )
        e->resolveSymbols(dgn);
}

void DataSumDeclaration::Constructor::setParent(DataSumDeclaration* dsDecl)
{
    if ( parent() )
        throw std::runtime_error("data sum constructor can only belong to one procedure");

    myParent = dsDecl;
}

DataSumDeclaration* DataSumDeclaration::Constructor::parent()
{
    return myParent;
}

DataSumDeclaration const* DataSumDeclaration::Constructor::parent() const
{
    return myParent;
}

Slice<VariableDeclaration*> DataSumDeclaration::Constructor::fields() const
{
    return myParameters;
}

//
// DataProductDeclaration

DataProductDeclaration::DataProductDeclaration(Symbol&& symbol)
    : Declaration(DeclKind::DataProduct, std::move(symbol), nullptr)
{
}

DataProductDeclaration::DataProductDeclaration(DataProductDeclaration const& rhs)
    : Declaration(rhs)
{
}

DataProductDeclaration::~DataProductDeclaration() = default;

void DataProductDeclaration::swap(DataProductDeclaration& rhs)
{
    Declaration::swap(rhs);
    using std::swap;
    swap(myDefinition, rhs.myDefinition);
}

void DataProductDeclaration::io(IStream& stream) const
{
    Declaration::io(stream);
    if ( definition() )
        definition()->io(stream);
}

IMPL_CLONE_BEGIN(DataProductDeclaration, Declaration, Declaration)
IMPL_CLONE_CHILD(myDefinition)
IMPL_CLONE_END
IMPL_CLONE_REMAP_BEGIN(DataProductDeclaration, Declaration)
IMPL_CLONE_REMAP(myDefinition)
IMPL_CLONE_REMAP_END

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
// DataProductDeclaration::Field

DataProductDeclaration::Field::Field(Symbol&& symbol,
                                     std::unique_ptr<Expression> constraint,
                                     std::unique_ptr<Expression> init)
    : Declaration(DeclKind::Field, std::move(symbol), nullptr)
    , myConstraint(std::move(constraint))
    , myInitializer(std::move(init))
{
}

DataProductDeclaration::Field::Field(Field const& rhs)
    : Declaration(rhs)
    , myParent(rhs.myParent)
{
    // clone myExpression
}

DataProductDeclaration::Field& DataProductDeclaration::Field::operator = (Field const& rhs)
{
    Field(rhs).swap(*this);
    return *this;
}

DataProductDeclaration::Field::~Field() = default;

void DataProductDeclaration::Field::swap(Field& rhs)
{
    Declaration::swap(rhs);
    using std::swap;
    swap(myParent, rhs.myParent);
    swap(myConstraint, rhs.myConstraint);
}

void DataProductDeclaration::Field::io(IStream& stream) const
{
    Declaration::io(stream);
    stream.next("constraint", myConstraint);
}

IMPL_CLONE_BEGIN(DataProductDeclaration::Field, Declaration, Declaration)
IMPL_CLONE_CHILD(myConstraint)
IMPL_CLONE_END
IMPL_CLONE_REMAP_BEGIN(DataProductDeclaration::Field, Declaration)
IMPL_CLONE_REMAP(myParent)
IMPL_CLONE_REMAP(myConstraint)
IMPL_CLONE_REMAP_END

void DataProductDeclaration::Field::resolveSymbols(Diagnostics& dgn)
{
    ScopeResolver resolver(myParent->scope());
    Context ctx(dgn, resolver);
    ctx.resolveExpression(myConstraint);
}

void DataProductDeclaration::Field::setParent(DataProductDeclaration* dpDecl)
{
    if ( parent() )
        throw std::runtime_error("field can only belong to one product type");

    myParent = dpDecl;
}

DataProductDeclaration* DataProductDeclaration::Field::parent()
{
    return myParent;
}

DataProductDeclaration const* DataProductDeclaration::Field::parent() const
{
    return myParent;
}

Expression& DataProductDeclaration::Field::constraint()
{
    return *myConstraint;
}

Expression const& DataProductDeclaration::Field::constraint() const
{
    return *myConstraint;
}

//
// SymbolDeclaration

SymbolDeclaration::SymbolDeclaration(Symbol&& symbol,
                                     std::unique_ptr<Expression> expression)
    : Declaration(DeclKind::Symbol, std::move(symbol), nullptr)
    , myExpression(std::move(expression))
{
}

SymbolDeclaration::SymbolDeclaration(SymbolDeclaration const& rhs)
    : Declaration(rhs)
{
}

SymbolDeclaration& SymbolDeclaration::operator = (SymbolDeclaration const& rhs)
{
    SymbolDeclaration(rhs).swap(*this);
    return *this;
}

SymbolDeclaration::~SymbolDeclaration() = default;

void SymbolDeclaration::swap(SymbolDeclaration& rhs)
{
    Declaration::swap(rhs);
    using std::swap;
    swap(myExpression, rhs.myExpression);
}

void SymbolDeclaration::io(IStream& stream) const
{
    Declaration::io(stream);
    myExpression->io(stream);
}

IMPL_CLONE_BEGIN(SymbolDeclaration, Declaration, Declaration)
IMPL_CLONE_CHILD(myExpression)
IMPL_CLONE_END
IMPL_CLONE_REMAP_BEGIN(SymbolDeclaration, Declaration)
IMPL_CLONE_REMAP(myExpression)
IMPL_CLONE_REMAP_END

void SymbolDeclaration::resolveSymbols(Diagnostics& dgn)
{
    ScopeResolver resolver(scope());
    resolver.addSupplementarySymbol(symbol());

    Context ctx(dgn, resolver);
    ctx.resolveExpression(myExpression);
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

VariableDeclaration::VariableDeclaration(VariableDeclaration const& rhs)
    : Declaration(rhs)
{
}

VariableDeclaration::~VariableDeclaration() = default;

void VariableDeclaration::swap(VariableDeclaration& rhs)
{
    Declaration::swap(rhs);
    using std::swap;
    swap(myConstraint, rhs.myConstraint);
    swap(myInitialization, rhs.myInitialization);
}

void VariableDeclaration::io(IStream& stream) const
{
    Declaration::io(stream);
    stream.next("constraint", myConstraint);
    if ( myInitialization )
        stream.next("init", myInitialization);
}

IMPL_CLONE_BEGIN(VariableDeclaration, Declaration, Declaration)
IMPL_CLONE_CHILD(myConstraint)
IMPL_CLONE_CHILD(myInitialization)
IMPL_CLONE_END
IMPL_CLONE_REMAP_BEGIN(VariableDeclaration, Declaration)
IMPL_CLONE_REMAP(myConstraint)
IMPL_CLONE_REMAP(myInitialization)
IMPL_CLONE_REMAP_END

void VariableDeclaration::resolveSymbols(Diagnostics& dgn)
{
    ScopeResolver resolver(scope());
    Context ctx(dgn, resolver);

    if ( myConstraint ) {
        ctx.resolveExpression(myConstraint);

        auto s = myConstraint->as<SymbolExpression>();
        if ( !s ) {
            ctx.error(*myConstraint) << "expected symbol expression";
            return;
        }

        if ( dgn.errorCount() )
            return;

        if ( !isDataDeclaration(s->declaration()->kind()) ) {
            ctx.error(*myConstraint) << "does not identify a data type";
            return;
        }
    }

    if ( myInitialization )
        ctx.resolveExpression(myInitialization);

    if ( !myConstraint && !myInitialization ) {
        ctx.error(symbol().identifier()) << "cannot infer data type without initializer";
        return;
    }

    // todo: constraint propagation via control flow
}

Expression* VariableDeclaration::constraint()
{
    return myConstraint.get();
}

Expression const* VariableDeclaration::constraint() const
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

ProcedureParameter::ProcedureParameter(ProcedureParameter const& rhs)
    : VariableDeclaration(rhs)
    , myParent(rhs.myParent)
{
}

ProcedureParameter& ProcedureParameter::operator = (ProcedureParameter const& rhs)
{
    ProcedureParameter(rhs).swap(*this);
    return *this;
}

ProcedureParameter::~ProcedureParameter() = default;

void ProcedureParameter::swap(ProcedureParameter& rhs)
{
    VariableDeclaration::swap(rhs);
    using std::swap;
    swap(myParent, rhs.myParent);
}

void ProcedureParameter::io(IStream& stream) const
{
    VariableDeclaration::io(stream);
}

IMPL_CLONE_BEGIN(ProcedureParameter, VariableDeclaration, Declaration)
IMPL_CLONE_END
IMPL_CLONE_REMAP_BEGIN(ProcedureParameter, VariableDeclaration)
IMPL_CLONE_REMAP(myParent)
IMPL_CLONE_REMAP_END

void ProcedureParameter::resolveSymbols(Diagnostics& dgn)
{
    ScopeResolver resolver(parent()->scope());
    resolver.addSupplementarySymbol(parent()->symbol());

    Context ctx(dgn, resolver);
    if ( constraint() )
        ctx.resolveExpression(myConstraint);
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
                                           std::unique_ptr<Expression> resultExpression)
    : Declaration(DeclKind::Procedure, std::move(symbol), nullptr)
    , myParameters(std::move(parameters))
    , myResult(std::make_unique<ProcedureParameter>(Symbol(lexer::Token()), std::move(resultExpression)))
{
    myResult->setParent(this);
    for ( auto& p : myParameters )
        p->setParent(this);
}

ProcedureDeclaration::ProcedureDeclaration(ProcedureDeclaration const& rhs)
    : Declaration(rhs)
    , myParameters(ast::clone(rhs.myParameters))
    , myResult(ast::clone(rhs.myResult))
    , myDefinition(ast::clone(rhs.myDefinition))
{
}

ProcedureDeclaration& ProcedureDeclaration::operator = (ProcedureDeclaration const& rhs)
{
    ProcedureDeclaration(rhs).swap(*this);
    return *this;
}

ProcedureDeclaration::~ProcedureDeclaration() = default;

void ProcedureDeclaration::swap(ProcedureDeclaration& rhs)
{
    Declaration::swap(rhs);
    using std::swap;
    swap(myParameters, rhs.myParameters);
    swap(myResult, rhs.myResult);
    swap(myDefinition, rhs.myDefinition);
}

void ProcedureDeclaration::io(IStream& stream) const
{
    Declaration::io(stream);
    stream.next("parameters", myParameters);
    stream.next("return", returnType());

    if ( myDefinition )
        stream.next("definition", myDefinition);
}

IMPL_CLONE_BEGIN(ProcedureDeclaration, Declaration, Declaration)
IMPL_CLONE_CHILD(myParameters)
IMPL_CLONE_CHILD(myResult)
IMPL_CLONE_CHILD(myDefinition)
IMPL_CLONE_END
IMPL_CLONE_REMAP_BEGIN(ProcedureDeclaration, Declaration)
IMPL_CLONE_REMAP(myParameters)
IMPL_CLONE_REMAP(myResult)
IMPL_CLONE_REMAP(myDefinition)
IMPL_CLONE_REMAP_END

void ProcedureDeclaration::resolveSymbols(Diagnostics& dgn)
{
    if ( auto defn = definition() )
        defn->resolveSymbols(dgn);
}

void ProcedureDeclaration::resolvePrototypeSymbols(Diagnostics& dgn)
{
    ScopeResolver resolver(scope());
    Context ctx(dgn, resolver);

    // Resolve return
    if ( !myResult->constraint() ) {
        ctx.error(symbol().identifier()) << "inferred return type not implemented";
        return;
    }

    myResult->resolveSymbols(dgn);

    // Resolve parameters -- ignore parameter name
    for ( auto& p : myParameters ) {
        if ( !p->constraint() ) {
            ctx.error(p->symbol().identifier()) << "inferred parameter types not implemented";
            return;
        }
        p->resolveSymbols(dgn);
    }
}

ProcedureScope* ProcedureDeclaration::definition()
{
    return myDefinition.get();
}

ProcedureScope const* ProcedureDeclaration::definition() const
{
    return myDefinition.get();
}

void ProcedureDeclaration::define(std::unique_ptr<ProcedureScope> definition)
{
    if ( myDefinition )
        throw std::runtime_error("procedure " + mySymbol->name() + " is already defined");

    myDefinition = std::move(definition);
    myDefinition->setDeclaration(this);

    for ( auto& p : myParameters )
        p->setScope(*myDefinition);
}

std::vector<std::unique_ptr<ProcedureParameter>>& ProcedureDeclaration::parameters()
{
    return myParameters;
}

Slice<ProcedureParameter*> ProcedureDeclaration::parameters() const
{
    return myParameters;
}

Expression* ProcedureDeclaration::returnType()
{
    return myResult->constraint();
}

Expression const* ProcedureDeclaration::returnType() const
{
    return myResult->constraint();
}

ProcedureParameter* ProcedureDeclaration::result()
{
    return myResult.get();
}

ProcedureParameter const* ProcedureDeclaration::result() const
{
    return myResult.get();
}

//
// ImportDeclaration

ImportDeclaration::ImportDeclaration(Symbol&& sym)
    : Declaration(DeclKind::Import, std::move(sym), nullptr)
{
    myModulePath.push_back(symbol().identifier());
}

ImportDeclaration::ImportDeclaration(std::vector<lexer::Token>&& modulePath)
    : Declaration(DeclKind::Import, Symbol(modulePath.back()), nullptr)
    , myModulePath(std::move(modulePath))
{
}

ImportDeclaration::ImportDeclaration(ImportDeclaration const& rhs)
    : Declaration(rhs)
{
}

ImportDeclaration& ImportDeclaration::operator = (ImportDeclaration const& rhs)
{
    ImportDeclaration(rhs).swap(*this);
    return *this;
}

ImportDeclaration::~ImportDeclaration() = default;

void ImportDeclaration::swap(ImportDeclaration& rhs)
{
    Declaration::swap(rhs);
}

void ImportDeclaration::io(IStream& stream) const
{
    Declaration::io(stream);
}

IMPL_CLONE_BEGIN(ImportDeclaration, Declaration, Declaration)
IMPL_CLONE_END
IMPL_CLONE_REMAP_BEGIN(ImportDeclaration, Declaration)
IMPL_CLONE_REMAP_END

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

SymbolVariable::SymbolVariable(SymbolVariable const& rhs)
    : Declaration(rhs)
    , myParent(rhs.myParent)
    , myName(rhs.myName)
    , myBoundExpression(rhs.myBoundExpression)
{
}

SymbolVariable& SymbolVariable::operator = (SymbolVariable const& rhs)
{
    SymbolVariable(rhs).swap(*this);
    return *this;
}

SymbolVariable::~SymbolVariable() = default;

void SymbolVariable::swap(SymbolVariable& rhs)
{
    Declaration::swap(rhs);
    using std::swap;
    swap(myParent, rhs.myParent);
    swap(myName, rhs.myName);
    swap(myBoundExpression, rhs.myBoundExpression);
}

void SymbolVariable::io(IStream& stream) const
{
    Declaration::io(stream);
    stream.next("name", myName);
}

IMPL_CLONE_BEGIN(SymbolVariable, Declaration, Declaration)
IMPL_CLONE_END
IMPL_CLONE_REMAP_BEGIN(SymbolVariable, Declaration)
IMPL_CLONE_REMAP(myParent)
IMPL_CLONE_REMAP(myBoundExpression)
IMPL_CLONE_REMAP_END

void SymbolVariable::resolveSymbols(Diagnostics&)
{
    throw std::runtime_error("symbol variables should not be resolved");
}

void SymbolVariable::bindExpression(Expression const* expr)
{
    if ( myBoundExpression )
        throw std::runtime_error("symbol variable is already bound to an expression");

    myBoundExpression = expr;
}

Expression const* SymbolVariable::boundExpression() const
{
    return myBoundExpression;
}

std::string const& SymbolVariable::name() const
{
    return myName;
}

Symbol const& SymbolVariable::parent() const
{
    return *myParent;
}

//
// Utilities

bool isDataDeclaration(DeclKind kind)
{
    switch (kind) {
    case DeclKind::DataProduct:
    case DeclKind::DataSum:
    case DeclKind::DataSumCtor:
        return true;
    }

    return false;
}

bool isMacroDeclaration(DeclKind kind)
{
    switch (kind) {
    case DeclKind::Symbol:
        return true;
    }

    return false;
}

bool hasIndirection(DeclKind kind)
{
    return isMacroDeclaration(kind) || kind == DeclKind::SymbolVariable;
}

    } // namespace ast
} // namespace kyfoo
