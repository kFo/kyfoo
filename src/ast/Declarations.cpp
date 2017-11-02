#include <kyfoo/ast/Declarations.hpp>

#include <kyfoo/Diagnostics.hpp>

#include <kyfoo/lexer/Scanner.hpp>

#include <kyfoo/ast/Expressions.hpp>
#include <kyfoo/ast/Fabrication.hpp>
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
    , myScope(rhs.myScope)
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

void Declaration::resolveAttributes(Module& endModule, Diagnostics& dgn)
{
    ScopeResolver resolver(*myScope);
    Context ctx(endModule, dgn, resolver);
    ctx.resolveStatements(myAttributes);
}

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

DeclarationScope& Declaration::scope()
{
    return *myScope;
}

DeclarationScope const& Declaration::scope() const
{
    return *myScope;
}

void Declaration::setScope(DeclarationScope& scope)
{
    if ( myScope && myScope != &scope ) {
        if ( myScope->declaration()->kind() != DeclKind::Template )
            throw std::runtime_error("declaration parent set twice");
    }

    myScope = &scope;
}

void Declaration::setAttributes(std::vector<std::unique_ptr<Expression>>&& exprs)
{
    for ( auto& e : exprs )
        myAttributes.emplace_back(std::move(e));

    exprs.clear();
}

Slice<Statement> const Declaration::attributes() const
{
    return myAttributes;
}

Slice<Statement> Declaration::attributes()
{
    return myAttributes;
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

void DataSumDeclaration::resolveSymbols(Module& endModule, Diagnostics& dgn)
{
    if ( definition() )
        definition()->resolveSymbols(endModule, dgn);
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
                                             std::vector<std::unique_ptr<VariableDeclaration>>&& pattern)
    : Declaration(DeclKind::DataSumCtor, std::move(symbol), nullptr)
    , myPattern(std::move(pattern))
{
}

DataSumDeclaration::Constructor::Constructor(Constructor const& rhs)
    : Declaration(rhs)
    , myParent(rhs.myParent)
    , myPattern(ast::clone(rhs.myPattern))
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
    swap(myPattern, rhs.myPattern);
}

void DataSumDeclaration::Constructor::io(IStream& stream) const
{
    Declaration::io(stream);
    stream.next("pattern", myPattern);
}

IMPL_CLONE_BEGIN(DataSumDeclaration::Constructor, Declaration, Declaration)
IMPL_CLONE_CHILD(myPattern)
IMPL_CLONE_END
IMPL_CLONE_REMAP_BEGIN(DataSumDeclaration::Constructor, Declaration)
IMPL_CLONE_REMAP(myParent)
IMPL_CLONE_REMAP(myPattern)
IMPL_CLONE_REMAP_END

void DataSumDeclaration::Constructor::resolveSymbols(Module& endModule, Diagnostics& dgn)
{
    for ( auto& e : myPattern )
        e->setScope(scope());

    for ( auto& e : myPattern )
        e->resolveSymbols(endModule, dgn);
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
    return myPattern;
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

void DataProductDeclaration::resolveSymbols(Module& endModule, Diagnostics& dgn)
{
    if ( definition() )
        definition()->resolveSymbols(endModule, dgn);
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

void DataProductDeclaration::Field::resolveSymbols(Module& endModule, Diagnostics& dgn)
{
    ScopeResolver resolver(*myParent->definition());
    Context ctx(endModule, dgn, resolver);
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

void SymbolDeclaration::resolveSymbols(Module& endModule, Diagnostics& dgn)
{
    ScopeResolver resolver(scope());
    resolver.addSupplementaryPrototype(symbol().prototype());

    Context ctx(endModule, dgn, resolver);
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
                                         ProcedureScope& scope,
                                         std::unique_ptr<Expression> constraint)
    : Declaration(DeclKind::Variable, std::move(symbol), &scope)
    , myConstraint(std::move(constraint))
{
}

VariableDeclaration::VariableDeclaration(ProcedureScope& scope,
                                         Declaration const& dataType)
    : Declaration(DeclKind::Variable, Symbol(lexer::Token()), &scope)
    , myDataType(&dataType)
{
}

VariableDeclaration::VariableDeclaration(VariableDeclaration const& rhs)
    : Declaration(rhs)
    , myDataType(rhs.myDataType)
{
}

VariableDeclaration& VariableDeclaration::operator = (VariableDeclaration const& rhs)
{
    VariableDeclaration(rhs).swap(*this);
    return *this;
}

VariableDeclaration::~VariableDeclaration() = default;

void VariableDeclaration::swap(VariableDeclaration& rhs)
{
    Declaration::swap(rhs);
    using std::swap;
    swap(myConstraint, rhs.myConstraint);
    swap(myConstraints, rhs.myConstraints);
    swap(myDataType, rhs.myDataType);
}

void VariableDeclaration::io(IStream& stream) const
{
    Declaration::io(stream);
    stream.openArray("constraints");
    for ( auto c : myConstraints )
        stream.next("", *c);
    stream.closeArray();
}

IMPL_CLONE_BEGIN(VariableDeclaration, Declaration, Declaration)
IMPL_CLONE_CHILD(myConstraint)
IMPL_CLONE_END
IMPL_CLONE_REMAP_BEGIN(VariableDeclaration, Declaration)
IMPL_CLONE_REMAP(myConstraints)
IMPL_CLONE_REMAP(myDataType)
IMPL_CLONE_REMAP_END

void VariableDeclaration::resolveSymbols(Module& endModule, Diagnostics& dgn)
{
    ScopeResolver resolver(scope());
    Context ctx(endModule, dgn, resolver);

    ctx.resolveExpression(myConstraint);
    addConstraint(*myConstraint);

    myDataType = ast::dataType(ctx, myConstraints);
    if ( !myDataType ) {
        if ( myConstraints.empty() )
            ctx.error(*this) << "cannot infer data type without constraints";
        else
            ctx.error(*this) << "no data type determined for variable declaration";
        return;
    }
}

void VariableDeclaration::addConstraint(Expression const& expr)
{
    myConstraints.push_back(&expr);
}

Slice<Expression const*> VariableDeclaration::constraints() const
{
    return myConstraints;
}

Declaration const* VariableDeclaration::dataType() const
{
    return myDataType;
}

//
// ProcedureParameter

ProcedureParameter::ProcedureParameter(Symbol&& symbol,
                                       ProcedureDeclaration& proc,
                                       PassSemantics passSemantics)
    : Declaration(DeclKind::ProcedureParameter, std::move(symbol), &proc.scope())
    , myPassSemantics(passSemantics)
{
}

ProcedureParameter::ProcedureParameter(ProcedureParameter const& rhs)
    : Declaration(rhs)
    , myConstraints(rhs.myConstraints)
    , myDataType(rhs.myDataType)
    , myPassSemantics(rhs.myPassSemantics)
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
    Declaration::swap(rhs);
    using std::swap;
    swap(myConstraints, rhs.myConstraints);
    swap(myDataType, rhs.myDataType);
    swap(myPassSemantics, rhs.myPassSemantics);
}

void ProcedureParameter::io(IStream& stream) const
{
    Declaration::io(stream);
}

IMPL_CLONE_BEGIN(ProcedureParameter, Declaration, Declaration)
IMPL_CLONE_END
IMPL_CLONE_REMAP_BEGIN(ProcedureParameter, Declaration)
IMPL_CLONE_REMAP(myConstraints)
IMPL_CLONE_REMAP(myDataType)
IMPL_CLONE_REMAP_END

void ProcedureParameter::resolveSymbols(Module& endModule, Diagnostics& dgn)
{
    ScopeResolver resolver(scope());
    Context ctx(endModule, dgn, resolver);
    myDataType = ast::dataType(ctx, myConstraints);

    if ( !myDataType )
        dgn.error(scope().module(), symbol().identifier()) << "parameter is missing data type";
}

Declaration const* ProcedureParameter::dataType() const
{
    return myDataType;
}

Slice<Expression*> const ProcedureParameter::constraints() const
{
    return myConstraints;
}

void ProcedureParameter::addConstraint(Expression const& expr)
{
    myConstraints.push_back(&expr);
}

ProcedureParameter::PassSemantics ProcedureParameter::passSemantics() const
{
    return myPassSemantics;
}

//
// ProcedureDeclaration

ProcedureDeclaration::ProcedureDeclaration(Symbol&& symbol,
                                           std::unique_ptr<Expression> returnExpression)
    : Declaration(DeclKind::Procedure, std::move(symbol), nullptr)
    , myReturnExpression(std::move(returnExpression))
{
}

ProcedureDeclaration::ProcedureDeclaration(ProcedureDeclaration const& rhs)
    : Declaration(rhs)
    , myOrdinals(rhs.myOrdinals)
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
    swap(myReturnExpression, rhs.myReturnExpression);
    swap(myThisExpr, rhs.myThisExpr);
    swap(myParameters, rhs.myParameters);
    swap(myOrdinals, rhs.myOrdinals);
    swap(myResult, rhs.myResult);
    swap(myDefinition, rhs.myDefinition);
}

void ProcedureDeclaration::io(IStream& stream) const
{
    Declaration::io(stream);
    stream.next("params", myParameters);
    stream.next("return", myResult);

    if ( myDefinition )
        stream.next("definition", myDefinition);
}

IMPL_CLONE_BEGIN(ProcedureDeclaration, Declaration, Declaration)
IMPL_CLONE_CHILD(myReturnExpression)
IMPL_CLONE_CHILD(myThisExpr)
IMPL_CLONE_CHILD(myParameters)
IMPL_CLONE_CHILD(myResult)
IMPL_CLONE_CHILD(myDefinition)
IMPL_CLONE_END
IMPL_CLONE_REMAP_BEGIN(ProcedureDeclaration, Declaration)
IMPL_CLONE_REMAP(myReturnExpression)
IMPL_CLONE_REMAP(myThisExpr)
IMPL_CLONE_REMAP(myParameters)
IMPL_CLONE_REMAP(myResult)
IMPL_CLONE_REMAP(myDefinition)
IMPL_CLONE_REMAP_END

void ProcedureDeclaration::resolveSymbols(Module& endModule, Diagnostics& dgn)
{
    if ( auto defn = definition() ) {
        for ( auto const& p : myParameters )
            defn->addSymbol(dgn, p->symbol(), *p);

        defn->resolveSymbols(endModule, dgn);
    }
}

void ProcedureDeclaration::resolvePrototypeSymbols(Module& endModule, Diagnostics& dgn)
{
    ScopeResolver resolver(scope());
    Context ctx(endModule, dgn, resolver);

    // Parameter deduction (step #1/2)
    auto const& id = symbol().identifier();
    auto const deduceParameters = myParameters.empty();
    if ( deduceParameters ) {
        auto thisType = outerDataDeclaration(*this);
        if ( thisType ) {
            auto thisToken = lexer::Token(lexer::TokenKind::Identifier, id.line(), id.column(), "this");
            myThisExpr = std::make_unique<ReferenceExpression>(std::make_unique<PrimaryExpression>(thisToken));
            myThisExpr->addConstraint(std::make_unique<PrimaryExpression>(lexer::Token(lexer::TokenKind::Identifier, id.line(), id.column(), "this_t")));
            myThisExpr->constraints()[0]->setDeclaration(*thisType);

            myParameters.emplace_back(std::make_unique<ProcedureParameter>(Symbol(thisToken), *this, ProcedureParameter::ByReference));
            myThisExpr->setDeclaration(*myParameters.back());
            for ( auto const& c : myThisExpr->constraints() )
                myParameters.back()->addConstraint(*c);
        }

        for ( int i = 0; i < mySymbol->prototype().pattern().size(); ++i ) {
            auto const& pattern = mySymbol->prototype().pattern()[i];
            auto expr = pattern;
            if ( auto a = pattern->as<ApplyExpression>() ) {
                if ( a->expressions().size() != 1 )
                    continue;

                expr = a->expressions()[0];
            }

            auto passSemantics = ProcedureParameter::ByValue;
            auto p = expr->as<PrimaryExpression>();
            if ( !p ) {
                if ( auto r = expr->as<ReferenceExpression>() ) {
                    p = r->expression().as<PrimaryExpression>();
                    passSemantics = ProcedureParameter::ByReference;
                }
            }

            if ( !p || p->token().kind() != lexer::TokenKind::Identifier )
                continue;

            auto hit = ctx.matchOverload(SymbolReference(p->token().lexeme()));
            if ( !hit )
                hit = ctx.matchOverload(p->token().lexeme());

            if ( !hit ) {
                myOrdinals.push_back(static_cast<int>(myParameters.size()));
                myParameters.emplace_back(std::make_unique<ProcedureParameter>(Symbol(p->token()), *this, passSemantics));
                p->setDeclaration(*myParameters.back());
            }
            else {
                myOrdinals.push_back(-1);
            }
        }
    }

    // Pattern resolution
    // This comes in the middle of parameter deduction because pattern resolution depends
    // on knowing what front expressions are to be interpreted as procedure parameters, 
    // otherwise causing an undeclared identifier error. Parameters eventually need to hold
    // references to their constraints, but pattern resolution must first resolve those constraints
    mySymbol->resolveSymbols(ctx);
    resolver.addSupplementaryPrototype(mySymbol->prototype());

    // Parameter deduction (step #2/2)
    if ( deduceParameters ) {
        // constraints must be added after the symbol is resolved due to rewrites
        for ( std::size_t i = 0; i < mySymbol->prototype().pattern().size(); ++i ) {
            auto const ordinal = myOrdinals[i];
            if ( ordinal < 0 )
                continue;

            auto const paramConstraints = mySymbol->prototype().pattern()[i]->constraints();
            for ( auto const& c : paramConstraints )
                myParameters[ordinal]->addConstraint(*c);
        }

        for ( auto& p : myParameters )
            p->resolveSymbols(endModule, dgn);
    }

    // Resolve return
    // todo: return type deduction
    if ( isCtor(*this) || isDtor(*this) ) {
        if ( myReturnExpression ) {
            ctx.error(*myReturnExpression) << "ctor/dtor cannot have a return type";
            return;
        }

        myReturnExpression = createEmptyExpression();
    }

    if ( !myReturnExpression ) {
        ctx.error(symbol().identifier()) << "inferred return type not implemented";
        return;
    }

    auto returnSemantics = ProcedureParameter::ByValue;
    ctx.resolveExpression(myReturnExpression);
    if ( myReturnExpression->as<ReferenceExpression>() )
        returnSemantics = ProcedureParameter::ByReference;

    myResult = std::make_unique<ProcedureParameter>(Symbol(lexer::Token(lexer::TokenKind::Identifier, id.line(), id.column(), "result")), *this, returnSemantics);
    myResult->addConstraint(*myReturnExpression);

    myResult->resolveSymbols(endModule, dgn);
}

void ProcedureDeclaration::unresolvePrototypeSymbols()
{
    if ( myReturnExpression )
        clearDeclarations(*myReturnExpression);

    if ( myThisExpr )
        clearDeclarations(*myThisExpr);

    myParameters.clear();
    myOrdinals.clear();
    myResult.reset();
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
        throw std::runtime_error("procedure " + mySymbol->identifier().lexeme() + " is already defined");

    myDefinition = std::move(definition);
    myDefinition->setDeclaration(this);
}

Expression* ProcedureDeclaration::returnType()
{
    return myReturnExpression.get();
}

Expression const* ProcedureDeclaration::returnType() const
{
    return myReturnExpression.get();
}

Declaration const* ProcedureDeclaration::thisType() const
{
    if ( myThisExpr )
        return myThisExpr->declaration()->as<ProcedureParameter>()->dataType();

    return nullptr;
}

Slice<ProcedureParameter*> ProcedureDeclaration::parameters()
{
    return myParameters;
}

Slice<ProcedureParameter*> const ProcedureDeclaration::parameters() const
{
    return myParameters;
}

ProcedureParameter* ProcedureDeclaration::result()
{
    return myResult.get();
}

ProcedureParameter const* ProcedureDeclaration::result() const
{
    return myResult.get();
}

int ProcedureDeclaration::ordinal(std::size_t index) const
{
    return myOrdinals[index];
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

void ImportDeclaration::resolveSymbols(Module&, Diagnostics&)
{
    // nop
}

//
// SymbolVariable

SymbolVariable::SymbolVariable(PatternsPrototype& prototype,
                               PrimaryExpression const& primary,
                               Expression const* bindExpr)
    : Declaration(DeclKind::SymbolVariable, Symbol(primary.token()), nullptr)
    , myPrototype(&prototype)
    , myBoundExpression(bindExpr)
{
    for ( auto const& c : primary.constraints() )
        myConstraints.push_back(c);
}

SymbolVariable::SymbolVariable(PatternsPrototype& prototype,
                               PrimaryExpression const& primary)
    : SymbolVariable(prototype, primary, nullptr)
{
}

SymbolVariable::SymbolVariable(SymbolVariable const& rhs)
    : Declaration(rhs)
    , myPrototype(rhs.myPrototype)
    , myConstraints(rhs.myConstraints)
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
    swap(myPrototype, rhs.myPrototype);
    swap(myConstraints, rhs.myConstraints);
    swap(myBoundExpression, rhs.myBoundExpression);
}

void SymbolVariable::io(IStream& stream) const
{
    Declaration::io(stream);
}

IMPL_CLONE_BEGIN(SymbolVariable, Declaration, Declaration)
IMPL_CLONE_END
IMPL_CLONE_REMAP_BEGIN(SymbolVariable, Declaration)
IMPL_CLONE_REMAP(myPrototype)
IMPL_CLONE_REMAP(myConstraints)
IMPL_CLONE_REMAP(myBoundExpression)
IMPL_CLONE_REMAP_END

void SymbolVariable::resolveSymbols(Module&, Diagnostics&)
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

lexer::Token const& SymbolVariable::identifier() const
{
    return symbol().identifier();
}

PatternsPrototype const& SymbolVariable::prototype() const
{
    return *myPrototype;
}

//
// TemplateDeclaration

TemplateDeclaration::TemplateDeclaration(Symbol&& sym)
    : Declaration(DeclKind::Template, std::move(sym), nullptr)
{
}

TemplateDeclaration::TemplateDeclaration(TemplateDeclaration const& rhs)
    : Declaration(rhs)
{
}

TemplateDeclaration& TemplateDeclaration::operator = (TemplateDeclaration const& rhs)
{
    TemplateDeclaration(rhs).swap(*this);
    return *this;
}

TemplateDeclaration::~TemplateDeclaration() = default;

void TemplateDeclaration::swap(TemplateDeclaration& rhs)
{
    Declaration::swap(rhs);
    using std::swap;
    swap(myDefinition, rhs.myDefinition);
}

void TemplateDeclaration::io(IStream& stream) const
{
    Declaration::io(stream);
    if ( myDefinition )
        stream.next("definition", myDefinition);
}

IMPL_CLONE_BEGIN(TemplateDeclaration, Declaration, Declaration)
IMPL_CLONE_CHILD(myDefinition)
IMPL_CLONE_END
IMPL_CLONE_REMAP_BEGIN(TemplateDeclaration, Declaration)
IMPL_CLONE_REMAP(myDefinition)
IMPL_CLONE_REMAP_END

void TemplateDeclaration::resolveSymbols(Module& endModule, Diagnostics& dgn)
{
    if ( auto defn = definition() )
        defn->resolveSymbols(endModule, dgn);
}

TemplateScope* TemplateDeclaration::definition()
{
    return myDefinition.get();
}

TemplateScope const* TemplateDeclaration::definition() const
{
    return myDefinition.get();
}

void TemplateDeclaration::define(std::unique_ptr<TemplateScope> definition)
{
    if ( myDefinition )
        throw std::runtime_error("template " + mySymbol->identifier().lexeme() + " is already defined");

    myDefinition = std::move(definition);
    myDefinition->setDeclaration(this);
}

void TemplateDeclaration::merge(TemplateDeclaration& rhs)
{
    myDefinition->merge(*rhs.myDefinition);
    rhs.myDefinition.reset();
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

bool isMemoryDeclaration(DeclKind kind)
{
    switch ( kind ) {
    case DeclKind::Variable:
    case DeclKind::ProcedureParameter:
        return true;
    }

    return false;
}

bool isCallableDeclaration(DeclKind kind)
{
    switch ( kind ) {
    case DeclKind::DataProduct:
    case DeclKind::DataSum:
    case DeclKind::DataSumCtor:
    case DeclKind::Procedure:
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

TemplateDeclaration const* parentTemplate(ProcedureDeclaration const& proc)
{
    return proc.scope().declaration()->as<TemplateDeclaration>();
}

bool isCtor(ProcedureDeclaration const& proc)
{
    auto templ = parentTemplate(proc);
    if ( !templ )
        return false;

    return templ->symbol().identifier().lexeme() == "ctor";
}

bool isDtor(ProcedureDeclaration const& proc)
{
    auto templ = parentTemplate(proc);
    if ( !templ )
        return false;

    return templ->symbol().identifier().lexeme() == "dtor";
}

template <typename Dispatch>
struct DeclarationPrinter
{
    using result_t = std::ostream&;
    Dispatch& dispatch;
    result_t stream;

    explicit DeclarationPrinter(Dispatch& dispatch, std::ostream& stream)
        : dispatch(dispatch)
        , stream(stream)
    {
    }

    result_t declDataSum(DataSumDeclaration const& ds)
    {
        return stream << ds.symbol().identifier().lexeme();
    }

    result_t declDataSumCtor(DataSumDeclaration::Constructor const& dsCtor)
    {
        return stream << dsCtor.symbol().identifier().lexeme();
    }

    result_t declDataProduct(DataProductDeclaration const& dp)
    {
        return stream << dp.symbol().identifier().lexeme();
    }

    result_t declField(DataProductDeclaration::Field const& f)
    {
        return stream << f.symbol().identifier().lexeme();
    }

    result_t declSymbol(SymbolDeclaration const& s)
    {
        return stream << s.symbol().identifier().lexeme();
    }

    result_t declProcedure(ProcedureDeclaration const& proc)
    {
        if ( auto templ = parentTemplate(proc) )
            print(stream, templ->symbol());

        stream << "(";
        auto first = begin(proc.symbol().prototype().pattern());
        auto last = end(proc.symbol().prototype().pattern());
        if ( first != last )
            print(stream, **first);

        for ( ++first; first != last; ++first ) {
            stream << ", ";
            print(stream, **first);
        }

        stream << ")";
        return stream;
    }

    result_t declProcedureParameter(ProcedureParameter const&)
    {
        // nop
        return stream;
    }

    result_t declVariable(VariableDeclaration const& var)
    {
        return stream << var.symbol().identifier().lexeme();
    }

    result_t declImport(ImportDeclaration const& imp)
    {
        return stream << imp.symbol().identifier().lexeme();
    }

    result_t declSymbolVariable(SymbolVariable const& symVar)
    {
        return stream << symVar.symbol().identifier().lexeme();
    }

    result_t declTemplate(TemplateDeclaration const& t)
    {
        return stream << t.symbol().identifier().lexeme();
    }
};

std::ostream& print(std::ostream& stream, Declaration const& decl)
{
    ShallowApply<DeclarationPrinter> op(stream);
    return op(decl);
}

    } // namespace ast
} // namespace kyfoo
