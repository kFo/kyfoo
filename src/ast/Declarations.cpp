#include <kyfoo/ast/Declarations.hpp>

#include <kyfoo/Diagnostics.hpp>

#include <kyfoo/lexer/Scanner.hpp>

#include <kyfoo/ast/Context.hpp>
#include <kyfoo/ast/ControlFlow.hpp>
#include <kyfoo/ast/Expressions.hpp>
#include <kyfoo/ast/Fabrication.hpp>
#include <kyfoo/ast/Module.hpp>
#include <kyfoo/ast/Scopes.hpp>

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
    swap(myAttributes, rhs.myAttributes);
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

SymRes Declaration::resolveSymbols(Context& ctx)
{
    return symbol().resolveSymbols(ctx);
}

void Declaration::resolveAttributes(Context& ctx)
{
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

lexer::Token const& Declaration::token() const
{
    return mySymbol->token();
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

Slice<Statement const> Declaration::attributes() const
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
IMPL_CLONE_END
IMPL_CLONE_REMAP_BEGIN(DataSumDeclaration, Declaration)
IMPL_CLONE_REMAP(myDefinition)
IMPL_CLONE_REMAP_END

SymRes DataSumDeclaration::resolveSymbols(Context& ctx)
{
    return Declaration::resolveSymbols(ctx);
}

void DataSumDeclaration::define(DataSumScope* scope)
{
    if ( myDefinition )
        throw std::runtime_error("type declaration defined more than once");

    myDefinition = scope;
    myDefinition->setDeclaration(this);
}

DataSumScope* DataSumDeclaration::definition()
{
    return myDefinition;
}

DataSumScope const* DataSumDeclaration::definition() const
{
    return myDefinition;
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

SymRes DataSumDeclaration::Constructor::resolveSymbols(Context& ctx)
{
    auto ret = Declaration::resolveSymbols(ctx);

    for ( auto& e : myPattern )
        e->setScope(scope());

    for ( auto& e : myPattern )
        ret |= ctx.resolveDeclaration(*e);

    return ret;
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

//
// Binder

Binder::Binder(DeclKind kind,
               Symbol&& symbol,
               DeclarationScope* scope,
               std::vector<std::unique_ptr<Expression>> constraints)
    : Declaration(kind, std::move(symbol), scope)
    , myConstraints(std::move(constraints))
{
}

Binder::Binder(DeclKind kind,
               Symbol&& symbol,
               DeclarationScope* scope,
               Expression const* type)
    : Declaration(kind, std::move(symbol), scope)
    , myType(type)
{
}

Binder::Binder(Binder const& rhs)
    : Declaration(rhs)
    , myType(rhs.myType)
{
    // clone myConstraints
}

Binder::~Binder() = default;

void Binder::swap(Binder& rhs)
{
    Declaration::swap(rhs);
    using std::swap;
    swap(myConstraints, rhs.myConstraints);
    swap(myType, rhs.myType);
}

void Binder::io(IStream& stream) const
{
    Declaration::io(stream);
    stream.next("constraint", myConstraints);
}

IMPL_CLONE_BEGIN(Binder, Declaration, Declaration)
IMPL_CLONE_CHILD(myConstraints)
IMPL_CLONE_END
IMPL_CLONE_REMAP_BEGIN(Binder, Declaration)
IMPL_CLONE_REMAP(myConstraints)
IMPL_CLONE_REMAP(myType)
IMPL_CLONE_REMAP_END

SymRes Binder::resolveSymbols(Context& ctx)
{
    auto ret = Declaration::resolveSymbols(ctx);
    ret |= ctx.resolveExpressions(myConstraints);

    if ( !ret )
        return ret;

    for ( auto const& c : myConstraints ) {
        if ( auto type = dataType(*c) ) {
            if ( myType ) {
                ctx.error(*this) << "more than one type found for binder";
                return SymRes::Fail;
            }

            myType = type;
        }
    }

    if ( !myType ) {
        ctx.error(*this) << "could not deduce type from constraints";
        return SymRes::Fail;
    }

    return ret;
}

void Binder::addConstraint(std::unique_ptr<Expression> c)
{
    myConstraints.emplace_back(std::move(c));
}

void Binder::addConstraints(std::vector<std::unique_ptr<Expression>>&& exprs)
{
    move(begin(exprs), end(exprs), back_inserter(myConstraints));
}

Slice<Expression*> Binder::constraints()
{
    return myConstraints;
}

Slice<Expression const*> const Binder::constraints() const
{
    return myConstraints;
}

Expression const* Binder::type() const
{
    return myType;
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
IMPL_CLONE_END
IMPL_CLONE_REMAP_BEGIN(DataProductDeclaration, Declaration)
IMPL_CLONE_REMAP(myDefinition)
IMPL_CLONE_REMAP_END

SymRes DataProductDeclaration::resolveSymbols(Context& ctx)
{
    return Declaration::resolveSymbols(ctx);
}

void DataProductDeclaration::define(DataProductScope* scope)
{
    if ( myDefinition )
        throw std::runtime_error("type declaration defined more than once");

    myDefinition = scope;
    myDefinition->setDeclaration(this);
}

DataProductScope* DataProductDeclaration::definition()
{
    return myDefinition;
}

DataProductScope const* DataProductDeclaration::definition() const
{
    return myDefinition;
}

//
// DataProductDeclaration::Field

DataProductDeclaration::Field::Field(Symbol&& symbol,
                                     std::vector<std::unique_ptr<Expression>> constraints,
                                     std::unique_ptr<Expression> init)
    : Binder(DeclKind::Field, std::move(symbol), nullptr, std::move(constraints))
    , myInitializer(std::move(init))
{
}

DataProductDeclaration::Field::Field(Field const& rhs)
    : Binder(rhs)
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
    Binder::swap(rhs);
    using std::swap;
    swap(myParent, rhs.myParent);
}

void DataProductDeclaration::Field::io(IStream& stream) const
{
    Binder::io(stream);
}

IMPL_CLONE_BEGIN(DataProductDeclaration::Field, Binder, Declaration)
IMPL_CLONE_CHILD(myInitializer)
IMPL_CLONE_END
IMPL_CLONE_REMAP_BEGIN(DataProductDeclaration::Field, Binder)
IMPL_CLONE_REMAP(myParent)
IMPL_CLONE_REMAP(myInitializer)
IMPL_CLONE_REMAP_END

SymRes DataProductDeclaration::Field::resolveSymbols(Context& ctx)
{
    ScopeResolver resolver(*myParent->definition());
    ctx.changeResolver(resolver);
    
    auto ret = Binder::resolveSymbols(ctx);
    if ( !ret )
        return ret;

    if ( myInitializer )
        ret |= ctx.resolveExpression(myInitializer);

    return ret;
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

SymRes SymbolDeclaration::resolveSymbols(Context& ctx)
{
    auto ret = Declaration::resolveSymbols(ctx);

    ScopeResolver resolver(scope());
    resolver.addSupplementaryPrototype(symbol().prototype());

    ctx.changeResolver(resolver);
    ret |= ctx.resolveExpression(myExpression);
    return ret;
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
                                         std::vector<std::unique_ptr<Expression>> constraints)
    : Binder(DeclKind::Variable, std::move(symbol), &scope, std::move(constraints))
{
}

VariableDeclaration::VariableDeclaration(Symbol&& symbol,
                                         ProcedureScope& scope,
                                         Expression const& type)
    : Binder(DeclKind::Variable, std::move(symbol), &scope, &type)
{
}

VariableDeclaration::VariableDeclaration(VariableDeclaration const& rhs)
    : Binder(rhs)
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
    Binder::swap(rhs);
}

void VariableDeclaration::io(IStream& stream) const
{
    Binder::io(stream);
}

IMPL_CLONE_BEGIN(VariableDeclaration, Binder, Declaration)
IMPL_CLONE_END
IMPL_CLONE_REMAP_BEGIN(VariableDeclaration, Binder)
IMPL_CLONE_REMAP_END

SymRes VariableDeclaration::resolveSymbols(Context& ctx)
{
    return Binder::resolveSymbols(ctx);
}

//
// ProcedureParameter

ProcedureParameter::ProcedureParameter(Symbol&& symbol,
                                       ProcedureDeclaration& proc,
                                       std::vector<std::unique_ptr<Expression>>&& constraints,
                                       PassSemantics passSemantics)
    : Binder(DeclKind::ProcedureParameter, std::move(symbol), &proc.scope(), std::move(constraints))
    , myPassSemantics(passSemantics)
{
}

ProcedureParameter::ProcedureParameter(Symbol&& symbol,
                                       ProcedureDeclaration& proc,
                                       Expression const* type,
                                       PassSemantics passSemantics)
    : Binder(DeclKind::ProcedureParameter, std::move(symbol), &proc.scope(), type)
    , myPassSemantics(passSemantics)
{
}

ProcedureParameter::ProcedureParameter(ProcedureParameter const& rhs)
    : Binder(rhs)
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
    Binder::swap(rhs);
    using std::swap;
    swap(myPassSemantics, rhs.myPassSemantics);
}

void ProcedureParameter::io(IStream& stream) const
{
    Binder::io(stream);
}

IMPL_CLONE_BEGIN(ProcedureParameter, Binder, Declaration)
IMPL_CLONE_END
IMPL_CLONE_REMAP_BEGIN(ProcedureParameter, Binder)
IMPL_CLONE_REMAP_END

SymRes ProcedureParameter::resolveSymbols(Context& ctx)
{
    return Binder::resolveSymbols(ctx);
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
IMPL_CLONE_CHILD(myType)
IMPL_CLONE_CHILD(myReturnExpression)
IMPL_CLONE_CHILD(myParameters)
IMPL_CLONE_CHILD(myResult)
IMPL_CLONE_END
IMPL_CLONE_REMAP_BEGIN(ProcedureDeclaration, Declaration)
IMPL_CLONE_REMAP(myType)
IMPL_CLONE_REMAP(myReturnExpression)
IMPL_CLONE_REMAP(myParameters)
IMPL_CLONE_REMAP(myResult)
IMPL_CLONE_REMAP(myDefinition)
IMPL_CLONE_REMAP_END

SymRes ProcedureDeclaration::resolveSymbols(Context& ctx)
{
    // todo: hack
    mySymbol->prototype().resolveVariables(scope());

    // deduce parameters
    auto const& id = symbol().token();
    if ( myParameters.empty() ) {
        if ( auto thisType = outerDataDeclaration(*this) ) {
            auto thisToken = lexer::Token(lexer::TokenKind::Identifier, id.line(), id.column(), "this");
            myParameters.emplace_back(
                std::make_unique<ProcedureParameter>(
                    Symbol(thisToken),
                    *this,
                    createPtrList<Expression>(std::make_unique<IdentifierExpression>(lexer::Token(lexer::TokenKind::Identifier, id.line(), id.column(), "this_t"),
                                                                                     *thisType)),
                    ProcedureParameter::ByReference));
        }

        for ( int i = 0; i < mySymbol->prototype().pattern().size(); ++i ) {
            auto& pattern = mySymbol->prototype().pattern()[i];
            auto expr = pattern;
            if ( auto a = pattern->as<ApplyExpression>() ) {
                if ( a->expressions().size() != 1 )
                    continue;

                expr = a->expressions()[0];
            }

            auto passSemantics = ProcedureParameter::ByValue;
            auto p = expr->as<IdentifierExpression>();
            if ( !p || p->token().kind() != lexer::TokenKind::Identifier )
                continue;

            auto hit = ctx.matchOverload(p->token().lexeme());
            if ( !hit ) {
                myOrdinals.push_back(static_cast<int>(myParameters.size()));
                myParameters.emplace_back(
                    std::make_unique<ProcedureParameter>(Symbol(p->token()),
                                                         *this,
                                                         pattern->takeConstraints(),
                                                         passSemantics));
                p->setDeclaration(*myParameters.back());
            }
            else {
                myOrdinals.push_back(-1);
            }
        }
    }

    SymRes ret;
    {
        ScopeResolver resolver(ctx.resolver().scope());
        resolver.addSupplementaryPrototype(mySymbol->prototype());
        auto save = ctx.changeResolver(resolver);
        for ( auto& p : myParameters )
            ret |= p->resolveSymbols(ctx);
        ctx.changeResolver(*save);
    }

    if ( !ret )
        return ret;

    ret |= mySymbol->resolveSymbols(ctx);
    if ( !ret )
        return ret;

    ScopeResolver resolver(scope());
    resolver.addSupplementaryPrototype(mySymbol->prototype());
    ctx.changeResolver(resolver);

    // Resolve return
    // todo: return type deduction
    if ( isCtor(*this) || isDtor(*this) ) {
        if ( myReturnExpression ) {
            ctx.error(*myReturnExpression) << "ctor/dtor cannot have a return type";
            return SymRes::Fail;
        }

        myReturnExpression = createEmptyExpression();
    }

    if ( !myReturnExpression ) {
        ctx.error(symbol().token()) << "inferred return type not implemented";
        return SymRes::Fail;
    }

    auto returnSemantics = ProcedureParameter::ByValue;
    ret |= ctx.resolveExpression(myReturnExpression);

    myResult = std::make_unique<ProcedureParameter>(
        Symbol(lexer::Token(lexer::TokenKind::Identifier, id.line(), id.column(), "result")),
        *this,
        flattenConstraints(ast::clone(myReturnExpression)),
        returnSemantics);

    ret |= ctx.resolveDeclaration(*myResult);
    if ( !ret )
        return ret;

    std::vector<std::unique_ptr<Expression>> paramTypes;
    paramTypes.reserve(myParameters.size());
    for ( auto const& p : myParameters )
        paramTypes.emplace_back(ast::clone(p->type()));

    myType = std::make_unique<ArrowExpression>(std::make_unique<TupleExpression>(TupleKind::Open, std::move(paramTypes)),
                                               ast::clone(myResult->type()));
    ret |= ctx.resolveExpression(*myType);

    return ret;
}

ProcedureScope* ProcedureDeclaration::definition()
{
    return myDefinition;
}

ProcedureScope const* ProcedureDeclaration::definition() const
{
    return myDefinition;
}

void ProcedureDeclaration::define(ProcedureScope* definition)
{
    if ( myDefinition )
        throw std::runtime_error("procedure " + mySymbol->token().lexeme() + " is already defined");

    myDefinition = definition;
    myDefinition->setDeclaration(this);
}

ArrowExpression const* ProcedureDeclaration::type() const
{
    return myType.get();
}

Expression* ProcedureDeclaration::returnType()
{
    return myReturnExpression.get();
}

Expression const* ProcedureDeclaration::returnType() const
{
    return myReturnExpression.get();
}

Slice<ProcedureParameter*> ProcedureDeclaration::parameters()
{
    return myParameters;
}

Slice<ProcedureParameter const*> ProcedureDeclaration::parameters() const
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

ProcedureParameter* ProcedureDeclaration::findParameter(std::string const& token)
{
    for ( auto const& p : myParameters )
        if ( p->symbol().token().lexeme() == token )
            return p.get();

    return nullptr;
}

ProcedureParameter const* ProcedureDeclaration::findParameter(std::string const& token) const
{
    return const_cast<ProcedureDeclaration*>(this)->findParameter(token);
}

//
// ImportDeclaration

ImportDeclaration::ImportDeclaration(Symbol&& sym)
    : Declaration(DeclKind::Import, std::move(sym), nullptr)
{
    myModulePath.push_back(symbol().token());
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

SymRes ImportDeclaration::resolveSymbols(Context& ctx)
{
    return Declaration::resolveSymbols(ctx);
}

//
// SymbolVariable

SymbolVariable::SymbolVariable(IdentifierExpression const& id,
                               DeclarationScope* scope,
                               PatternsPrototype& prototype,
                               Expression const* bindExpr)
    : Declaration(DeclKind::SymbolVariable, Symbol(id.token()), scope)
    , myPrototype(&prototype)
    , myBoundExpression(bindExpr)
{
    for ( auto const& c : id.constraints() )
        myConstraints.push_back(c);
}

SymbolVariable::SymbolVariable(IdentifierExpression const& id,
                               DeclarationScope* scope,
                               PatternsPrototype& prototype)
    : SymbolVariable(id, scope, prototype, nullptr)
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

SymRes SymbolVariable::resolveSymbols(Context&)
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

lexer::Token const& SymbolVariable::token() const
{
    return symbol().token();
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
IMPL_CLONE_END
IMPL_CLONE_REMAP_BEGIN(TemplateDeclaration, Declaration)
IMPL_CLONE_REMAP(myDefinition)
IMPL_CLONE_REMAP_END

SymRes TemplateDeclaration::resolveSymbols(Context& ctx)
{
    return Declaration::resolveSymbols(ctx);
}

TemplateScope* TemplateDeclaration::definition()
{
    return myDefinition;
}

TemplateScope const* TemplateDeclaration::definition() const
{
    return myDefinition;
}

void TemplateDeclaration::define(TemplateScope* definition)
{
    if ( myDefinition )
        throw std::runtime_error("template " + mySymbol->token().lexeme() + " is already defined");

    myDefinition = definition;
    myDefinition->setDeclaration(this);
}

void TemplateDeclaration::merge(TemplateDeclaration& rhs)
{
    myDefinition->merge(*rhs.myDefinition);
    rhs.myDefinition = nullptr;
}

//
// Utilities

bool isDataDeclaration(DeclKind kind)
{
    switch (kind) {
    case DeclKind::DataProduct:
    case DeclKind::DataSum:
        return true;
    }

    return false;
}

bool isBinder(DeclKind kind)
{
    switch ( kind ) {
    case DeclKind::Field:
    case DeclKind::ProcedureParameter:
    case DeclKind::Variable:
        return true;
    }

    return false;
}

Binder const* getBinder(Declaration const& decl)
{
    if ( isBinder(decl.kind()) )
        return static_cast<Binder const*>(&decl);

    return nullptr;
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

bool hasIndirection(Expression const& expr)
{
    if ( auto id = identify(expr) )
        return hasIndirection(id->declaration()->kind());

    return false;
}

Expression const* getType(Declaration const& decl)
{
    if ( auto d = decl.as<DataSumDeclaration>() )
        return &Expression::universe(1);

    if ( auto d = decl.as<DataSumDeclaration::Constructor>() )
        return &Expression::universe(1);

    if ( auto d = decl.as<DataProductDeclaration>() )
        return &Expression::universe(1);

    if ( auto d = decl.as<DataProductDeclaration::Field>() )
        return d->type();

    if ( auto d = decl.as<SymbolDeclaration>() )
        return d->expression()->type();

    if ( auto d = decl.as<ProcedureDeclaration>() )
        return d->type();

    if ( auto d = decl.as<ProcedureParameter>() )
        return d->type();

    if ( auto d = decl.as<VariableDeclaration>() )
        return d->type();

    if ( auto d = decl.as<SymbolVariable>() ) {
        auto expr = d->boundExpression();
        if ( !expr )
            return nullptr;

        return expr->type();
    }

    if ( auto d = decl.as<TemplateDeclaration>() )
        return &Expression::universe(0);

    return nullptr;
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

    return templ->symbol().token().lexeme() == "ctor";
}

bool isDtor(ProcedureDeclaration const& proc)
{
    auto templ = parentTemplate(proc);
    if ( !templ )
        return false;

    return templ->symbol().token().lexeme() == "dtor";
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
        return stream << ds.symbol().token().lexeme();
    }

    result_t declDataSumCtor(DataSumDeclaration::Constructor const& dsCtor)
    {
        return stream << dsCtor.symbol().token().lexeme();
    }

    result_t declDataProduct(DataProductDeclaration const& dp)
    {
        return stream << dp.symbol().token().lexeme();
    }

    result_t declField(DataProductDeclaration::Field const& f)
    {
        return stream << f.symbol().token().lexeme();
    }

    result_t declSymbol(SymbolDeclaration const& s)
    {
        return stream << s.symbol().token().lexeme();
    }

    result_t declProcedure(ProcedureDeclaration const& proc)
    {
        if ( auto templ = parentTemplate(proc) )
            print(stream, templ->symbol());

        auto sink = [this](Expression const& e) {
            if ( auto id = e.as<IdentifierExpression>() ) {
                if ( auto param = id->declaration()->as<ProcedureParameter>() ) {
                    declProcedureParameter(*param);
                    return;
                }
            }

            print(stream, e);
        };

        stream << "(";
        auto first = begin(proc.symbol().prototype().pattern());
        auto last = end(proc.symbol().prototype().pattern());
        if ( first != last )
            sink(**first);

        for ( ++first; first != last; ++first ) {
            stream << ", ";
            sink(**first);
        }

        stream << ")";
        
        if ( proc.returnType() ) {
            stream << " -> ";
            sink(*proc.returnType());
        }

        return stream;
    }

    result_t declProcedureParameter(ProcedureParameter const& p)
    {
        stream << p.token().lexeme();
        for ( auto const& c : p.constraints() ) {
            stream << " : ";
            print(stream, *c);
        }
        return stream;
    }

    result_t declVariable(VariableDeclaration const& var)
    {
        stream << var.symbol().token().lexeme();
        for ( auto const& c : var.constraints() ) {
            stream << " : ";
            print(stream, *c);
        }
        return stream;
    }

    result_t declImport(ImportDeclaration const& imp)
    {
        return stream << imp.symbol().token().lexeme();
    }

    result_t declSymbolVariable(SymbolVariable const& symVar)
    {
        return stream << symVar.symbol().token().lexeme();
    }

    result_t declTemplate(TemplateDeclaration const& t)
    {
        return stream << t.symbol().token().lexeme();
    }
};

std::ostream& print(std::ostream& stream, Declaration const& decl)
{
    ShallowApply<DeclarationPrinter> op(stream);
    return op(decl);
}

    } // namespace ast
} // namespace kyfoo
