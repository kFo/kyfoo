#include <kyfoo/ast/Declarations.hpp>

#include <kyfoo/Diagnostics.hpp>
#include <kyfoo/String.hpp>
#include <kyfoo/Utilities.hpp>

#include <kyfoo/lexer/Scanner.hpp>

#include <kyfoo/ast/Context.hpp>
#include <kyfoo/ast/ControlFlow.hpp>
#include <kyfoo/ast/Expressions.hpp>
#include <kyfoo/ast/Fabrication.hpp>
#include <kyfoo/ast/Module.hpp>
#include <kyfoo/ast/Overloading.hpp>
#include <kyfoo/ast/Scopes.hpp>
#include <kyfoo/ast/Visitors.hpp>

namespace kyfoo {
    namespace ast {

//
// Declaration

const char* to_string(Declaration::Kind kind)
{
    static const char* s[] = {
#define X(a,b,c) b,
        DECLARATION_KINDS(X)
#undef X
    };

    return s[static_cast<uz>(kind)];
}

Declaration::Declaration(Declaration::Kind kind,
                         Symbol symbol,
                         Scope* scope)
    : myKind(kind)
    , mySymbol(mk<Symbol>(std::move(symbol)))
    , myScope(scope)
{
}

Declaration::Declaration(Declaration const& rhs)
    : myKind(rhs.myKind)
    , myScope(rhs.myScope)
{
}

Declaration::~Declaration() = default;

void Declaration::swap(Declaration& rhs) noexcept
{
    using kyfoo::swap;
    swap(myKind, rhs.myKind);
    swap(mySymbol, rhs.mySymbol);
    swap(myScope, rhs.myScope);
    swap(myAttributes, rhs.myAttributes);
    // myCodeGenData does not get copied
}

IMPL_CLONE_NOBASE_BEGIN(Declaration, Declaration)
IMPL_CLONE_CHILD(mySymbol)
IMPL_CLONE_END
IMPL_CLONE_REMAP_NOBASE_BEGIN(Declaration)
IMPL_CLONE_REMAP(mySymbol)
IMPL_CLONE_REMAP(myScope)
IMPL_CLONE_REMAP_END

SymRes Declaration::resolveSymbols(Context& ctx)
{
    return symbol().resolveSymbols(ctx);
}

SymRes Declaration::resolveAttributes(Context& ctx)
{
    SymRes ret;
    for ( auto& attr : myAttributes )
        ret |= ctx.resolveStatement(attr);

    return ret;
}

Declaration::Kind Declaration::kind() const
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

Scope& Declaration::scope()
{
    return *myScope;
}

Scope const& Declaration::scope() const
{
    return *myScope;
}

void Declaration::setScope(Scope& scope)
{
    ENFORCE(!myScope || myScope == &scope || myScope->declaration()->kind() == Declaration::Kind::Template,
            "declaration parent set twice");

    myScope = &scope;
}

void Declaration::setAttributes(std::vector<Box<Expression>> exprs)
{
    for ( auto& e : exprs )
        myAttributes.emplace_back(std::move(e));

    exprs.clear();
}

Slice<ExpressionStatement const> Declaration::attributes() const
{
    return myAttributes;
}

Slice<ExpressionStatement> Declaration::attributes()
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

void Declaration::setCodegenData(Box<codegen::CustomData> data)
{
    ENFORCE(!codegenData(), "codegen data can only be set once");

    myCodeGenData = std::move(data);
}

void Declaration::setCodegenData(Box<codegen::CustomData> data) const
{
    return const_cast<Declaration*>(this)->setCodegenData(std::move(data));
}

//
// DefinableDeclaration

DefinableDeclaration::DefinableDeclaration(Declaration::Kind kind, Symbol symbol)
    : Declaration(kind, std::forward<Symbol>(symbol), nullptr)
{
}

DefinableDeclaration::DefinableDeclaration(DefinableDeclaration const&) = default;

DefinableDeclaration::~DefinableDeclaration() = default;

void DefinableDeclaration::swap(DefinableDeclaration& rhs) noexcept
{
    Declaration::swap(rhs);
    using kyfoo::swap;
    swap(myDefinition, rhs.myDefinition);
}

IMPL_CLONE_BEGIN(DefinableDeclaration, Declaration, Declaration)
IMPL_CLONE_END
IMPL_CLONE_REMAP_BEGIN(DefinableDeclaration, Declaration)
IMPL_CLONE_REMAP(myDefinition)
IMPL_CLONE_REMAP_END

SymRes DefinableDeclaration::resolveSymbols(Context& ctx)
{
    return Declaration::resolveSymbols(ctx);
}

Scope* DefinableDeclaration::definition()
{
    return myDefinition;
}

Scope const* DefinableDeclaration::definition() const
{
    return myDefinition;
}

//
// Binder

Binder::Binder(Declaration::Kind kind,
               Symbol symbol,
               Scope* scope,
               std::vector<Box<Expression>> constraints)
    : Declaration(kind, std::move(symbol), scope)
    , myConstraints(std::move(constraints))
{
}

Binder::Binder(Declaration::Kind kind,
               Symbol symbol,
               Scope* scope,
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

void Binder::swap(Binder& rhs) noexcept
{
    Declaration::swap(rhs);
    using kyfoo::swap;
    swap(myConstraints, rhs.myConstraints);
    swap(myType, rhs.myType);
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

    if ( !ret || myType )
        return ret;

    for ( auto const& c : myConstraints ) {
        if ( auto type = dataType(*c) ) {
            if ( myType ) {
                ctx.error(diag::expected_one_type, *this);
                return SymRes::Fail;
            }

            myType = type;
        }
    }

    return ret;
}

void Binder::appendConstraint(Box<Expression> c)
{
    myConstraints.emplace_back(std::move(c));
}

void Binder::appendConstraints(std::vector<Box<Expression>> exprs)
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
// Field

Field::Field(DataTypeDeclaration& parent,
             Symbol symbol,
             std::vector<Box<Expression>> constraints,
             Box<Expression> init)
    : Binder(Declaration::Kind::Field, std::move(symbol), nullptr, std::move(constraints))
    , myParent(&parent)
    , myInitializer(std::move(init))
{
}

Field::Field(Field const& rhs)
    : Binder(rhs)
    , myParent(rhs.myParent)
{
    // clone myExpression
}

Field& Field::operator = (Field const& rhs)
{
    Field(rhs).swap(*this);
    return *this;
}

Field::~Field() = default;

void Field::swap(Field& rhs) noexcept
{
    Binder::swap(rhs);
    using kyfoo::swap;
    swap(myParent, rhs.myParent);
}

IMPL_CLONE_BEGIN(Field, Binder, Declaration)
IMPL_CLONE_CHILD(myInitializer)
IMPL_CLONE_END
IMPL_CLONE_REMAP_BEGIN(Field, Binder)
IMPL_CLONE_REMAP(myParent)
IMPL_CLONE_REMAP(myInitializer)
IMPL_CLONE_REMAP_END

SymRes Field::resolveSymbols(Context& ctx)
{
    Resolver resolver(*myParent->definition());
    REVERT = ctx.pushResolver(resolver);
    
    auto ret = Binder::resolveSymbols(ctx);
    if ( !ret )
        return ret;

    if ( myInitializer )
        ret |= ctx.resolveExpression(myInitializer);

    return ret;
}

DataTypeDeclaration* Field::parent()
{
    return myParent;
}

DataTypeDeclaration const* Field::parent() const
{
    return myParent;
}

//
// DataTypeDeclaration

DataTypeDeclaration::DataTypeDeclaration(Symbol symbol, DataTypeDeclaration const* super)
    : Base(Declaration::Kind::DataType, std::move(symbol))
    , mySuper(super)
{
}

DataTypeDeclaration::DataTypeDeclaration(DataTypeDeclaration const&) = default;

DataTypeDeclaration::~DataTypeDeclaration() = default;

void DataTypeDeclaration::swap(DataTypeDeclaration& rhs) noexcept
{
    Base::swap(rhs);
    using kyfoo::swap;
    swap(myDefinition, rhs.myDefinition);
}

IMPL_CLONE_BEGIN(DataTypeDeclaration, Base, Declaration)
IMPL_CLONE_END
IMPL_CLONE_REMAP_BEGIN(DataTypeDeclaration, Base)
IMPL_CLONE_REMAP(myDefinition)
IMPL_CLONE_REMAP_END

SymRes DataTypeDeclaration::resolveSymbols(Context& ctx)
{
    return Base::resolveSymbols(ctx);
}

DataTypeDeclaration const* DataTypeDeclaration::super() const
{
    return mySuper;
}

//
// SymbolDeclaration

SymbolDeclaration::SymbolDeclaration(Symbol symbol,
                                     Box<Expression> expression)
    : Declaration(Declaration::Kind::Symbol, std::move(symbol), nullptr)
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

void SymbolDeclaration::swap(SymbolDeclaration& rhs) noexcept
{
    Declaration::swap(rhs);
    using kyfoo::swap;
    swap(myExpression, rhs.myExpression);
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

    Resolver resolver(scope());
    resolver.addSupplementaryPrototype(symbol().prototype());

    REVERT = ctx.changeResolver(resolver);
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

VariableDeclaration::VariableDeclaration(Symbol symbol,
                                         ProcedureScope& scope,
                                         std::vector<Box<Expression>> constraints)
    : Binder(Declaration::Kind::Variable, std::move(symbol), &scope, std::move(constraints))
{
}

VariableDeclaration::VariableDeclaration(Symbol symbol,
                                         ProcedureScope& scope,
                                         Expression const& type)
    : Binder(Declaration::Kind::Variable, std::move(symbol), &scope, &type)
{
}

VariableDeclaration::VariableDeclaration(VariableDeclaration const& rhs)
    : Binder(rhs)
{
    // clone myTypeAsRef
}

VariableDeclaration& VariableDeclaration::operator = (VariableDeclaration const& rhs)
{
    VariableDeclaration(rhs).swap(*this);
    return *this;
}

VariableDeclaration::~VariableDeclaration() = default;

void VariableDeclaration::swap(VariableDeclaration& rhs) noexcept
{
    Binder::swap(rhs);
    using kyfoo::swap;
    swap(myTypeAsRef, rhs.myTypeAsRef);
}

IMPL_CLONE_BEGIN(VariableDeclaration, Binder, Declaration)
IMPL_CLONE_CHILD(myTypeAsRef)
IMPL_CLONE_END
IMPL_CLONE_REMAP_BEGIN(VariableDeclaration, Binder)
IMPL_CLONE_REMAP(myTypeAsRef)
IMPL_CLONE_REMAP_END

SymRes VariableDeclaration::resolveSymbols(Context& ctx)
{
    auto ret = Binder::resolveSymbols(ctx);
    if ( !ret )
        return ret;

    if ( myType ) {
        myTypeAsRef = createRefType(front(*myType).location(), clone(myType));
        ret |= ctx.resolveExpression(myTypeAsRef);
        myType = myTypeAsRef.get();
    }

    return ret;
}

//
// ProcedureParameter

ProcedureParameter::ProcedureParameter(Symbol symbol,
                                       ProcedureDeclaration& proc,
                                       std::vector<Box<Expression>> constraints)
    : Binder(Declaration::Kind::ProcedureParameter, std::move(symbol), &proc.scope(), std::move(constraints))
{
}

ProcedureParameter::ProcedureParameter(Symbol symbol,
                                       ProcedureDeclaration& proc,
                                       Expression const* type)
    : Binder(Declaration::Kind::ProcedureParameter, std::move(symbol), &proc.scope(), type)
{
}

ProcedureParameter::ProcedureParameter(ProcedureParameter const& rhs) = default;

ProcedureParameter& ProcedureParameter::operator = (ProcedureParameter const& rhs)
{
    ProcedureParameter(rhs).swap(*this);
    return *this;
}

ProcedureParameter::~ProcedureParameter() = default;

void ProcedureParameter::swap(ProcedureParameter& rhs) noexcept
{
    Binder::swap(rhs);
}

IMPL_CLONE_BEGIN(ProcedureParameter, Binder, Declaration)
IMPL_CLONE_END
IMPL_CLONE_REMAP_BEGIN(ProcedureParameter, Binder)
IMPL_CLONE_REMAP_END

SymRes ProcedureParameter::resolveSymbols(Context& ctx)
{
    auto ret = Binder::resolveSymbols(ctx);
    if ( !ret )
        return ret;

    if ( !myType ) {
        ctx.error(diag::no_deduction, *this);
        return SymRes::Fail;
    }

    return ret;
}

//
// ProcedureDeclaration

ProcedureDeclaration::ProcedureDeclaration(Symbol symbol,
                                           Box<Expression> returnExpression)
    : Base(Declaration::Kind::Procedure, std::move(symbol))
    , myReturnExpression(std::move(returnExpression))
{
}

ProcedureDeclaration::ProcedureDeclaration(ProcedureDeclaration const& rhs)
    : Base(rhs)
    , myOrdinals(rhs.myOrdinals)
{
}

ProcedureDeclaration& ProcedureDeclaration::operator = (ProcedureDeclaration const& rhs)
{
    ProcedureDeclaration(rhs).swap(*this);
    return *this;
}

ProcedureDeclaration::~ProcedureDeclaration() = default;

void ProcedureDeclaration::swap(ProcedureDeclaration& rhs) noexcept
{
    Base::swap(rhs);
    using kyfoo::swap;
    swap(myReturnExpression, rhs.myReturnExpression);
    swap(myParameters, rhs.myParameters);
    swap(myOrdinals, rhs.myOrdinals);
    swap(myResult, rhs.myResult);
    swap(myDefinition, rhs.myDefinition);
}

IMPL_CLONE_BEGIN(ProcedureDeclaration, Base, Declaration)
IMPL_CLONE_CHILD(myType)
IMPL_CLONE_CHILD(myReturnExpression)
IMPL_CLONE_CHILD(myParameters)
IMPL_CLONE_CHILD(myResult)
IMPL_CLONE_END
IMPL_CLONE_REMAP_BEGIN(ProcedureDeclaration, Base)
IMPL_CLONE_REMAP(myType)
IMPL_CLONE_REMAP(myReturnExpression)
IMPL_CLONE_REMAP(myParameters)
IMPL_CLONE_REMAP(myResult)
IMPL_CLONE_REMAP(myDefinition)
IMPL_CLONE_REMAP_END

SymRes ProcedureDeclaration::resolveSymbols(Context& ctx)
{
    /* todo(hack):
        Type binder declarations need to be resolved first due to the following ast edits
        that may or may not clone portions of the ast. Cloning an expression that 
        introduces a declaration will result in a multiple_declaration error otherwise.
    */
    {
        Resolver resolver(scope());
        REVERT = ctx.changeResolver(resolver);
        mySymbol->prototype().resolveVariables(ctx);
    }

    // deduce parameters
    auto const& id = symbol().token();
    if ( myParameters.empty() ) {
        auto canBeParam = [&ctx](Expression const& e) {
            auto p = e.as<IdentifierExpression>();
            if ( !p || p->token().kind() != lexer::TokenKind::Identifier )
                return false;

            if ( ctx.matchOverload(p->token().lexeme()) )
                return false;

            return true;
        };

        for ( uz i = 0; i < mySymbol->prototype().pattern().card(); ++i ) {
            auto& protoPattern = mySymbol->prototype().myPattern;
            while ( TupleExpression::tryExpandTuple(protoPattern, next(begin(protoPattern), i)) )
                continue;

            if ( auto app = mySymbol->prototype().pattern()[i]->as<ApplyExpression>() ) {
                auto a = std::move(reinterpret_cast<Box<ApplyExpression>&>(protoPattern[i]));
                protoPattern[i] = std::move(a->myExpressions[0]);
                for ( uz j = 1; j < a->myExpressions.size(); ++j ) {
                    protoPattern.emplace(begin(protoPattern) + i + j, std::move(a->myExpressions[j]));
                    protoPattern[i + j]->appendConstraints(ast::clone(a->constraints()));
                }
                protoPattern[i]->appendConstraints(a->takeConstraints());
            }

            auto& e = *mySymbol->prototype().pattern()[i];
            if ( canBeParam(e) ) {
                auto p = e.as<IdentifierExpression>();
                myOrdinals.push_back(static_cast<int>(myParameters.size()));
                myParameters.emplace_back(
                    mk<ProcedureParameter>(Symbol(p->token()),
                                           *this,
                                           e.takeConstraints()));
                p->setDeclaration(*myParameters.back());
            }
            else {
                myOrdinals.push_back(-1);
            }
        }
    }

    SymRes ret;
    {
        Resolver resolver(ctx.resolver().scope());
        resolver.addSupplementaryPrototype(mySymbol->prototype());
        REVERT = ctx.pushResolver(resolver);
        for ( auto& p : myParameters )
            ret |= p->resolveSymbols(ctx);
    }

    if ( !ret )
        return ret;

    ret |= mySymbol->resolveSymbols(ctx);
    if ( !ret )
        return ret;

    Resolver resolver(scope());
    resolver.addSupplementaryPrototype(mySymbol->prototype());
    REVERT = ctx.pushResolver(resolver);

    // Resolve return

    if ( !myReturnExpression )
        myReturnExpression = createEmptyExpression(symbol().token().location());

    ret |= ctx.resolveExpression(myReturnExpression);
    if ( !ret )
        return ret;

    myResult = mk<ProcedureParameter>(
        Symbol(lexer::Token(lexer::TokenKind::Identifier, "result", id.location())),
        *this,
        flattenConstraints(ast::clone(myReturnExpression)));

    ret |= ctx.resolveDeclaration(*myResult);
    if ( !ret )
        return ret;

    std::vector<Box<Expression>> paramTypes;
    paramTypes.reserve(myParameters.size());
    for ( auto const& p : myParameters )
        paramTypes.emplace_back(ast::clone(p->type()));

    myType = mk<ArrowExpression>(mk<TupleExpression>(TupleKind::Open, std::move(paramTypes)),
                                 ast::clone(myResult->type()));
    ret |= ctx.resolveExpression(*myType);

    return ret;
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

Slice<int const> ProcedureDeclaration::ordinals() const
{
    return myOrdinals;
}

ProcedureParameter* ProcedureDeclaration::findParameter(stringv token)
{
    for ( auto const& p : myParameters )
        if ( p->symbol().token().lexeme() == token )
            return p.get();

    return nullptr;
}

ProcedureParameter const* ProcedureDeclaration::findParameter(stringv token) const
{
    return const_cast<ProcedureDeclaration*>(this)->findParameter(token);
}

//
// ImportDeclaration

ImportDeclaration::ImportDeclaration(Symbol sym)
    : Declaration(Declaration::Kind::Import, std::move(sym), nullptr)
{
    myModulePath.push_back(symbol().token());
}

ImportDeclaration::ImportDeclaration(std::vector<lexer::Token>&& modulePath)
    : Declaration(Declaration::Kind::Import, Symbol(modulePath.back()), nullptr)
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

void ImportDeclaration::swap(ImportDeclaration& rhs) noexcept
{
    Declaration::swap(rhs);
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

SymbolVariable::SymbolVariable(lexer::Token tok,
                               Scope& scope,
                               std::vector<Box<Expression>> constraints,
                               Expression const& expr)
    : SymbolVariable(std::move(tok), &scope, std::move(constraints), &expr)
{
}

SymbolVariable::SymbolVariable(lexer::Token tok,
                               Scope& scope,
                               std::vector<Box<Expression>> constraints)
    : SymbolVariable(std::move(tok), &scope, std::move(constraints), nullptr)
{
}

SymbolVariable::SymbolVariable(lexer::Token tok,
                               Scope* scope,
                               std::vector<Box<Expression>> constraints,
                               Expression const* expr)
    : Binder(Declaration::Kind::SymbolVariable,
             Symbol(std::move(tok)),
             scope,
             std::move(constraints))
    , myBoundExpression(expr)
{
}

SymbolVariable::SymbolVariable(SymbolVariable const& rhs) = default;

SymbolVariable& SymbolVariable::operator = (SymbolVariable const& rhs)
{
    SymbolVariable(rhs).swap(*this);
    return *this;
}

SymbolVariable::~SymbolVariable() = default;

void SymbolVariable::swap(SymbolVariable& rhs) noexcept
{
    Binder::swap(rhs);
    using kyfoo::swap;
    swap(myBoundExpression, rhs.myBoundExpression);
}

IMPL_CLONE_BEGIN(SymbolVariable, Binder, Declaration)
IMPL_CLONE_END
IMPL_CLONE_REMAP_BEGIN(SymbolVariable, Declaration)
IMPL_CLONE_REMAP(myBoundExpression)
IMPL_CLONE_REMAP_END

SymRes SymbolVariable::resolveSymbols(Context&)
{
    ENFORCEU("symbol variables should not be resolved");
}

void SymbolVariable::bindExpression(Expression const* expr)
{
    ENFORCE(!myBoundExpression || myBoundExpression == expr, "symbol variable is already bound to an expression");

    myBoundExpression = expr;
}

Expression const* SymbolVariable::boundExpression() const
{
    return myBoundExpression;
}

//
// TemplateDeclaration

TemplateDeclaration::TemplateDeclaration(Symbol sym)
    : Base(Declaration::Kind::Template, std::move(sym))
{
}

TemplateDeclaration::TemplateDeclaration(TemplateDeclaration const& rhs) = default;

TemplateDeclaration& TemplateDeclaration::operator = (TemplateDeclaration const& rhs)
{
    TemplateDeclaration(rhs).swap(*this);
    return *this;
}

TemplateDeclaration::~TemplateDeclaration() = default;

void TemplateDeclaration::swap(TemplateDeclaration& rhs) noexcept
{
    Base::swap(rhs);
    using kyfoo::swap;
    swap(myDefinition, rhs.myDefinition);
}

IMPL_CLONE_BEGIN(TemplateDeclaration, Base, Declaration)
IMPL_CLONE_END
IMPL_CLONE_REMAP_BEGIN(TemplateDeclaration, Base)
IMPL_CLONE_REMAP(myDefinition)
IMPL_CLONE_REMAP_END

SymRes TemplateDeclaration::resolveSymbols(Context& ctx)
{
    return Declaration::resolveSymbols(ctx);
}

void TemplateDeclaration::merge(TemplateDeclaration& rhs)
{
    myDefinition->merge(*rhs.myDefinition);
    rhs.myDefinition = nullptr;
}

//
// Utilities

bool isBinder(Declaration::Kind kind)
{
    switch ( kind ) {
    case Declaration::Kind::Field:
    case Declaration::Kind::ProcedureParameter:
    case Declaration::Kind::Variable:
    case Declaration::Kind::SymbolVariable:
        return true;

    default:
        return false;
    }
}

Binder const* getBinder(Declaration const& decl)
{
    if ( isBinder(decl.kind()) )
        return static_cast<Binder const*>(&decl);

    return nullptr;
}

Binder const* getBinder(Expression const& expr)
{
    if ( auto decl = getDeclaration(expr) )
        return getBinder(*decl);

    return nullptr;
}

bool isCallableDeclaration(Declaration::Kind kind)
{
    switch ( kind ) {
    case Declaration::Kind::DataType:
    case Declaration::Kind::Procedure:
        return true;

    default:
        return false;
    }
}

bool isMacroDeclaration(Declaration::Kind kind)
{
    switch (kind) {
    case Declaration::Kind::Symbol:
        return true;

    default:
        return false;
    }
}

bool hasIndirection(Declaration::Kind kind)
{
    return isMacroDeclaration(kind) || kind == Declaration::Kind::SymbolVariable;
}

bool hasIndirection(Expression const& expr)
{
    if ( auto id = identify(expr) )
        return hasIndirection(id->declaration()->kind());

    return false;
}

Expression const* getType(Declaration const& decl)
{
    if ( auto d = decl.as<DataTypeDeclaration>() )
        return &Expression::universe(1);

    if ( auto d = decl.as<Field>() )
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

bool isDtor(ProcedureDeclaration const& proc)
{
    auto templ = parentTemplate(proc);
    if ( !templ )
        return false;

    return templ->symbol().token().lexeme() == "dtor";
}

bool isDefinableDeclaration(Declaration::Kind kind)
{
    switch (kind)
    {
    case Declaration::Kind::DataType:
    case Declaration::Kind::Procedure:
    case Declaration::Kind::Template:
        return true;

    default:
        return false;
    }
}

DefinableDeclaration const* getDefinableDeclaration(Declaration const& decl)
{
    return getDefinableDeclaration(const_cast<Declaration&>(decl));
}

DefinableDeclaration* getDefinableDeclaration(Declaration& decl)
{
    if ( !isDefinableDeclaration(decl.kind()) )
        return nullptr;

    return static_cast<DefinableDeclaration*>(&decl);
}

Scope const* getDefinition(Declaration const& decl)
{
    return getDefinition(const_cast<Declaration&>(decl));
}

Scope* getDefinition(Declaration& decl)
{
    if ( auto d = getDefinableDeclaration(decl) )
        return d->definition();

    return nullptr;
}

void define(Declaration& decl, Scope& defn)
{
    if ( auto dt = decl.as<DataTypeDeclaration>() )
        if ( auto dtDefn = defn.as<DataTypeScope>() )
            return dt->define(*dtDefn);

    if ( auto proc = decl.as<ProcedureDeclaration>() )
        if ( auto procDefn = defn.as<ProcedureScope>() )
            return proc->define(*procDefn);

    if ( auto templ = decl.as<TemplateDeclaration>() )
        if ( auto templDefn = defn.as<TemplateScope>() )
            return templ->define(*templDefn);

    ENFORCEU("declaration/definition mismatch");
}

template <typename Dispatch>
struct DeclarationPrinter
{
    using Result = DefaultOutStream&;
    Dispatch& dispatch;
    Result stream;

    explicit DeclarationPrinter(Dispatch& dispatch, DefaultOutStream& stream)
        : dispatch(dispatch)
        , stream(stream)
    {
    }

    Result declDataType(DataTypeDeclaration const& dt)
    {
        return stream(dt.symbol());
    }

    Result declField(Field const& f)
    {
        return stream(f.symbol());
    }

    Result declSymbol(SymbolDeclaration const& s)
    {
        return stream(s.symbol());
    }

    Result declProcedure(ProcedureDeclaration const& proc)
    {
        if ( auto templ = parentTemplate(proc) )
            stream(templ->symbol());

        auto sink = [this](Expression const& e) {
            if ( auto decl = getDeclaration(e) ) {
                if ( auto param = decl->as<ProcedureParameter>() ) {
                    declProcedureParameter(*param);
                    return;
                }
            }

            stream(e);
        };

        stream("(");
        auto first = begin(proc.symbol().prototype().pattern());
        auto last = end(proc.symbol().prototype().pattern());
        if ( first != last ) {
            sink(**first);

            for ( ++first; first != last; ++first ) {
                stream(", ");
                sink(**first);
            }
        }

        stream(")");

        if ( proc.returnType() ) {
            stream(" -> ");
            sink(*proc.returnType());
        }

        return stream;
    }

    Result declProcedureParameter(ProcedureParameter const& p)
    {
        stream(p.symbol().token().lexeme());
        for ( auto const& c : p.constraints() )
            stream(" : ")(*c);

        return stream;
    }

    Result declVariable(VariableDeclaration const& var)
    {
        stream(var.symbol());
        for ( auto const& c : var.constraints() )
            stream(" : ")(*c);

        return stream;
    }

    Result declImport(ImportDeclaration const& imp)
    {
        return stream(imp.symbol());
    }

    Result declSymbolVariable(SymbolVariable const& symVar)
    {
        return stream(symVar.symbol());
    }

    Result declTemplate(TemplateDeclaration const& t)
    {
        return stream(t.symbol());
    }
};

    } // namespace ast

    void ascii::write(DefaultOutStream& sink, ast::Declaration const& decl)
    {
        ast::ShallowApply<ast::DeclarationPrinter> op(sink);
        op(decl);
    }

} // namespace kyfoo
