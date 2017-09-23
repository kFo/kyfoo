#include <kyfoo/ast/Scopes.hpp>

#include <cassert>

#include <kyfoo/Diagnostics.hpp>

#include <kyfoo/ast/Axioms.hpp>
#include <kyfoo/ast/Declarations.hpp>
#include <kyfoo/ast/Expressions.hpp>
#include <kyfoo/ast/Module.hpp>
#include <kyfoo/ast/Semantics.hpp>
#include <kyfoo/ast/Context.hpp>

namespace kyfoo {
    namespace ast {

//
// DeclarationScope

DeclarationScope::DeclarationScope(Module* module,
                                   DeclarationScope* parent,
                                   Declaration* decl)
    : myModule(module)
    , myParent(parent)
    , myDeclaration(decl)
{
    mySymbols.emplace_back(this, "");
}

DeclarationScope::DeclarationScope(Module& module)
    : DeclarationScope(&module, nullptr, nullptr)
{
}

DeclarationScope::DeclarationScope(DeclarationScope* parent)
    : DeclarationScope(&parent->module(), parent, nullptr)
{
}

DeclarationScope::DeclarationScope(DeclarationScope& parent, Declaration& decl)
    : DeclarationScope(&parent.module(), &parent, &decl)
{
}

DeclarationScope::DeclarationScope(DeclarationScope const& rhs)
    : myModule(rhs.myModule)
    , myDeclaration(rhs.myDeclaration)
    , myParent(rhs.myParent)
{
    // mySymbols, myProcedures, and myImports are to be filled out
    // by the semantic passes that build these indices
}

DeclarationScope& DeclarationScope::operator = (DeclarationScope const& rhs)
{
    DeclarationScope(rhs).swap(*this);
    return *this;
}

DeclarationScope::~DeclarationScope() = default;

void DeclarationScope::swap(DeclarationScope& rhs)
{
    using std::swap;
    swap(myModule, rhs.myModule);
    swap(myDeclaration, rhs.myDeclaration);
    swap(myParent, rhs.myParent);
    swap(myDeclarations, rhs.myDeclarations);
    swap(mySymbols, rhs.mySymbols);
    swap(myImports, rhs.myImports);
}

void DeclarationScope::io(IStream& stream) const
{
    stream.next("declarations", myDeclarations);
}

IMPL_CLONE_NOBASE_BEGIN(DeclarationScope, DeclarationScope)
IMPL_CLONE_CHILD(myDeclarations)
IMPL_CLONE_END
IMPL_CLONE_REMAP_NOBASE_BEGIN(DeclarationScope)
IMPL_CLONE_REMAP(myModule)
IMPL_CLONE_REMAP(myDeclaration)
IMPL_CLONE_REMAP(myParent)
IMPL_CLONE_REMAP(myDeclarations)
IMPL_CLONE_REMAP_END

void DeclarationScope::resolveImports(Diagnostics& dgn)
{
    for ( auto& e : myDeclarations ) {
        if ( auto d = e->as<ImportDeclaration>() ) {
            module().import(dgn, d->identifier());
        }
    }
}

void DeclarationScope::resolveSymbols(Diagnostics& dgn)
{
    SymbolDependencyTracker tracker(module(), dgn);
    for ( auto const& d : myDeclarations )
        traceDependencies(tracker, *d);

    if ( dgn.errorCount() )
        return;

    tracker.sortPasses();

    ScopeResolver resolver(*this);

    // Resolve top-level declarations
    for ( auto const& symGroup : tracker.groups ) {
        for ( auto const& d : symGroup->declarations ) {
            if ( auto proc = d->as<ProcedureDeclaration>() )
                proc->resolvePrototypeSymbols(dgn);
            else
                d->symbol().resolveSymbols(dgn, resolver);

            if ( !addSymbol(dgn, d->symbol(), *d) )
                continue;

            if ( isMacroDeclaration(d->kind()) )
                d->resolveSymbols(dgn);
        }
    }

    // Resolve definitions
    for ( auto const& symGroup : tracker.groups ) {
        for ( auto const& d : symGroup->declarations ) {
            if ( isMacroDeclaration(d->kind()) )
                continue;

            if ( d->symbol().prototype().isConcrete() ) {
                if ( auto proc = d->as<ProcedureDeclaration>() ) {
                    if ( proc->symbol().prototype().isConcrete() )
                        proc->resolveSymbols(dgn);
                }
                else {
                    d->resolveSymbols(dgn);
                }
            }
        }
    }
}

/**
 * Locates a symbol with parameter list that has exact semantic match
 * 
 * Equivalent symbols:
 * \code
 * mytype<n : integer>
 * mytype<m : integer>
 * \endcode
 * 
 * Non-equivalent symbols:
 * \code
 * mytype<n : integer>
 * mytype<n : ascii>
 * \endcode
 */
LookupHit DeclarationScope::findEquivalent(Diagnostics& dgn, SymbolReference const& symbol) const
{
    auto symSpace = findSymbolSpace(dgn, symbol.name());
    if ( symSpace )
        return LookupHit(symSpace, symSpace->findEquivalent(dgn, symbol.pattern()));

    if ( myDeclaration && symbol.pattern().empty() )
        return LookupHit(symSpace, myDeclaration->symbol().prototype().findVariable(symbol.name()));

    return LookupHit();
}

/**
 * Locates a symbol with parameter list that could be instantiated by a value expression
 *
 * Overload example:
 * \code
 * mytype<n : integer>
 * mytype<32>
 * \endcode
 *
 * Non-overload example:
 * \code
 * mytype<n : integer>
 * mytype<m : integer>
 * \endcode
 *
 * \code
 * mytype<n : integer>
 * mytype<"str">
 * \endcode
 */
LookupHit DeclarationScope::findOverload(Diagnostics& dgn, SymbolReference const& sym) const
{
    LookupHit hit;
    auto symSpace = findSymbolSpace(dgn, sym.name());
    if ( symSpace ) {
        auto t = symSpace->findOverload(dgn, sym.pattern());
        hit.lookup(symSpace, t.instance ? t.instance : t.parent);
    }

    return hit;
}

void DeclarationScope::setDeclaration(Declaration* declaration)
{
    myDeclaration = declaration;
}

void DeclarationScope::append(std::unique_ptr<Declaration> declaration)
{
    myDeclarations.emplace_back(std::move(declaration));
    myDeclarations.back()->setScope(*this);
}

void DeclarationScope::import(Module& module)
{
    append(std::make_unique<ImportDeclaration>(Symbol(lexer::Token(lexer::TokenKind::Identifier, 0, 0, module.name()))));
}

void DeclarationScope::merge(DeclarationScope& rhs)
{
    myDeclarations.reserve(myDeclarations.size() + rhs.myDeclarations.size());
    for ( auto& e : rhs.myDeclarations ) {
        myDeclarations.emplace_back(std::move(e));
        myDeclarations.back()->setScope(*this);
    }

    rhs.myDeclarations.clear();
}

SymbolSpace* DeclarationScope::createSymbolSpace(Diagnostics&, std::string const& name)
{
    auto symLess = [](SymbolSpace const& s, std::string const& name) { return s.name() < name; };
    auto l = lower_bound(begin(mySymbols), end(mySymbols), name, symLess);
    if ( l != end(mySymbols) && l->name() == name )
        return &*l;

    l = mySymbols.insert(l, SymbolSpace(this, name));
    return &*l;
}

bool DeclarationScope::addSymbol(Diagnostics& dgn,
                                 Symbol const& sym,
                                 Declaration& decl)
{
    auto symSpace = createSymbolSpace(dgn, sym.identifier().lexeme());

    ScopeResolver resolver(*this);
    Context ctx(dgn, resolver);

    if ( auto other = symSpace->findEquivalent(dgn, sym.prototype().pattern()) ) {
        auto templDecl = decl.as<TemplateDeclaration>();
        if ( !templDecl ) {
            auto& err = dgn.error(module(), sym.identifier()) << "symbol is already defined";
            err.see(*other);
            return false;
        }

        auto otherTemplDecl = other->as<TemplateDeclaration>();
        if ( !otherTemplDecl ) {
            auto& err = dgn.error(module(), sym.identifier()) << "symbol was not first defined as a template";
            err.see(*other);
            return false;
        }

        otherTemplDecl->merge(*templDecl);
        return true;
    }

    symSpace->append(ctx, sym.prototype(), decl);
    return true;
}

SymbolSpace* DeclarationScope::findSymbolSpace(Diagnostics&, std::string const& name) const
{
    auto symLess = [](SymbolSpace const& s, std::string const& name) { return s.name() < name; };
    auto symSet = lower_bound(begin(mySymbols), end(mySymbols), name, symLess);
    if ( symSet != end(mySymbols) && symSet->name() == name )
        return &*symSet;

    return nullptr;
}

Module& DeclarationScope::module()
{
    return *myModule;
}

Module const& DeclarationScope::module() const
{
    return *myModule;
}

Declaration* DeclarationScope::declaration()
{
    return myDeclaration;
}

Declaration const* DeclarationScope::declaration() const
{
    return myDeclaration;
}

DeclarationScope* DeclarationScope::parent()
{
    return myParent;
}

DeclarationScope const* DeclarationScope::parent() const
{
    return myParent;
}

Slice<Declaration*> DeclarationScope::childDeclarations() const
{
    return myDeclarations;
}

//
// DataSumScope

DataSumScope::DataSumScope(DeclarationScope& parent,
                           DataSumDeclaration& declaration)
    : DeclarationScope(parent, declaration)
{
}

DataSumScope::DataSumScope(DataSumScope const& rhs)
    : DeclarationScope(rhs)
{
}

DataSumScope& DataSumScope::operator = (DataSumScope const& rhs)
{
    DataSumScope(rhs).swap(*this);
    return *this;
}

DataSumScope::~DataSumScope() = default;

void DataSumScope::swap(DataSumScope& rhs)
{
    DeclarationScope::swap(rhs);
}

void DataSumScope::io(IStream& stream) const
{
    DeclarationScope::io(stream);
}

IMPL_CLONE_BEGIN(DataSumScope, DeclarationScope, DeclarationScope)
IMPL_CLONE_END
IMPL_CLONE_REMAP_BEGIN(DataSumScope, DeclarationScope)
IMPL_CLONE_REMAP_END

void DataSumScope::resolveSymbols(Diagnostics& dgn)
{
    ScopeResolver resolver(*this);
    for ( auto const& d : myDeclarations ) {
        auto dsCtor = d->as<DataSumDeclaration::Constructor>();
        if ( !dsCtor )
            throw std::runtime_error("data sum must only contain constructors");

        dsCtor->symbol().resolveSymbols(dgn, resolver);
        parent()->addSymbol(dgn, d->symbol(), *d);
    }

    for ( auto& e : myDeclarations )
        e->resolveSymbols(dgn);
}

DataSumDeclaration* DataSumScope::declaration()
{
    return static_cast<DataSumDeclaration*>(myDeclaration);
}

//
// DataProductScope

DataProductScope::DataProductScope(DeclarationScope& parent,
                                   DataProductDeclaration& declaration)
    : DeclarationScope(parent, declaration)
{
}

DataProductScope::DataProductScope(DataProductScope const& rhs)
    : DeclarationScope(rhs)
{
    // myFields is populated by the semantic pass
}

DataProductScope& DataProductScope::operator = (DataProductScope const& rhs)
{
    DataProductScope(rhs).swap(*this);
    return *this;
}

DataProductScope::~DataProductScope() = default;

void DataProductScope::swap(DataProductScope& rhs)
{
    DeclarationScope::swap(rhs);
    using std::swap;
    swap(myFields, rhs.myFields);
}

void DataProductScope::io(IStream& stream) const
{
    DeclarationScope::io(stream);
}

IMPL_CLONE_BEGIN(DataProductScope, DeclarationScope, DeclarationScope)
IMPL_CLONE_END
IMPL_CLONE_REMAP_BEGIN(DataProductScope, DeclarationScope)
IMPL_CLONE_REMAP(myFields)
IMPL_CLONE_REMAP_END

void DataProductScope::resolveSymbols(Diagnostics& dgn)
{
    for ( auto& d : myDeclarations )
        if ( auto v = d->as<DataProductDeclaration::Field>() )
            myFields.push_back(v);

    DeclarationScope::resolveSymbols(dgn);
}

DataProductDeclaration* DataProductScope::declaration()
{
    return static_cast<DataProductDeclaration*>(myDeclaration);
}

Slice<DataProductDeclaration::Field*> DataProductScope::fields()
{
    return myFields;
}

const Slice<DataProductDeclaration::Field*> DataProductScope::fields() const
{
    return myFields;
}

//
// ProcedureScope

ProcedureScope::ProcedureScope(DeclarationScope& parent,
                               ProcedureDeclaration& declaration)
    : DeclarationScope(parent, declaration)
{
}

ProcedureScope::ProcedureScope(ProcedureScope const& rhs)
    : DeclarationScope(rhs)
{
}

ProcedureScope& ProcedureScope::operator = (ProcedureScope const& rhs)
{
    ProcedureScope(rhs).swap(*this);
    return *this;
}

ProcedureScope::~ProcedureScope() = default;

void ProcedureScope::swap(ProcedureScope& rhs)
{
    DeclarationScope::swap(rhs);
    using std::swap;
    swap(myStatements, rhs.myStatements);
}

void ProcedureScope::io(IStream& stream) const
{
    DeclarationScope::io(stream);
    stream.openArray("statements");
    for ( auto const& e : myStatements )
        stream.next("", e.expression());
    stream.closeArray();
}

IMPL_CLONE(ProcedureScope)

void ProcedureScope::cloneChildren(DeclarationScope& c, clone_map_t& map) const
{
    ProcedureScope& ps = static_cast<ProcedureScope&>(c);
    for ( auto const& e : myStatements )
        ps.myStatements.emplace_back(e.clone(map));
}

void ProcedureScope::remapReferences(clone_map_t const& map)
{
    for ( auto& e : myStatements )
        e.remapReferences(map);
}

void ProcedureScope::resolveSymbols(Diagnostics& dgn)
{
    // Resolve declarations
    DeclarationScope::resolveSymbols(dgn);

    // Resolve expressions
    ScopeResolver resolver(*this);
    Context ctx(dgn, resolver);
    for ( auto& e : myStatements )
        e.resolveSymbols(ctx);
}

ProcedureDeclaration* ProcedureScope::declaration()
{
    return static_cast<ProcedureDeclaration*>(myDeclaration);
}

void ProcedureScope::append(std::unique_ptr<Expression> expression)
{
    myStatements.emplace_back(std::move(expression));
}

Slice<ProcedureScope::Statement> const ProcedureScope::statements() const
{
    return myStatements;
}

//
// ProcedureScope::UnnamedVariable

ProcedureScope::UnnamedVariable::UnnamedVariable(Expression const& expr,
                                                 Declaration const& dataType)
    : myExpression(&expr)
    , myDataType(&dataType)
{
}

ProcedureScope::UnnamedVariable::~UnnamedVariable() = default;

void ProcedureScope::UnnamedVariable::remapReferences(clone_map_t const& map)
{
    const_cast<Expression*>(myExpression)->remapReferences(map);
    const_cast<Declaration*>(myDataType)->remapReferences(map);
}

Expression const& ProcedureScope::UnnamedVariable::constraint() const
{
    return *myExpression;
}

Declaration const& ProcedureScope::UnnamedVariable::dataType() const
{
    return *myDataType;
}

//
// ProcedureScope::Statement

ProcedureScope::Statement::Statement(std::unique_ptr<Expression> expr)
    : myExpression(std::move(expr))
{
}

ProcedureScope::Statement::Statement(Statement const& rhs)
    : myUnnamedVariables(rhs.myUnnamedVariables)
{
}

ProcedureScope::Statement& ProcedureScope::Statement::operator = (Statement const& rhs)
{
    Statement(rhs).swap(*this);
    return *this;
}

ProcedureScope::Statement::Statement(Statement&& rhs)
    : myExpression(std::move(rhs.myExpression))
    , myUnnamedVariables(std::move(rhs.myUnnamedVariables))
{
}

ProcedureScope::Statement& ProcedureScope::Statement::operator = (Statement&& rhs)
{
    this->~Statement();
    new (this) Statement(std::move(rhs));
    return *this;
}

ProcedureScope::Statement::~Statement() = default;

void ProcedureScope::Statement::swap(Statement& rhs)
{
    using std::swap;
    swap(myExpression, rhs.myExpression);
    swap(myUnnamedVariables, rhs.myUnnamedVariables);
}

ProcedureScope::Statement ProcedureScope::Statement::clone(clone_map_t& map) const
{
    Statement ret(std::unique_ptr<Expression>(myExpression->clone(map)));
    ret.myUnnamedVariables = myUnnamedVariables;
    return std::move(ret);
}

void ProcedureScope::Statement::remapReferences(clone_map_t const& map)
{
    myExpression->remapReferences(map);
    for ( auto& e : myUnnamedVariables )
        e.remapReferences(map);
}

Expression const& ProcedureScope::Statement::expression() const
{
    return *myExpression;
}

Slice<ProcedureScope::UnnamedVariable> const ProcedureScope::Statement::unnamedVariables() const
{
    return myUnnamedVariables;
}

void ProcedureScope::Statement::resolveSymbols(Context& ctx)
{
    ctx.resolveExpression(myExpression);
    scanUnnamed(ctx.module());
}

void ProcedureScope::Statement::appendUnnamed(Expression const& expr)
{
    auto decl = resolveIndirections(expr.declaration());
    if ( !decl )
        throw std::runtime_error("unnamed instance must have a type");

    Declaration const* dt = decl;
    if ( auto proc = decl->as<ProcedureDeclaration>() ) {
        if ( isCtor(*proc) )
            dt = proc->parameters()[0]->dataType();
        else
            dt = proc->returnType()->declaration();
    }

    myUnnamedVariables.emplace_back(expr, *dt);
}

template <typename Dispatcher>
struct UnnamedScan
{
    using result_t = void;
    Dispatcher& dispatch;
    Module const& module;
    ProcedureScope::Statement* stmt;

    UnnamedScan(Dispatcher& dispatch, Module const& mod, ProcedureScope::Statement& stmt)
        : dispatch(dispatch)
        , module(mod)
        , stmt(&stmt)
    {
    }

    void scan(Expression const& expr)
    {
        auto proc = expr.declaration()->as<ProcedureDeclaration>();
        if ( !proc )
            return;

        auto const hasReturn = proc->returnType()->declaration() != module.axioms().intrinsic(EmptyLiteralType);
        if ( isCtor(*proc) || hasReturn )
            stmt->appendUnnamed(expr);
    }

    result_t exprPrimary(PrimaryExpression const& p)
    {
        scan(p);
    }

    result_t exprReference(ReferenceExpression const&)
    {
        // nop
    }

    result_t exprTuple(TupleExpression const& t)
    {
        for ( auto const& e : t.expressions() )
            dispatch(*e);
    }

    result_t exprApply(ApplyExpression const& a)
    {
        for ( auto const& e : a.expressions() )
            dispatch(*e);

        scan(a);
    }

    result_t exprSymbol(SymbolExpression const& s)
    {
        for ( auto const& e : s.expressions() )
            dispatch(*e);

        scan(s);
    }

    result_t exprDot(DotExpression const& d)
    {
        for ( auto const& e : d.expressions() )
            dispatch(*e);
    }
};

void ProcedureScope::Statement::scanUnnamed(Module const& module)
{
    ShallowApply<UnnamedScan> op(module, *this);
    op(*myExpression);
}

//
// TemplateScope

TemplateScope::TemplateScope(DeclarationScope& parent,
                             TemplateDeclaration& declaration)
    : DeclarationScope(parent, declaration)
{
}

TemplateScope::TemplateScope(TemplateScope const& rhs)
    : DeclarationScope(rhs)
{
}

TemplateScope& TemplateScope::operator = (TemplateScope const& rhs)
{
    TemplateScope(rhs).swap(*this);
    return *this;
}

TemplateScope::~TemplateScope() = default;

void TemplateScope::swap(TemplateScope& rhs)
{
    DeclarationScope::swap(rhs);
}

void TemplateScope::io(IStream& stream) const
{
    DeclarationScope::io(stream);
}

IMPL_CLONE_BEGIN(TemplateScope, DeclarationScope, DeclarationScope)
IMPL_CLONE_END
IMPL_CLONE_REMAP_BEGIN(TemplateScope, DeclarationScope)
IMPL_CLONE_REMAP_END

void TemplateScope::resolveSymbols(Diagnostics& dgn)
{
    // Resolve declarations
    DeclarationScope::resolveSymbols(dgn);
}

TemplateDeclaration* TemplateScope::declaration()
{
    return static_cast<TemplateDeclaration*>(myDeclaration);
}

    } // namespace ast
} // namespace kyfoo
