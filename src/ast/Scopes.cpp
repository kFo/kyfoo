#include <kyfoo/ast/Scopes.hpp>

#include <cassert>

#include <kyfoo/Diagnostics.hpp>

#include <kyfoo/ast/Declarations.hpp>
#include <kyfoo/ast/Expressions.hpp>
#include <kyfoo/ast/Module.hpp>
#include <kyfoo/ast/Semantics.hpp>
#include <kyfoo/ast/Context.hpp>

namespace kyfoo {
    namespace ast {

//
// DeclarationScope

DeclarationScope::DeclarationScope(Module* module)
    : myModule(module)
    , myParent(nullptr)
{
}

DeclarationScope::DeclarationScope(DeclarationScope* parent)
    : myModule(parent->module())
    , myParent(parent)
{
}

DeclarationScope::DeclarationScope(DeclarationScope* parent, Declaration& decl)
    : myModule(parent->module())
    , myParent(parent)
    , myDeclaration(&decl)
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
    swap(myProcedureOverloads, rhs.myProcedureOverloads);
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
            module()->import(dgn, d->identifier());
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

    ScopeResolver resolver(this);

    // Resolve top-level declarations
    for ( auto const& symGroup : tracker.groups ) {
        for ( auto const& d : symGroup->declarations ) {
            d->symbol().resolveSymbols(dgn, resolver);

            if ( auto proc = d->as<ProcedureDeclaration>() ) {
                proc->resolvePrototypeSymbols(dgn);
                if ( !addProcedure(dgn, proc->symbol(), *proc) )
                    continue;
            }
            else {
                if ( !addSymbol(dgn, d->symbol(), *d) )
                    continue;
            }

            if ( isMacroDeclaration(d->kind()) )
                d->resolveSymbols(dgn);
        }
    }

    // Resolve definitions
    for ( auto& e : myDeclarations ) {
        if ( isMacroDeclaration(e->kind()) )
            continue;

        if ( !e->symbol().hasFreeVariables() )
            e->resolveSymbols(dgn);
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
LookupHit DeclarationScope::findEquivalent(SymbolReference const& symbol) const
{
    auto symSet = findSymbol(symbol.name());
    if ( symSet )
        return LookupHit(symSet, symSet->findEquivalent(symbol.parameters()));

    if ( myDeclaration && symbol.parameters().empty() )
        return LookupHit(symSet, myDeclaration->symbol().findVariable(symbol.name()));

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
LookupHit DeclarationScope::findValue(Diagnostics& dgn, SymbolReference const& symbol) const
{
    LookupHit hit;
    auto symSet = findSymbol(symbol.name());
    if ( symSet ) {
        auto t = symSet->findValue(dgn, symbol.parameters());
        if ( t.instance )
            myModule->appendTemplateInstance(t.instance);

        hit.lookup(symSet, t.instance ? t.instance : t.parent);
    }

    return hit;
}

LookupHit DeclarationScope::findProcedureOverload(Diagnostics& dgn, SymbolReference const& procOverload) const
{
    LookupHit hit;
    auto symSet = findProcedure(procOverload.name());
    if ( symSet ) {
        auto t = symSet->findValue(dgn, procOverload.parameters());
        auto decl = t.instance ? t.instance : t.parent;
        if ( t.instance )
            myModule->appendTemplateInstance(t.instance);

        hit.lookup(symSet, static_cast<ProcedureDeclaration const*>(decl));
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
    append(std::make_unique<ImportDeclaration>(Symbol(module.name())));
}

SymbolSet* DeclarationScope::createSymbolSet(std::string const& name)
{
    auto l = lower_bound(begin(mySymbols), end(mySymbols), name);
    if ( l != end(mySymbols) && l->name() == name )
        return &*l;

    l = mySymbols.insert(l, SymbolSet(this, name));
    return &*l;
}

SymbolSet* DeclarationScope::createProcedureOverloadSet(std::string const& name)
{
    auto l = lower_bound(begin(myProcedureOverloads), end(myProcedureOverloads), name);
    if ( l != end(myProcedureOverloads) && l->name() == name )
        return &*l;

    l = myProcedureOverloads.insert(l, SymbolSet(this, name));
    return &*l;
}

bool DeclarationScope::addSymbol(Diagnostics& dgn, Symbol const& sym, Declaration& decl)
{
    auto symSet = createSymbolSet(sym.name());
    if ( auto other = symSet->findEquivalent(sym.parameters()) ) {
        auto& err = dgn.error(module(), sym.identifier()) << "symbol is already defined";
        err.see(other);
        return false;
    }

    symSet->append(sym.parameters(), decl);
    return true;
}

bool DeclarationScope::addProcedure(Diagnostics& dgn, Symbol const& sym, ProcedureDeclaration& procDecl)
{
    auto procSet = createProcedureOverloadSet(sym.name());
    if ( auto other = procSet->findEquivalent(sym.parameters()) ) {
        auto& err = dgn.error(module(), sym.identifier()) << "procedure declaration conflicts with existing overload";
        err.see(other);
        return false;
    }

    std::vector<Expression*> paramConstraints;
    paramConstraints.reserve(procDecl.parameters().size());
    for ( auto const& e : procDecl.parameters() )
        paramConstraints.push_back(e->constraint());

    procSet->append(paramConstraints, procDecl);
    return true;
}

SymbolSet const* DeclarationScope::findSymbol(std::string const& identifier) const
{
    auto symSet = lower_bound(begin(mySymbols), end(mySymbols), identifier);
    if ( symSet != end(mySymbols) && *symSet == identifier )
        return &*symSet;

    return nullptr;
}

SymbolSet const* DeclarationScope::findProcedure(std::string const& identifier) const
{
    auto procOverloads = lower_bound(begin(myProcedureOverloads), end(myProcedureOverloads), identifier);
    if ( procOverloads != end(myProcedureOverloads) && *procOverloads == identifier )
        return &*procOverloads;

    return nullptr;
}

Module* DeclarationScope::module()
{
    return myModule;
}

Module const* DeclarationScope::module() const
{
    return myModule;
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

DataSumScope::DataSumScope(DeclarationScope* parent,
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
    ScopeResolver resolver(this);
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

DataProductScope::DataProductScope(DeclarationScope* parent,
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

ProcedureScope::ProcedureScope(DeclarationScope* parent,
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
    swap(myExpressions, rhs.myExpressions);
}

void ProcedureScope::io(IStream& stream) const
{
    DeclarationScope::io(stream);
    stream.next("expressions", myExpressions);
}

IMPL_CLONE_BEGIN(ProcedureScope, DeclarationScope, DeclarationScope)
IMPL_CLONE_CHILD(myExpressions)
IMPL_CLONE_END
IMPL_CLONE_REMAP_BEGIN(ProcedureScope, DeclarationScope)
IMPL_CLONE_REMAP(myExpressions)
IMPL_CLONE_REMAP_END

void ProcedureScope::resolveSymbols(Diagnostics& dgn)
{
    ScopeResolver resolver(this);

    // Resolve parameters
    for ( auto const& p : declaration()->parameters() ) {
        p->symbol().resolveSymbols(dgn, resolver);
        if ( !addSymbol(dgn, p->symbol(), *p) )
            continue;
    }

    // Resolve declarations
    DeclarationScope::resolveSymbols(dgn);

    // Resolve expressions
    Context ctx(dgn, resolver);
    ctx.resolveExpressions(myExpressions);
}

ProcedureDeclaration* ProcedureScope::declaration()
{
    return static_cast<ProcedureDeclaration*>(myDeclaration);
}

void ProcedureScope::append(std::unique_ptr<Expression> expression)
{
    myExpressions.emplace_back(std::move(expression));
}

Slice<Expression*> ProcedureScope::expressions()
{
    return myExpressions;
}

const Slice<Expression*> ProcedureScope::expressions() const
{
    return myExpressions;
}

    } // namespace ast
} // namespace kyfoo
