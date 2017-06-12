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

DeclarationScope::~DeclarationScope() = default;

void DeclarationScope::io(IStream& stream) const
{
    stream.next("declarations", myDeclarations);
}

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

    for ( auto const& symGroup : tracker.groups ) {
        for ( auto const& d : symGroup->declarations ) {
            d->symbol().resolveSymbols(dgn, resolver);
            if ( !addSymbol(dgn, d->symbol(), *d) )
                continue;

            if ( auto proc = d->as<ProcedureDeclaration>() ) {
                for ( auto& p : proc->parameters() )
                    p->resolveSymbols(dgn);

                addProcedure(dgn, proc->symbol(), *proc);
            }
        }
    }

    for ( auto& e : myDeclarations )
        e->resolveSymbols(dgn);
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
LookupHit DeclarationScope::findValue(SymbolReference const& symbol) const
{
    LookupHit hit;
    auto symSet = findSymbol(symbol.name());
    if ( symSet )
        hit.lookup(symSet, symSet->findValue(symbol.parameters()));

    return hit;
}

LookupHit DeclarationScope::findProcedureOverload(SymbolReference const& procOverload) const
{
    LookupHit hit;
    auto symSet = findProcedure(procOverload.name());
    if ( symSet )
        hit.lookup(symSet, static_cast<ProcedureDeclaration const*>(symSet->findValue(procOverload.parameters())));

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
    append(
        std::make_unique<ImportDeclaration>(
            Symbol(lexer::Token(lexer::TokenKind::Identifier, 0, 0, module.name()))));
}

SymbolSet* DeclarationScope::createSymbolSet(std::string const& name)
{
    auto l = lower_bound(begin(mySymbols), end(mySymbols), name);
    if ( l != end(mySymbols) && l->name() == name )
        return &*l;

    l = mySymbols.insert(l, SymbolSet(name));
    return &*l;
}

SymbolSet* DeclarationScope::createProcedureOverloadSet(std::string const& name)
{
    auto l = lower_bound(begin(myProcedureOverloads), end(myProcedureOverloads), name);
    if ( l != end(myProcedureOverloads) && l->name() == name )
        return &*l;

    l = myProcedureOverloads.insert(l, SymbolSet(name));
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
    if ( symSet != end(mySymbols) && symSet->name() == identifier )
        return &*symSet;

    return nullptr;
}

SymbolSet const* DeclarationScope::findProcedure(std::string const& identifier) const
{
    auto procOverloads = lower_bound(begin(myProcedureOverloads), end(myProcedureOverloads), identifier);
    if ( procOverloads != end(myProcedureOverloads) && procOverloads->name() == identifier )
        return &*procOverloads;

    return nullptr;
}

Module* DeclarationScope::module()
{
    return myModule;
}

Declaration* DeclarationScope::declaration()
{
    return myDeclaration;
}

DeclarationScope* DeclarationScope::parent()
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
    : DeclarationScope(parent)
    , myDataDeclaration(&declaration)
{
}

DataSumScope::~DataSumScope() = default;

void DataSumScope::io(IStream& stream) const
{
    DeclarationScope::io(stream);
}

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

//
// DataProductScope

DataProductScope::DataProductScope(DeclarationScope* parent,
                                   DataProductDeclaration& declaration)
    : DeclarationScope(parent)
    , myDataDeclaration(&declaration)
{
}

DataProductScope::~DataProductScope() = default;

void DataProductScope::io(IStream& stream) const
{
    DeclarationScope::io(stream);
}

//
// ProcedureScope

ProcedureScope::ProcedureScope(DeclarationScope* parent,
                               ProcedureDeclaration& declaration)
    : DeclarationScope(parent)
    , myDeclaration(&declaration)
{
}

ProcedureScope::~ProcedureScope() = default;

void ProcedureScope::io(IStream& stream) const
{
    DeclarationScope::io(stream);
    stream.next("expressions", myExpressions);
}

void ProcedureScope::resolveSymbols(Diagnostics& dgn)
{
    ScopeResolver resolver(this);

    // Resolve parameters
    for ( auto const& p : myDeclaration->parameters() ) {
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

void ProcedureScope::append(std::unique_ptr<Expression> expression)
{
    myExpressions.emplace_back(std::move(expression));
}

    } // namespace ast
} // namespace kyfoo
