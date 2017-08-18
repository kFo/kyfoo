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
            d->symbol().resolveSymbols(dgn, resolver);

            if ( auto proc = d->as<ProcedureDeclaration>() )
                proc->resolvePrototypeSymbols(dgn);

            if ( !addSymbol(dgn, d->symbol(), *d) )
                continue;

            if ( isMacroDeclaration(d->kind()) )
                d->resolveSymbols(dgn);
        }
    }

    // Resolve definitions
    for ( auto& e : myDeclarations ) {
        if ( isMacroDeclaration(e->kind()) )
            continue;

        if ( !e->symbol().prototype().hasFreeVariables() )
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
LookupHit DeclarationScope::findCovariant(Diagnostics& dgn, SymbolReference const& sym)
{
    LookupHit hit;
    auto symSpace = findSymbolSpace(dgn, sym.name());
    if ( symSpace ) {
        auto t = symSpace->findCovariant(dgn, sym.pattern());
        if ( t.instance )
            myModule->appendTemplateInstance(t.instance);

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
    if ( auto proc = decl.as<ProcedureDeclaration>() ) {
        if ( sym.prototype().pattern().empty() ) {
            if ( auto other = symSpace->findEquivalent(dgn, proc->prototype().pattern()) ) {
                auto& err = dgn.error(module(), sym.identifier()) << "symbol clashes with procedure signature";
                err.see(*other);
                return false;
            }

            symSpace->append(ctx, proc->prototype(), decl);
            return true;
        }
        else {
            TemplateDeclaration* templ = nullptr;
            if ( auto existingDecl = symSpace->findEquivalent(dgn, sym.prototype().pattern()) ) {
                templ = existingDecl->as<TemplateDeclaration>();
                if ( !templ ) {
                    auto& err = ctx.error(sym.identifier()) << "procedures may only be appended to openly-defined templates";
                    err.see(*templ);
                    return false;
                }
            }
            else {
                clone_map_t cloneMap;
                std::unique_ptr<Symbol> templSym(sym.clone(cloneMap));
                templSym->remapReferences(cloneMap);
                auto templDecl = std::make_unique<TemplateDeclaration>(std::move(*templSym));
                templDecl->define(std::make_unique<TemplateScope>(*this, *templDecl));
                templ = templDecl.get();
                append(std::move(templDecl));
                symSpace->append(ctx, templ->symbol().prototype(), *templ);
            }

            templ->definition()->addProcedure(dgn, proc->prototype(), *proc);
            return true;
        }
    }

    if ( auto other = symSpace->findEquivalent(dgn, sym.prototype().pattern()) ) {
        auto& err = dgn.error(module(), sym.identifier()) << "symbol is already defined";
        err.see(*other);
        return false;
    }

    symSpace->append(ctx, sym.prototype(), decl);
    return true;
}

bool DeclarationScope::addProcedure(Diagnostics& dgn,
                                    PatternsPrototype const& proto,
                                    ProcedureDeclaration& proc)
{
    auto& symSpace = mySymbols.front();
    if ( auto decl = symSpace.findEquivalent(dgn, proto.pattern()) ) {
        // todo: error context for procedures
        auto& err = dgn.error(module(), myParent->declaration()->symbol().identifier()) << "procedure signature already defined";
        err.see(*decl);
        return false;
    }

    ScopeResolver resolver(*this);
    Context ctx(dgn, resolver);
    symSpace.append(ctx, proto, proc);
    return true;
}

SymbolSpace const* DeclarationScope::findSymbolSpace(Diagnostics&, std::string const& name) const
{
    auto symLess = [](SymbolSpace const& s, std::string const& name) { return s.name() < name; };
    auto symSet = lower_bound(begin(mySymbols), end(mySymbols), name, symLess);
    if ( symSet != end(mySymbols) && symSet->name() == name )
        return &*symSet;

    return nullptr;
}

SymbolSpace* DeclarationScope::findSymbolSpace(Diagnostics& dgn, std::string const& name)
{
    return const_cast<SymbolSpace*>(const_cast<DeclarationScope const*>(this)->findSymbolSpace(dgn, name));
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
    // Resolve declarations
    DeclarationScope::resolveSymbols(dgn);

    // Resolve expressions
    ScopeResolver resolver(*this);
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
