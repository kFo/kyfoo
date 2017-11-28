#include <kyfoo/ast/Scopes.hpp>

#include <cassert>

#include <kyfoo/Diagnostics.hpp>

#include <kyfoo/ast/Axioms.hpp>
#include <kyfoo/ast/ControlFlow.hpp>
#include <kyfoo/ast/Declarations.hpp>
#include <kyfoo/ast/Expressions.hpp>
#include <kyfoo/ast/Fabrication.hpp>
#include <kyfoo/ast/Module.hpp>
#include <kyfoo/ast/Semantics.hpp>
#include <kyfoo/ast/Context.hpp>

namespace kyfoo {
namespace ast {

//
// DeclarationScope

DeclarationScope::DeclarationScope(Kind kind,
                                   Module* module,
                                   DeclarationScope* parent,
                                   Declaration* decl)
    : myKind(kind)
    , myModule(module)
    , myParent(parent)
    , myDeclaration(decl)
{
    mySymbols.emplace_back(this, "");
}

DeclarationScope::DeclarationScope(Module& module)
    : DeclarationScope(Kind::Declaration, &module, nullptr, nullptr)
{
}

DeclarationScope::DeclarationScope(DeclarationScope* parent)
    : DeclarationScope(Kind::Declaration, &parent->module(), parent, nullptr)
{
}

DeclarationScope::DeclarationScope(DeclarationScope& parent, Declaration& decl)
    : DeclarationScope(Kind::Declaration, &parent.module(), &parent, &decl)
{
}

DeclarationScope::DeclarationScope(DeclarationScope const& rhs)
    : myKind(rhs.myKind)
    , myModule(rhs.myModule)
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
    swap(myKind, rhs.myKind);
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

void DeclarationScope::resolveSymbols(Module& endModule, Diagnostics& dgn)
{
    SymbolDependencyTracker tracker(module(), dgn);
    for ( auto const& d : myDeclarations )
        traceDependencies(tracker, *d);

    if ( dgn.errorCount() )
        return;

    tracker.sortPasses();

    ScopeResolver resolver(*this);
    Context ctx(endModule, dgn, resolver);

    // Resolve top-level declarations
    for ( auto const& symGroup : tracker.groups ) {
        for ( auto const& d : symGroup->declarations ) {
            if ( auto proc = d->as<ProcedureDeclaration>() )
                proc->resolvePrototypeSymbols(endModule, dgn);
            else
                d->symbol().resolveSymbols(ctx);

            if ( !addSymbol(dgn, d->symbol(), *d) )
                continue;

            if ( isMacroDeclaration(d->kind()) )
                d->resolveSymbols(endModule, dgn);
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
                        proc->resolveSymbols(endModule, dgn);
                }
                else {
                    d->resolveSymbols(endModule, dgn);
                }
            }
        }
    }
}

void DeclarationScope::resolveAttributes(Module& endModule, Diagnostics& dgn)
{
    for ( auto& d : myDeclarations )
        d->resolveAttributes(endModule, dgn);
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
    auto symSpace = findSymbolSpace(symbol.name());
    if ( symSpace )
        return LookupHit(symSpace, symSpace->findEquivalent(symbol.pattern()));

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
LookupHit DeclarationScope::findOverload(Module& endModule, Diagnostics& dgn, SymbolReference const& sym) const
{
    LookupHit hit;
    auto symSpace = findSymbolSpace(sym.name());
    if ( symSpace ) {
        auto t = symSpace->findOverload(endModule, dgn, sym.pattern());
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

    if ( auto other = symSpace->findEquivalent(sym.prototype().pattern()) ) {
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

    symSpace->append(sym.prototype(), decl);
    return true;
}

SymbolSpace* DeclarationScope::findSymbolSpace(std::string const& name) const
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
    : DeclarationScope(Kind::DataSum, &parent.module(), &parent, &declaration)
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

void DataSumScope::resolveSymbols(Module& endModule, Diagnostics& dgn)
{
    ScopeResolver resolver(*this);
    Context ctx(endModule, dgn, resolver);
    for ( auto const& d : myDeclarations ) {
        auto dsCtor = d->as<DataSumDeclaration::Constructor>();
        if ( !dsCtor )
            throw std::runtime_error("data sum must only contain constructors");

        dsCtor->symbol().resolveSymbols(ctx);
        parent()->addSymbol(dgn, d->symbol(), *d);
    }

    for ( auto& e : myDeclarations )
        e->resolveSymbols(endModule, dgn);
}

DataSumDeclaration* DataSumScope::declaration()
{
    return static_cast<DataSumDeclaration*>(myDeclaration);
}

//
// DataProductScope

DataProductScope::DataProductScope(DeclarationScope& parent,
                                   DataProductDeclaration& declaration)
    : DeclarationScope(Kind::DataProduct, &parent.module(), &parent, &declaration)
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

void DataProductScope::resolveSymbols(Module& endModule, Diagnostics& dgn)
{
    for ( auto& d : myDeclarations )
        if ( auto v = d->as<DataProductDeclaration::Field>() )
            myFields.push_back(v);

    DeclarationScope::resolveSymbols(endModule, dgn);

    resolveConstructors(dgn);
    resolveDestructor(endModule, dgn);
}

void DataProductScope::resolveConstructors(Diagnostics&)
{
    // todo: only make default ctor when no other ctor/dtor defined
}

std::unique_ptr<ProcedureDeclaration> DataProductScope::createDefaultConstructor()
{
    return nullptr;
}

void DataProductScope::resolveDestructor(Module& endModule, Diagnostics& dgn)
{
    auto makeTempl = [this, &dgn] {
        auto templ = std::make_unique<TemplateDeclaration>(
            Symbol(lexer::Token(lexer::TokenKind::Identifier,
                                declaration()->symbol().identifier().line(),
                                declaration()->symbol().identifier().column(),
                                "dtor")));
        templ->define(std::make_unique<TemplateScope>(*this, *templ));
        append(std::move(templ));
        addSymbol(dgn, myDeclarations.back()->symbol(), *myDeclarations.back());
    };

    auto symSpace = findSymbolSpace("dtor");
    if ( !symSpace ) {
        makeTempl();
        symSpace = findSymbolSpace("dtor");
    }

    auto decl = symSpace->findEquivalent({});
    if ( !decl ) {
        makeTempl();
        decl = symSpace->findEquivalent({});
    }

    auto templ = decl->as<TemplateDeclaration>();
    if ( !templ ) {
        dgn.error(module(), *decl) << "dtor must be declared as a procedure";
        return;
    }

    decl = templ->definition()->findOverload(endModule, dgn, SymbolReference("")).decl();
    if ( !decl ) {
        auto proc = createDefaultDestructor();
        auto p = proc.get();
        templ->definition()->append(std::move(proc));
        p->resolvePrototypeSymbols(endModule, dgn);
        templ->definition()->addSymbol(dgn, p->symbol(), *p);
        p->resolveSymbols(endModule, dgn);
        myDestructor = p;
        return;
    }

    auto proc = decl->as<ProcedureDeclaration>();
    if ( !proc ) {
        dgn.error(module(), *decl) << "dtor must be declared as a procedure";
        return;
    }

    myDestructor = proc;
}

std::unique_ptr<ProcedureDeclaration> DataProductScope::createDefaultDestructor()
{
    auto proc = std::make_unique<ProcedureDeclaration>(
        Symbol(lexer::Token(lexer::TokenKind::Identifier,
                            declaration()->symbol().identifier().line(),
                            declaration()->symbol().identifier().column(),
                            "")),
        nullptr);

    auto ps = std::make_unique<ProcedureScope>(*this, *proc);

    for ( auto f = myFields.rbegin(); f != myFields.rend(); ++f ) {
        auto dp = (*f)->constraint().declaration()->as<DataProductDeclaration>();
        if ( !dp )
            continue;

        if ( auto dtor = dp->definition()->destructor() )
            ps->append(createMemberCallExpression(*dtor, **f));
    }

    proc->define(std::move(ps));
    return proc;
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

ProcedureDeclaration const* DataProductScope::destructor() const
{
    return myDestructor;
}

//
// ProcedureScope

ProcedureScope::ProcedureScope(DeclarationScope& parent,
                               ProcedureDeclaration& declaration)
    : ProcedureScope(parent, declaration, nullptr)
{
}

ProcedureScope::ProcedureScope(DeclarationScope& parent,
                               ProcedureDeclaration& declaration,
                               BasicBlock* mergeBlock)
    : ProcedureScope(parent, declaration, mergeBlock, lexer::Token(), lexer::Token())
{
}

ProcedureScope::ProcedureScope(DeclarationScope& parent,
                               ProcedureDeclaration& declaration,
                               BasicBlock* mergeBlock,
                               lexer::Token const& openToken,
                               lexer::Token const& label)
    : DeclarationScope(Kind::Procedure, &parent.module(), &parent, &declaration)
    , myMergeBlock(mergeBlock)
    , myOpenToken(openToken)
    , myLabel(label)
{
    createBasicBlock();
}

ProcedureScope::ProcedureScope(ProcedureScope const& rhs)
    : DeclarationScope(rhs)
    , myMergeBlock(rhs.myMergeBlock)
    , myOpenToken(rhs.myOpenToken)
    , myLabel(rhs.myLabel)
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
    swap(myMergeBlock, rhs.myMergeBlock);
    swap(myOpenToken, rhs.myOpenToken);
    swap(myLabel, rhs.myLabel);
    swap(myBasicBlocks, rhs.myBasicBlocks);
    swap(myChildScopes, rhs.myChildScopes);
}

void ProcedureScope::io(IStream& stream) const
{
    DeclarationScope::io(stream);
    if ( myLabel.kind() != lexer::TokenKind::Undefined )
        stream.next("name", myLabel.lexeme());

    stream.openArray("blocks");
    for ( auto& bb : myBasicBlocks )
        bb->io(stream);
    stream.closeArray();

    for ( auto& s : myChildScopes )
        s->io(stream);
}

IMPL_CLONE_BEGIN(ProcedureScope, DeclarationScope, DeclarationScope)
IMPL_CLONE_CHILD(myBasicBlocks)
IMPL_CLONE_CHILD(myChildScopes)
IMPL_CLONE_END
IMPL_CLONE_REMAP_BEGIN(ProcedureScope, DeclarationScope)
IMPL_CLONE_REMAP(myMergeBlock)
IMPL_CLONE_REMAP(myBasicBlocks)
IMPL_CLONE_REMAP(myChildScopes)
IMPL_CLONE_REMAP_END

void ProcedureScope::resolveSymbols(Module& endModule, Diagnostics& dgn)
{
    // Resolve declarations
    DeclarationScope::resolveSymbols(endModule, dgn);

    // Resolve expressions
    ScopeResolver resolver(*this);
    Context ctx(endModule, dgn, resolver);
    for ( auto& bb : myBasicBlocks )
        bb->resolveSymbols(ctx);

    for ( auto& s : myChildScopes )
        s->resolveSymbols(endModule, dgn);
}

bool ProcedureScope::isJumpTarget() const
{
    return myOpenToken.kind() != lexer::TokenKind::Undefined;
}

BasicBlock const* ProcedureScope::mergeBlock() const
{
    return myMergeBlock;
}

BasicBlock* ProcedureScope::mergeBlock()
{
    return myMergeBlock;
}

lexer::Token const& ProcedureScope::openToken() const
{
    return myOpenToken;
}

lexer::Token const& ProcedureScope::label() const
{
    return myLabel;
}

ProcedureDeclaration* ProcedureScope::declaration()
{
    return static_cast<ProcedureDeclaration*>(myDeclaration);
}

ProcedureDeclaration const* ProcedureScope::declaration() const
{
    return static_cast<ProcedureDeclaration const*>(myDeclaration);
}

Slice<ProcedureScope*> ProcedureScope::childScopes()
{
    return myChildScopes;
}

Slice<ProcedureScope*> const ProcedureScope::childScopes() const
{
    return myChildScopes;
}

Slice<BasicBlock*> ProcedureScope::basicBlocks()
{
    return myBasicBlocks;
}

Slice<BasicBlock*> const ProcedureScope::basicBlocks() const
{
    return myBasicBlocks;
}

void ProcedureScope::append(std::unique_ptr<Expression> expr)
{
    if ( myBasicBlocks.back()->junction() )
        createBasicBlock();

    myBasicBlocks.back()->append(std::move(expr));
}

void ProcedureScope::appendConstruction(std::unique_ptr<VarExpression> expr)
{
    if ( myBasicBlocks.back()->junction() )
        createBasicBlock();

    myBasicBlocks.back()->appendConstruction(std::move(expr));
}

BasicBlock* ProcedureScope::createBasicBlock()
{
    myBasicBlocks.emplace_back(std::make_unique<BasicBlock>(this));
    return myBasicBlocks.back().get();
}

void ProcedureScope::popBasicBlock()
{
    myBasicBlocks.pop_back();
}

ProcedureScope* ProcedureScope::createChildScope(BasicBlock* mergeBlock,
                                                 lexer::Token const& openToken,
                                                 lexer::Token const& label)
{
    myChildScopes.push_back(std::make_unique<ProcedureScope>(*this, *declaration(), mergeBlock, openToken, label));
    return myChildScopes.back().get();
}

ProcedureScope* ProcedureScope::createChildScope(BasicBlock* mergeBlock)
{
    return createChildScope(mergeBlock, lexer::Token(), lexer::Token());
}

//
// TemplateScope

TemplateScope::TemplateScope(DeclarationScope& parent,
                             TemplateDeclaration& declaration)
    : DeclarationScope(Kind::Template, &parent.module(), &parent, &declaration)
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

void TemplateScope::resolveSymbols(Module& endModule, Diagnostics& dgn)
{
    DeclarationScope::resolveSymbols(endModule, dgn);
}

TemplateDeclaration* TemplateScope::declaration()
{
    return static_cast<TemplateDeclaration*>(myDeclaration);
}

    } // namespace ast
} // namespace kyfoo
