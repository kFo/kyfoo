#include <kyfoo/ast/Scopes.hpp>

#include <cassert>
#include <functional>

#include <kyfoo/Diagnostics.hpp>
#include <kyfoo/Utility.hpp>

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
    swap(myLambdas, rhs.myLambdas);
    swap(mySymbols, rhs.mySymbols);
    swap(myImports, rhs.myImports);
}

void DeclarationScope::io(IStream& stream) const
{
    stream.next("declarations", myDeclarations);
}

IMPL_CLONE_NOBASE_BEGIN(DeclarationScope, DeclarationScope)
IMPL_CLONE_CHILD(myDeclarations)
IMPL_CLONE_CHILD(myLambdas)
IMPL_CLONE_END
IMPL_CLONE_REMAP_NOBASE_BEGIN(DeclarationScope)
IMPL_CLONE_REMAP(myModule)
IMPL_CLONE_REMAP(myDeclaration)
IMPL_CLONE_REMAP(myParent)
IMPL_CLONE_REMAP(myDeclarations)
IMPL_CLONE_REMAP(myLambdas)
IMPL_CLONE_REMAP_END

void DeclarationScope::resolveImports(Diagnostics& dgn)
{
    for ( auto& e : myDeclarations ) {
        if ( auto d = e->as<ImportDeclaration>() ) {
            module().import(dgn, d->token());
        }
    }
}

SymRes DeclarationScope::resolveSymbols(Module& endModule, Diagnostics& dgn)
{
    SymbolDependencyTracker tracker(module(), dgn);
    for ( auto const& d : myDeclarations )
        traceDependencies(tracker, *d);

    if ( dgn.errorCount() )
        return SymRes::Fail;

    tracker.sortPasses();

    ScopeResolver resolver(*this);
    Context ctx(endModule, dgn, resolver);
    SymRes ret = SymRes::Success;

    // Resolve top-level declarations
    for ( auto const& symGroup : tracker.groups ) {
        for ( auto const& d : symGroup->declarations ) {
            ret |= ctx.resolveDeclaration(*d);
            addSymbol(dgn, d->symbol(), *d);
        }
    }

    if ( dgn.errorCount() )
        return ret;

    // Resolve lambdas
    for ( auto& l : myLambdas )
        ret |= ctx.resolveDeclaration(*l);

    if ( dgn.errorCount() )
        return ret;

    // Resolve definitions
    for ( auto& defn : myDefinitions ) {
        if ( defn->declaration()->symbol().prototype().isConcrete() ) {
            ret |= defn->resolveSymbols(endModule, dgn);
        }
    }

    return ret;
}

void DeclarationScope::resolveAttributes(Module& endModule, Diagnostics& dgn)
{
    ScopeResolver resolver(*this);
    Context ctx(endModule, dgn, resolver);
    for ( auto& d : myDeclarations )
        d->resolveAttributes(ctx);

    for ( auto& s : myDefinitions )
        s->resolveAttributes(endModule, dgn);
}

LookupHit DeclarationScope::findEquivalent(SymbolReference const& symbol) const
{
    auto symSpace = findSymbolSpace(symbol.name());
    if ( symSpace )
        return LookupHit(symSpace, symSpace->findEquivalent(symbol.pattern()));

    if ( myDeclaration && symbol.pattern().empty() )
        return LookupHit(symSpace, myDeclaration->symbol().prototype().findVariable(symbol.name()));

    return LookupHit();
}

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

void DeclarationScope::append(std::unique_ptr<DeclarationScope> definition)
{
    myDefinitions.emplace_back(std::move(definition));
    myDefinitions.back()->myParent = this;
}

void DeclarationScope::appendLambda(std::unique_ptr<ProcedureDeclaration> proc,
                                    std::unique_ptr<ProcedureScope> defn)
{
    myLambdas.emplace_back(std::move(proc));
    myLambdas.back()->setScope(*this);
    append(std::move(defn));
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
    auto symSpace = createSymbolSpace(dgn, sym.token().lexeme());

    if ( auto other = symSpace->findEquivalent(sym.prototype().pattern()) ) {
        auto templDecl = decl.as<TemplateDeclaration>();
        if ( !templDecl ) {
            auto& err = dgn.error(module(), sym.token()) << "symbol is already defined";
            err.see(*other);
            return false;
        }

        auto otherTemplDecl = other->as<TemplateDeclaration>();
        if ( !otherTemplDecl ) {
            auto& err = dgn.error(module(), sym.token()) << "symbol was not first defined as a template";
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

Slice<Declaration const*> DeclarationScope::childDeclarations() const
{
    return myDeclarations;
}

Slice<DeclarationScope const*> DeclarationScope::childDefinitions() const
{
    return myDefinitions;
}

Slice<ProcedureDeclaration const*> DeclarationScope::childLambdas() const
{
    return myLambdas;
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

SymRes DataSumScope::resolveSymbols(Module& endModule, Diagnostics& dgn)
{
    for ( auto const& d : myDeclarations ) {
        auto dsCtor = d->as<DataSumDeclaration::Constructor>();
        if ( !dsCtor )
            throw std::runtime_error("data sum must only contain constructors");

        myCtors.push_back(dsCtor);
    }

    return DeclarationScope::resolveSymbols(endModule, dgn);
}

DataSumDeclaration* DataSumScope::declaration()
{
    return static_cast<DataSumDeclaration*>(myDeclaration);
}

Slice<DataSumDeclaration::Constructor*> DataSumScope::constructors()
{
    return myCtors;
}

Slice<DataSumDeclaration::Constructor const*> DataSumScope::constructors() const
{
    return myCtors;
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

SymRes DataProductScope::resolveSymbols(Module& endModule, Diagnostics& dgn)
{
    for ( auto& d : myDeclarations )
        if ( auto v = d->as<DataProductDeclaration::Field>() )
            myFields.push_back(v);

    auto ret = DeclarationScope::resolveSymbols(endModule, dgn);

    resolveConstructors(endModule, dgn);
    resolveDestructor(endModule, dgn);

    return ret;
}

SymRes DataProductScope::resolveConstructors(Module& endModule, Diagnostics& dgn)
{
    // todo: only make default ctor when no other ctor/dtor defined

    ScopeResolver resolver(*this);
    Context ctx(endModule, dgn, resolver);

    SymRes ret;
    for ( auto const& d : myDeclarations ) {
        if ( d->symbol().token().lexeme() == "ctor" && d->symbol().prototype().pattern().empty() ) {
            auto templ = d->as<TemplateDeclaration>();
            if ( !templ )
                continue;

            if ( auto reflectedTempl = reflectBuilder(*templ) ) {
                ret |= ctx.resolveDeclaration(*reflectedTempl);
                if ( !ret )
                    return ret;

                ret |= reflectedTempl->definition()->resolveSymbols(endModule, dgn);
                if ( !ret )
                    return ret;

                parent()->addSymbol(dgn, reflectedTempl->symbol(), *reflectedTempl);
            }
        }
    }

    return ret;
}

std::unique_ptr<ProcedureDeclaration> DataProductScope::createDefaultConstructor()
{
    return nullptr;
}

TemplateDeclaration* DataProductScope::reflectBuilder(TemplateDeclaration const& ctorTempl)
{
    auto builder = std::make_unique<TemplateDeclaration>(makeSym(makeToken("mk"),
                                                                 createPtrList<Expression>(createIdentifier(*declaration()))));
    auto builderDefn = std::make_unique<TemplateScope>(*module().scope(), *builder);

    auto const defn = ctorTempl.definition();
    for ( auto const& d : defn->childDeclarations() ) {
        if ( d->symbol().token().lexeme().empty() ) {
            auto procCtor = d->as<ProcedureDeclaration>();
            if ( !procCtor )
                continue;

            auto proc = std::make_unique<ProcedureDeclaration>(copyProcSym(d->symbol()),
                                                               createIdentifier(*declaration()));
            auto procDefn = std::make_unique<ProcedureScope>(*builderDefn, *proc);

            auto v_ = std::make_unique<VariableDeclaration>(Symbol(makeToken("r")),
                                                            *procDefn,
                                                            createPtrList<Expression>(createIdentifier(*declaration())));
            auto v = v_.get();
            static_cast<DeclarationScope&>(*procDefn).append(std::move(v_));
            procDefn->append(createMemberCall(*procCtor, *v, createTuple(proc->symbol().prototype().pattern())));
            procDefn->basicBlocks().back()->setJunction(
                std::make_unique<ReturnJunction>(makeToken("return"), createIdentifier(*v)));
            
            proc->define(procDefn.get());
            builderDefn->append(std::move(procDefn));
            builderDefn->append(std::move(proc));
        }
    }

    builder->define(builderDefn.get());
    module().scope()->append(std::move(builderDefn));

    auto ret = builder.get();
    module().scope()->append(std::move(builder));
    return ret;
}

void DataProductScope::resolveDestructor(Module& endModule, Diagnostics& dgn)
{
    auto makeTempl = [this, &dgn] {
        auto templ = std::make_unique<TemplateDeclaration>(
            Symbol(lexer::Token(lexer::TokenKind::Identifier,
                                declaration()->symbol().token().line(),
                                declaration()->symbol().token().column(),
                                "dtor")));
        auto templDefn = std::make_unique<TemplateScope>(*this, *templ);
        templ->define(templDefn.get());
        append(std::move(templDefn));
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

    ScopeResolver narrowResolver(*templ->definition(), IResolver::Narrow);
    Context ctx(endModule, dgn, narrowResolver);
    decl = ctx.matchOverload("").decl();
    if ( !decl ) {
        auto proc = createDefaultDestructor();
        auto p = proc.get();
        templ->definition()->append(std::move(proc));
        ScopeResolver resolver(*templ->definition());
        ctx.changeResolver(resolver);
        ctx.resolveDeclaration(*p);
        templ->definition()->addSymbol(dgn, p->symbol(), *p);
        p->definition()->resolveSymbols(endModule, dgn);
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
                            declaration()->symbol().token().line(),
                            declaration()->symbol().token().column(),
                            "")),
        nullptr);

    auto ps = std::make_unique<ProcedureScope>(*this, *proc);

    for ( auto f = myFields.rbegin(); f != myFields.rend(); ++f ) {
        auto id = identify(*(*f)->type());
        if ( !id )
            continue;

        auto dp = id->declaration()->as<DataProductDeclaration>();
        if ( !dp )
            continue;

        if ( auto dtor = dp->definition()->destructor() )
            ps->append(createMemberCall(*dtor, **f));
    }

    proc->define(ps.get());
    append(std::move(ps));
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

Slice<DataProductDeclaration::Field const*> DataProductScope::fields() const
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

SymRes ProcedureScope::resolveSymbols(Module& endModule, Diagnostics& dgn)
{
    // Resolve declarations
    auto ret = DeclarationScope::resolveSymbols(endModule, dgn);

    // Resolve expressions
    ScopeResolver resolver(*this);
    Context ctx(endModule, dgn, resolver);
    for ( auto& bb : myBasicBlocks )
        ret |= bb->resolveSymbols(ctx);

    for ( auto& s : myChildScopes )
        ret |= s->resolveSymbols(endModule, dgn);

    if ( !ret )
        return ret;

    if ( !isTop() )
        return ret;

    return cacheVariableExtents(ctx);
}

bool ProcedureScope::isJumpTarget() const
{
    return myOpenToken.kind() != lexer::TokenKind::Undefined;
}

bool ProcedureScope::isTop() const
{
    if ( !myParent )
        return true;

    if ( auto s = myParent->as<ProcedureScope>() )
        return s->declaration() != declaration();

    return true;
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

Slice<ProcedureScope const*> ProcedureScope::childScopes() const
{
    return myChildScopes;
}

Slice<BasicBlock*> ProcedureScope::basicBlocks()
{
    return myBasicBlocks;
}

Slice<BasicBlock const*> ProcedureScope::basicBlocks() const
{
    return myBasicBlocks;
}

void ProcedureScope::append(std::unique_ptr<Expression> expr)
{
    if ( myBasicBlocks.back()->junction() )
        createBasicBlock();

    myBasicBlocks.back()->append(std::move(expr));
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

namespace {
template <typename Dispatcher>
struct Sequencer
{
    using result_t = void;
    using extent_set_t = std::set<Extent*, ExtentCompare>;

    Dispatcher& dispatch;

    Context& ctx;
    extent_set_t& extents;
    BasicBlock const& basicBlock;
    bool refCtx = false;

    Sequencer(Dispatcher& dispatch,
              Context& ctx,
              extent_set_t& extents,
              BasicBlock const& bb)
        : dispatch(dispatch)
        , ctx(ctx)
        , extents(extents)
        , basicBlock(bb)
    {
    }

    result_t recurse(Slice<Expression const*> exprs)
    {
        for ( auto e : exprs )
            dispatch(*e);
    }

    Extent::Usage::Kind asRead()
    {
        return refCtx ? Extent::Usage::Ref : Extent::Usage::Read;
    }

    result_t exprLiteral(LiteralExpression const&)
    {
        // nop
    }

    result_t exprIdentifier(IdentifierExpression const& id)
    {
        if ( auto d = id.declaration() ) {
            auto ext = extents.find(*d);
            if ( ext != extents.end() )
                (*ext)->appendUsage(basicBlock, id, asRead());
        }
    }

    result_t exprTuple(TupleExpression const& t)
    {
        recurse(t.expressions());
    }

    result_t exprApply(ApplyExpression const& a)
    {
        dispatch(*a.subject());
        auto decl = getDeclaration(a.subject());
        auto declExt = extents.find(*decl);
        if ( declExt != end(extents) )
            (*declExt)->appendUsage(basicBlock, a, Extent::Usage::Ref);

        auto args = a.arguments();
        if ( auto proc = decl->as<ProcedureDeclaration>() ) {
            auto o = proc->ordinals();
            auto p = proc->parameters();
            for ( std::size_t i = 0; i < args.size(); ++i ) {
                check_point refCtx;
                refCtx = o[i] >= 0 && isReference(ctx, *p[o[i]]->type());
                dispatch(*args[i]);
            }

            return;
        }

        recurse(args);
    }

    result_t exprSymbol(SymbolExpression const& s)
    {
        recurse(s.expressions());
        exprIdentifier(s);
    }

    result_t exprDot(DotExpression const& d)
    {
        recurse(d.expressions());
    }

    result_t exprAssign(AssignExpression const& v)
    {
        if ( auto d = getDeclaration(v.right()) ) {
            auto ext = extents.find(*d);
            if ( ext != end(extents) ) {
                // todo: movable types
                //(*ext)->appendUsage(basicBlock, v, Extent::Usage::Move);
                (*ext)->appendUsage(basicBlock, v, Extent::Usage::Read);
            }
        }
        else {
            dispatch(v.right());
        }

        if ( auto d = getDeclaration(v.left()) ) {
            auto ext = extents.find(*d);
            if ( ext != end(extents) )
                (*ext)->appendUsage(basicBlock, v, Extent::Usage::Write);
        }
        else {
            dispatch(v.left());
        }
    }

    result_t exprLambda(LambdaExpression const& l)
    {
        dispatch(l.parameters());
        dispatch(l.body());
    }

    result_t exprArrow(ArrowExpression const& a)
    {
        dispatch(a.from());
        dispatch(a.to());
    }

    result_t exprUniverse(UniverseExpression const&)
    {
        // nop
    }
};

} // namespace

SymRes ProcedureScope::cacheVariableExtents(Context& ctx)
{
    if ( myBasicBlocks.empty() )
        return SymRes::Success;

    for ( auto sym : declaration()->symbol().prototype().symbolVariables() )
        myExtents.emplace_back(*sym);

    for ( auto param : declaration()->parameters() )
        myExtents.emplace_back(*param);

    ycomb(
        [this](auto rec, ProcedureScope const& scope) -> void {
            for ( auto d : scope.childDeclarations() )
                if ( d->kind() == DeclKind::Variable )
                    myExtents.emplace_back(*d);

            for ( auto s : scope.childScopes() )
                rec(*s);
        })(*this);

    Sequencer<ShallowApply<Sequencer>>::extent_set_t extents;
    for ( auto& ext : myExtents )
        extents.insert(&ext);

    for ( FlowTracer trace(*myBasicBlocks.front()); ; ) {
        auto bb = trace.currentBlock();
        for ( auto& ext : myExtents )
            ext.appendBlock(*bb);

        ShallowApply<Sequencer> op(ctx, extents, *bb);
        for ( auto stmt : bb->statements() )
            op(stmt->expression());

        if ( auto br = bb->junction()->as<BranchJunction>() ) {
            if ( br->statement() )
                op(*br->condition());
        }
        else if ( auto ret = bb->junction()->as<ReturnJunction>() ) {
            if ( ret->statement() )
                op(*ret->expression());
        }

        if ( trace.advanceBlock() != FlowTracer::Forward ) {
            auto flow = trace.advancePath();
            while ( flow == FlowTracer::Loop )
                flow = trace.advancePath();

            if ( flow == FlowTracer::None )
                break;
        }
    }

    // Flow connectivity
    for ( auto& ext : myExtents ) {
        for ( auto b : ext.blocks() ) {
            for ( auto incoming : b->bb->incoming() ) {
                for ( auto b2 : ext.blocks() ) {
                    if ( b2->bb == incoming ) {
                        b2->succ.push_back(b);
                        b->pred.push_back(b2);
                        break;
                    }
                }
            }
        }
    }

    for ( auto& ext : myExtents )
        ext.pruneEmptyBlocks();

    SymRes ret;
    for ( auto& ext : myExtents )
        ret |= ext.cacheLocalFlows(ctx);

    return ret;
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

SymRes TemplateScope::resolveSymbols(Module& endModule, Diagnostics& dgn)
{
    return DeclarationScope::resolveSymbols(endModule, dgn);
}

TemplateDeclaration* TemplateScope::declaration()
{
    return static_cast<TemplateDeclaration*>(myDeclaration);
}

    } // namespace ast
} // namespace kyfoo
