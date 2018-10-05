#include <kyfoo/ast/Scopes.hpp>

#include <cassert>
#include <functional>

#include <kyfoo/Diagnostics.hpp>
#include <kyfoo/Utilities.hpp>

#include <kyfoo/ast/Axioms.hpp>
#include <kyfoo/ast/ControlFlow.hpp>
#include <kyfoo/ast/Declarations.hpp>
#include <kyfoo/ast/Expressions.hpp>
#include <kyfoo/ast/Fabrication.hpp>
#include <kyfoo/ast/Module.hpp>
#include <kyfoo/ast/Semantics.hpp>
#include <kyfoo/ast/Context.hpp>
#include <kyfoo/ast/Overloading.hpp>
#include <kyfoo/ast/Visitors.hpp>

namespace kyfoo::ast {

//
// DeclarationScope

Scope::Scope(Kind kind,
                                   Module* module,
                                   Scope* parent)
    : myKind(kind)
    , myModule(module)
    , myParent(parent)
{
    mySymbols.emplace_back(this, "");
}

Scope::Scope(Module& module)
    : Scope(Kind::Base, &module, nullptr)
{
}

Scope::Scope(Scope* parent)
    : Scope(Kind::Base, &parent->module(), parent)
{
}

Scope::Scope(Scope const& rhs)
    : myKind(rhs.myKind)
    , myModule(rhs.myModule)
    , myDeclaration(rhs.myDeclaration)
    , myParent(rhs.myParent)
{
    // mySymbols, myProcedures, and myImports are to be filled out
    // by the semantic passes that build these indices
}

Scope& Scope::operator = (Scope const& rhs)
{
    Scope(rhs).swap(*this);
    return *this;
}

Scope::~Scope() = default;

void Scope::swap(Scope& rhs) noexcept
{
    using kyfoo::swap;
    swap(myKind, rhs.myKind);
    swap(myModule, rhs.myModule);
    swap(myDeclaration, rhs.myDeclaration);
    swap(myParent, rhs.myParent);
    swap(myDeclarations, rhs.myDeclarations);
    swap(myDefinitions, rhs.myDefinitions);
    swap(myLambdas, rhs.myLambdas);
    swap(mySymbols, rhs.mySymbols);
    swap(myImports, rhs.myImports);
}

IMPL_CLONE_NOBASE_BEGIN(Scope, Scope)
IMPL_CLONE_CHILD(myDeclarations)
IMPL_CLONE_CHILD(myDefinitions)
IMPL_CLONE_CHILD(myLambdas)
IMPL_CLONE_END
IMPL_CLONE_REMAP_NOBASE_BEGIN(Scope)
IMPL_CLONE_REMAP(myModule)
IMPL_CLONE_REMAP(myDeclaration)
IMPL_CLONE_REMAP(myParent)
IMPL_CLONE_REMAP(myDeclarations)
IMPL_CLONE_REMAP(myDefinitions)
IMPL_CLONE_REMAP(myLambdas)
IMPL_CLONE_REMAP_END

void Scope::resolveImports(Diagnostics& dgn)
{
    for ( auto& e : myDeclarations ) {
        if ( auto d = e->as<ImportDeclaration>() ) {
            module().import(dgn, d->token());
        }
    }

    for ( auto& defn : myDefinitions )
        defn->resolveImports(dgn);
}

SymRes Scope::resolveDeclarations(Context& ctx)
{
    SymRes ret = SymRes::Success;
    SymbolDependencyTracker tracker(module(), ctx.diagnostics());
    for ( auto const& d : myDeclarations )
        ret |= traceDependencies(tracker, *d);

    if ( ret.error() )
        return ret;

    tracker.sortPasses();

    // Resolve top-level declarations
    for ( auto const& symGroup : tracker.groups ) {
        if ( symGroup->upstreamErrors )
            continue;

        for ( auto d : symGroup->declarations ) {
            auto res = ctx.resolveDeclaration(*d);
            if ( res.error() ) {
                for ( auto dep : symGroup->dependents )
                    dep->upstreamErrors = true;
            }
            ret |= res;

            addSymbol(ctx.diagnostics(), d->symbol(), *d);
        }
    }

    if ( ret.error() )
        return ret;

    if ( ret.error() )
        return ret;

    for ( auto& decl : myDeclarations ) {
        if ( decl->symbol().prototype().isConcrete() )
            if ( auto defn = getDefinition(*decl) )
                ret |= ctx.resolveScopeDeclarations(*defn);
    }

    if ( ret.error() )
        return ret;

    if ( ret.error() )
        return ret;

    // Resolve lambdas
    for ( auto& l : myLambdas )
        ret |= ctx.resolveDeclaration(*l);

    return ret;
}

SymRes Scope::resolveDefinitions(Context& ctx)
{
    SymRes ret;

    // Resolve definitions
    for ( auto& defn : myDefinitions )
        if ( defn->declaration()->symbol().prototype().isConcrete() )
            ret |= ctx.resolveScopeDefinitions(*defn);

    if ( ret.error() )
        return ret;

    return ret;
}

SymRes Scope::resolveAttributes(Context& ctx)
{
    SymRes ret;
    for ( auto& d : myDeclarations )
        ret |= d->resolveAttributes(ctx);

    for ( auto& s : myDefinitions )
        ret |= ctx.resolveScopeAttributes(*s);

    return ret;
}

Lookup Scope::findEquivalent(SymbolReference const& symbol) const
{
    Lookup ret(symbol);
    auto symSpace = findSymbolSpace(symbol.name());
    if ( symSpace ) {
        ret.appendTrace(*symSpace);
        if ( auto decl = symSpace->findEquivalent(symbol.pattern()) )
            ret.resolveTo(*decl);
    }
    else if ( myDeclaration && symbol.pattern().empty() )
        if ( auto symVar = myDeclaration->symbol().prototype().findVariable(symbol.name()) )
            ret.resolveTo(*symVar);

    return ret;
}

void Scope::setDeclaration(DefinableDeclaration& declaration)
{
    myDeclaration = &declaration;
    myParent = &myDeclaration->scope();
}

Lookup Scope::findOverload(Context& ctx, SymbolReference const& sym) const
{
    Lookup hit(sym);
    auto symSpace = findSymbolSpace(sym.name());
    if ( symSpace ) {
        hit.appendTrace(*symSpace);
        hit.resolveTo(symSpace->findViableOverloads(ctx, sym.pattern()));
    }

    return hit;
}

void Scope::append(Box<Declaration> declaration)
{
    myDeclarations.emplace_back(std::move(declaration));
    myDeclarations.back()->setScope(*this);
}

void Scope::append(Box<Scope> definition)
{
    myDefinitions.emplace_back(std::move(definition));
    myDefinitions.back()->myParent = this;
}

void Scope::appendLambda(Box<ProcedureDeclaration> proc,
                                    Box<ProcedureScope> defn)
{
    myLambdas.emplace_back(std::move(proc));
    myLambdas.back()->setScope(*this);
    append(std::move(defn));
}

void Scope::import(Module& module)
{
    append(mk<ImportDeclaration>(Symbol(lexer::Token(lexer::TokenKind::Identifier, std::string(module.name()), lexer::SourceLocation()))));
}

void Scope::merge(Scope& rhs)
{
    myDeclarations.reserve(myDeclarations.size() + rhs.myDeclarations.size());
    for ( auto& e : rhs.myDeclarations ) {
        myDeclarations.emplace_back(std::move(e));
        myDeclarations.back()->setScope(*this);
    }

    rhs.myDeclarations.clear();
}

SymbolSpace* Scope::createSymbolSpace(Diagnostics&, std::string_view name)
{
    auto symLess = [](SymbolSpace const& s, std::string_view name) { return s.name() < name; };
    auto l = lower_bound(begin(mySymbols), end(mySymbols), name, symLess);
    if ( l != end(mySymbols) && l->name() == name )
        return &*l;

    l = mySymbols.insert(l, SymbolSpace(this, std::string(name)));
    return &*l;
}

bool Scope::addSymbol(Diagnostics& dgn,
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

SymbolSpace* Scope::findSymbolSpace(std::string_view name) const
{
    auto symLess = [](SymbolSpace const& s, std::string_view name) { return s.name() < name; };
    auto symSet = lower_bound(begin(mySymbols), end(mySymbols), name, symLess);
    if ( symSet != end(mySymbols) && symSet->name() == name )
        return &*symSet;

    return nullptr;
}

Scope::Kind Scope::kind() const
{
    return myKind;
}

Module& Scope::module()
{
    return *myModule;
}

Module const& Scope::module() const
{
    return *myModule;
}

DefinableDeclaration* Scope::declaration()
{
    return myDeclaration;
}

DefinableDeclaration const* Scope::declaration() const
{
    return myDeclaration;
}

Scope* Scope::parent()
{
    return myParent;
}

Scope const* Scope::parent() const
{
    return myParent;
}

Slice<Declaration const*> Scope::childDeclarations() const
{
    return myDeclarations;
}

Slice<Scope const*> Scope::childDefinitions() const
{
    return myDefinitions;
}

Slice<ProcedureDeclaration const*> Scope::childLambdas() const
{
    return myLambdas;
}

//
// DataSumScope

DataSumScope::DataSumScope(Scope& parent,
                           DataSumDeclaration& declaration)
    : Scope(Kind::DataSum, &parent.module(), &parent)
{
    declaration.define(*this);
}

DataSumScope::DataSumScope(DataSumScope const& rhs)
    : Scope(rhs)
{
}

DataSumScope& DataSumScope::operator = (DataSumScope const& rhs)
{
    DataSumScope(rhs).swap(*this);
    return *this;
}

DataSumScope::~DataSumScope() = default;

void DataSumScope::swap(DataSumScope& rhs) noexcept
{
    Scope::swap(rhs);
}

IMPL_CLONE_BEGIN(DataSumScope, Scope, Scope)
IMPL_CLONE_END
IMPL_CLONE_REMAP_BEGIN(DataSumScope, Scope)
IMPL_CLONE_REMAP_END

SymRes DataSumScope::resolveDeclarations(Context& ctx)
{
    for ( auto const& d : myDeclarations ) {
        auto dsCtor = d->as<DataSumDeclaration::Constructor>();
        if ( !dsCtor )
            throw std::runtime_error("data sum must only contain constructors");

        myCtors.push_back(dsCtor);
    }

    return Scope::resolveDeclarations(ctx);
}

SymRes DataSumScope::resolveDefinitions(Context& ctx)
{
    return Scope::resolveDefinitions(ctx);
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

DataProductScope::DataProductScope(Scope& parent,
                                   DataProductDeclaration& declaration)
    : Scope(Kind::DataProduct, &parent.module(), &parent)
{
    declaration.define(*this);
}

DataProductScope::DataProductScope(DataProductScope const& rhs)
    : Scope(rhs)
{
    // myFields is populated by the semantic pass
}

DataProductScope& DataProductScope::operator = (DataProductScope const& rhs)
{
    DataProductScope(rhs).swap(*this);
    return *this;
}

DataProductScope::~DataProductScope() = default;

void DataProductScope::swap(DataProductScope& rhs) noexcept
{
    Scope::swap(rhs);
    using kyfoo::swap;
    swap(myFields, rhs.myFields);
}

IMPL_CLONE_BEGIN(DataProductScope, Scope, Scope)
IMPL_CLONE_END
IMPL_CLONE_REMAP_BEGIN(DataProductScope, Scope)
IMPL_CLONE_REMAP_END

SymRes DataProductScope::resolveDeclarations(Context& ctx)
{
    for ( auto& d : myDeclarations )
        if ( auto v = d->as<DataProductDeclaration::Field>() )
            myFields.push_back(v);

    return Scope::resolveDeclarations(ctx);
}

SymRes DataProductScope::resolveDefinitions(Context& ctx)
{
    return Scope::resolveDefinitions(ctx);
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

//
// ProcedureScope

ProcedureScope::ProcedureScope(Scope& parent,
                               ProcedureDeclaration& declaration)
    : ProcedureScope(parent, declaration, nullptr)
{
}

ProcedureScope::ProcedureScope(Scope& parent,
                               ProcedureDeclaration& declaration,
                               BasicBlock* mergeBlock)
    : ProcedureScope(parent, declaration, mergeBlock, lexer::Token(), lexer::Token())
{
}

ProcedureScope::ProcedureScope(Scope& parent,
                               ProcedureDeclaration& declaration,
                               BasicBlock* mergeBlock,
                               lexer::Token openToken,
                               lexer::Token label)
    : Scope(Kind::Procedure, &parent.module(), &parent)
    , myMergeBlock(mergeBlock)
    , myOpenToken(std::move(openToken))
    , myLabel(std::move(label))
{
    declaration.define(*this);
    createBasicBlock();
}

ProcedureScope::ProcedureScope(ProcedureScope& parent,
                               BasicBlock* mergeBlock,
                               lexer::Token openToken,
                               lexer::Token label)
    : Scope(Kind::Procedure, &parent.module(), &parent)
    , myMergeBlock(mergeBlock)
    , myOpenToken(std::move(openToken))
    , myLabel(std::move(label))
{
    myDeclaration = parent.declaration();
    createBasicBlock();
}

ProcedureScope::ProcedureScope(ProcedureScope const& rhs)
    : Scope(rhs)
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

void ProcedureScope::swap(ProcedureScope& rhs) noexcept
{
    Scope::swap(rhs);
    using kyfoo::swap;
    swap(myMergeBlock, rhs.myMergeBlock);
    swap(myOpenToken, rhs.myOpenToken);
    swap(myLabel, rhs.myLabel);
    swap(myBasicBlocks, rhs.myBasicBlocks);
    swap(myChildScopes, rhs.myChildScopes);
}

IMPL_CLONE_BEGIN(ProcedureScope, Scope, Scope)
IMPL_CLONE_CHILD(myBasicBlocks)
IMPL_CLONE_CHILD(myChildScopes)
IMPL_CLONE_END
IMPL_CLONE_REMAP_BEGIN(ProcedureScope, Scope)
IMPL_CLONE_REMAP(myMergeBlock)
IMPL_CLONE_REMAP(myBasicBlocks)
IMPL_CLONE_REMAP(myChildScopes)
IMPL_CLONE_REMAP_END

SymRes ProcedureScope::resolveDeclarations(Context& ctx)
{
    return Scope::resolveDeclarations(ctx);
}

SymRes ProcedureScope::resolveDefinitions(Context& ctx)
{
    auto ret = Scope::resolveDefinitions(ctx);

    for ( auto& bb : myBasicBlocks )
        ret |= bb->resolveSymbols(ctx);

    for ( auto& s : myChildScopes )
        ret |= ctx.resolveScope(*s);

    if ( !ret )
        return ret;

    if ( !isTop() )
        return ret;

    cacheDominators();
    ret |= cacheVariableExtents(ctx);

    return ret;
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

BasicBlock* ProcedureScope::entryBlock()
{
    return myBasicBlocks.front().get();
}

BasicBlock const* ProcedureScope::entryBlock() const
{
    return myBasicBlocks.front().get();
}

void ProcedureScope::append(Box<Expression> expr)
{
    if ( myBasicBlocks.back()->junction() )
        createBasicBlock();

    myBasicBlocks.back()->append(std::move(expr));
}

BasicBlock* ProcedureScope::createBasicBlock()
{
    myBasicBlocks.emplace_back(mk<BasicBlock>(this));
    return myBasicBlocks.back().get();
}

void ProcedureScope::popBasicBlock()
{
    myBasicBlocks.pop_back();
}

ProcedureScope* ProcedureScope::createChildScope(BasicBlock* mergeBlock,
                                                 lexer::Token openToken,
                                                 lexer::Token label)
{
    Box<ProcedureScope> child(new ProcedureScope(*this, mergeBlock, std::move(openToken), std::move(label)));
    myChildScopes.push_back(std::move(child));
    return myChildScopes.back().get();
}

void ProcedureScope::cacheDominators()
{
    entryBlock()->myDominators.insert(entryBlock());

    FlatSet<BasicBlock*> all;
    ycomb([&all](auto rec, ProcedureScope& scope) -> void {
        for ( auto b : scope.basicBlocks() )
            all.insert(b);

        for ( auto c : scope.childScopes() )
            rec(*c);
    })(*this);

    auto allButEntry = all;
    allButEntry.remove(entryBlock());

    for ( auto b : allButEntry )
        b->myDominators = all;

    for ( bool change = true; change; ) {
        change = false;
        for ( auto b : allButEntry ) {
            auto oldSize = b->myDominators.size();
            for ( auto p : b->incoming() )
                b->myDominators.intersectWith(p->myDominators);

            b->myDominators.insert(b);
            if ( b->myDominators.size() != oldSize )
                change = true;
        }
    }

    for ( auto b : allButEntry ) {
        auto idom = b->myDominators;
        idom.remove(b);
        for ( auto d : b->myDominators ) {
            if ( d == b )
                continue;

            auto dd = d->myDominators;
            dd.remove(d);
            idom.diffWith(dd);
        }

        assert(idom.size() == 1);
        b->myImmediateDominator = idom[0];
    }
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
    bool writeCtx = false;

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

    Extent::Usage::Kind currentUsage()
    {
        if ( writeCtx )
            return Extent::Usage::Write;

        if ( refCtx )
            return Extent::Usage::Ref;
        
        return Extent::Usage::Read;
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
                (*ext)->appendUsage(basicBlock, id, currentUsage());
        }
    }

    result_t exprTuple(TupleExpression const& t)
    {
        recurse(t.expressions());
    }

    result_t exprApply(ApplyExpression const& a)
    {
        check_point writeCtx;
        writeCtx = false;

        {
            check_point refCtx;
            refCtx = true;
            dispatch(*a.subject());
        }

        auto args = a.arguments();
        if ( auto proc = a.procedure() ) {
            auto o = proc->ordinals();
            auto p = proc->parameters();
            check_point refCtx;
            for ( uz i = 0; i < args.size(); ++i ) {
                refCtx = o[i] >= 0 && isReference(*p[o[i]]->type());
                dispatch(*args[i]);
            }

            return;
        }

        recurse(args);
    }

    result_t exprSymbol(SymbolExpression const& s)
    {
        check_point refCtx;
        check_point writeCtx;
        refCtx = writeCtx = false;

        recurse(s.expressions());
        exprIdentifier(s);
    }

    result_t exprDot(DotExpression const& d)
    {
        {
            check_point writeCtx;
            writeCtx = false;

            check_point refCtx;
            refCtx = true;
            for ( auto m : d.expressions()(0, $ - 1) )
                dispatch(*m);
        }

        dispatch(*d.expressions().back());
    }

    result_t exprAssign(AssignExpression const& v)
    {
        check_point writeCtx;
        check_point refCtx;

        dispatch(v.right());
        
        writeCtx = true;
        dispatch(v.left());
    }

    result_t exprLambda(LambdaExpression const&)
    {
        // nop
    }

    result_t exprArrow(ArrowExpression const& a)
    {
        check_point writeCtx;
        check_point refCtx;
        refCtx = writeCtx = false;

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

    // todo: crawl over declarations searching for uses
    /*for ( auto sym : declaration()->symbol().prototype().symbolVariables() )
        myExtents.emplace_back(*sym);*/

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

    for ( FlowTracer trace(*entryBlock()); ; ) {
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
            while ( flow == FlowTracer::Repeat )
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

TemplateScope::TemplateScope(Scope& parent,
                             TemplateDeclaration& declaration)
    : Scope(Kind::Template, &parent.module(), &parent)
{
    declaration.define(*this);
}

TemplateScope::TemplateScope(TemplateScope const& rhs)
    : Scope(rhs)
{
}

TemplateScope& TemplateScope::operator = (TemplateScope const& rhs)
{
    TemplateScope(rhs).swap(*this);
    return *this;
}

TemplateScope::~TemplateScope() = default;

void TemplateScope::swap(TemplateScope& rhs) noexcept
{
    Scope::swap(rhs);
}

IMPL_CLONE_BEGIN(TemplateScope, Scope, Scope)
IMPL_CLONE_END
IMPL_CLONE_REMAP_BEGIN(TemplateScope, Scope)
IMPL_CLONE_REMAP_END

SymRes TemplateScope::resolveDeclarations(Context& ctx)
{
    return Scope::resolveDeclarations(ctx);
}

SymRes TemplateScope::resolveDefinitions(Context& ctx)
{
    return Scope::resolveDefinitions(ctx);
}

TemplateDeclaration* TemplateScope::declaration()
{
    return static_cast<TemplateDeclaration*>(myDeclaration);
}

} // namespace kyfoo::ast
