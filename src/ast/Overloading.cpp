#include <kyfoo/ast/Overloading.hpp>

#include <kyfoo/ast/Axioms.hpp>
#include <kyfoo/ast/Context.hpp>
#include <kyfoo/ast/Declarations.hpp>
#include <kyfoo/ast/Expressions.hpp>
#include <kyfoo/ast/Module.hpp>
#include <kyfoo/ast/Scopes.hpp>
#include <kyfoo/ast/Semantics.hpp>

namespace kyfoo::ast {

//
// Viability

Viability::Viability(Variance v, ProcedureDeclaration const* conversion)
    : myVariance(v)
    , myConversion(conversion)
{
}

Variance Viability::variance() const
{
    return myVariance;
}

ProcedureDeclaration const* Viability::conversion() const
{
    return myConversion;
}

Viability::operator bool() const
{
    return conversion() || myVariance;
}

//
// OverloadViability

void OverloadViability::append(Variance v, ProcedureDeclaration const* conversion)
{
    myViabilities.append(v, conversion);
}

void OverloadViability::append(Viability pv)
{
    myViabilities.append(std::move(pv));
}

ab<Viability>::ConstIterator OverloadViability::begin() const
{
    return myViabilities.begin();
}

ab<Viability>::Iterator OverloadViability::begin()
{
    return myViabilities.begin();
}

ab<Viability>::ConstIterator OverloadViability::end() const
{
    return myViabilities.end();
}

ab<Viability>::Iterator OverloadViability::end()
{
    return myViabilities.end();
}

bool OverloadViability::empty() const
{
    return !myViabilities;
}

uz OverloadViability::card() const
{
    return myViabilities.card();
}

Viability const& OverloadViability::operator [] (uz index) const
{
    return myViabilities[index];
}

Viability& OverloadViability::operator [] (uz index)
{
    return myViabilities[index];
}

Variance OverloadViability::variance() const
{
    Variance ret = Variance::Exact;
    for ( auto& p : myViabilities ) {
        if ( p.variance().invariant() )
            return Variance::Invariant;
        else if ( p.variance().exact() )
            continue;

        ret = Variance::Covariant;
    }

    return ret;
}

OverloadViability::operator bool() const
{
    return std::all_of(myViabilities.begin(), myViabilities.end(), $);
}

//
// Via

Via::Via(Rank rank, OverloadViability&& viability, Prototype& proto, Substitutions&& substs)
    : myRank(rank)
    , myViability(std::move(viability))
    , myProto(&proto)
    , mySubsts(std::move(substs))
{
}

Declaration* Via::instantiate(Context& ctx)
{
    if ( mySubsts.empty() )
        return myProto->proto.decl;

    for ( uz i = 0; i < mySubsts.card(); ++i ) {
        ENFORCE(!needsSubstitution(mySubsts.expr(i)),
                "cannot instantiate template without substitution for symbol variable");
    }

    // use existing instantiation if it exists
    for ( uz i = 0; i < myProto->instances.card(); ++i ) {
        auto const& inst = myProto->instances[i];
        auto const& instVars = inst.params->symbolVariables();
        ENFORCE(instVars.card() == mySubsts.card(), "invalid template instance");

        auto l = begin(instVars);
        uz r = 0;
        while ( l != end(instVars) ) {
            auto lhs = (*l)->boundExpression();
            if ( !lhs )
                break;

            if ( !matchEquivalent(*lhs, mySubsts.expr(r)) )
                break;

            ++l;
            ++r;
        }

        if ( l == end(instVars) )
            return inst.decl;
    }

    // create new instantiation
    CloneMap cloneMap;
    myProto->ownDeclarations.append(ast::beginClone(myProto->proto.decl, cloneMap));
    auto instanceDecl = myProto->ownDeclarations.back().get();
    remap(*instanceDecl, cloneMap);

    instanceDecl->symbol().prototype().bindVariables(mySubsts);
    auto res = ctx.resolveDeclaration(*instanceDecl);
    ENFORCE(!res.error(), "invalid substitution");

    myProto->instances.append(PatternsDecl{&instanceDecl->symbol().prototype(), instanceDecl});

    if ( !res )
        return instanceDecl;

    if ( auto defn = getDefinition(*myProto->proto.decl) ) {
        myProto->ownDefinitions.append(ast::beginClone(defn, cloneMap));
        auto instanceDefn = myProto->ownDefinitions.back().get();
        remap(*instanceDefn, cloneMap);
        define(*instanceDecl, *instanceDefn);

        ENFORCE(instanceDecl == instanceDefn->declaration(), "decl/defn mismatch");

        ctx.resolveScopeDeclarations(*instanceDefn);
        ctx.appendInstantiatedDefinition(*instanceDefn);
    }

    return instanceDecl;
}

Via::Rank Via::rank() const
{
    return myRank;
}

OverloadViability const& Via::viability() const
{
    return myViability;
}

OverloadViability& Via::viability()
{
    return myViability;
}

Prototype const& Via::prototype() const
{
    return *myProto;
}

Prototype& Via::prototype()
{
    return *myProto;
}

bool Via::operator < (Via const& rhs) const
{
    return myRank < rhs.myRank;
}

//
// ViableSet

void ViableSet::swap(ViableSet& rhs) noexcept
{
    using kyfoo::swap;
    swap(myVias, rhs.myVias);
    swap(myDeclaration, rhs.myDeclaration);
}

bool ViableSet::empty() const
{
    return !myVias;
}

uz ViableSet::card() const
{
    return myVias.card();
}

ab<Via>::ConstIterator ViableSet::begin() const
{
    return myVias.begin();
}

ab<Via>::Iterator ViableSet::begin()
{
    return myVias.begin();
}

ab<Via>::ConstIterator ViableSet::end() const
{
    return myVias.end();
}

ab<Via>::Iterator ViableSet::end()
{
    return myVias.end();
}

Via const& ViableSet::operator [] (uz index) const
{
    return myVias[index];
}

Via& ViableSet::operator [] (uz index)
{
    return myVias[index];
}

Declaration const* ViableSet::single() const
{
    return myDeclaration;
}

Declaration* ViableSet::single()
{
    return myDeclaration;
}

Via& ViableSet::best()
{
    return myVias.front();
}

Via const& ViableSet::best() const
{
    return myVias.front();
}

ViableSet::Result ViableSet::result() const
{
    if ( empty() )
        return None;

    if ( myVias.card() > 1 && myVias[0].rank() == myVias[1].rank() )
        return Ambiguous;

    if ( myVias[0].rank() == Via::Conversion )
        return NeedsConversion;

    return Single;
}

void ViableSet::append(OverloadViability viability, Prototype& proto, Substitutions substs)
{
    auto r = rank(viability.variance(), !substs.empty());
    Via c(r, std::move(viability), proto, std::move(substs));
    myVias.insert(std::upper_bound(begin(), end(), c), std::move(c));
}

void ViableSet::merge(ViableSet rhs)
{
    for ( auto& e : rhs.myVias )
        myVias.insert(std::upper_bound(begin(), end(), e), std::move(e));

    if ( !myDeclaration )
        myDeclaration = rhs.myDeclaration;
    else if ( rhs.myDeclaration )
        myDeclaration = nullptr;

    rhs.clear();
}

void ViableSet::condense(Context& ctx)
{
    if ( result() == Single )
        myDeclaration = best().instantiate(ctx);
}

void ViableSet::clear()
{
    myVias.clear();
    myDeclaration = nullptr;
}

//
// SymbolSpace

SymbolSpace::SymbolSpace(Scope* scope, std::string name)
    : myScope(scope)
    , myName(std::move(name))
{
}

SymbolSpace::SymbolSpace(SymbolSpace&& rhs)
    : myScope(rhs.myScope)
    , myName(std::move(rhs.myName))
    , myPrototypes(std::move(rhs.myPrototypes))
{
    rhs.myScope = nullptr;
}

SymbolSpace& SymbolSpace::operator = (SymbolSpace&& rhs)
{
    this->~SymbolSpace();
    new (this) SymbolSpace(std::move(rhs));

    return *this;
}

SymbolSpace::~SymbolSpace() = default;

void SymbolSpace::swap(SymbolSpace& rhs) noexcept
{
    using kyfoo::swap;
    swap(myScope, rhs.myScope);
    swap(myName, rhs.myName);
    swap(myPrototypes, rhs.myPrototypes);
}

stringv SymbolSpace::name() const
{
    return myName;
}

Slice<Prototype const> SymbolSpace::prototypes() const
{
    return myPrototypes;
}

void SymbolSpace::append(PatternsPrototype const& prototype,
                         Declaration& declaration)
{
    myPrototypes.append(prototype, declaration);
}

Declaration const* SymbolSpace::findEquivalent(Slice<Expression const*> paramlist) const
{
    for ( auto const& e : myPrototypes )
        if ( matchEquivalent(e.proto.params->pattern(), paramlist) )
            return e.proto.decl;

    return nullptr;
}

Declaration* SymbolSpace::findEquivalent(Slice<Expression const*> paramlist)
{
    return const_cast<Declaration*>(const_cast<SymbolSpace const*>(this)->findEquivalent(paramlist));
}

ViableSet SymbolSpace::findViableOverloads(Context& ctx, Slice<Expression const*> paramlist)
{
    ViableSet ret;

    auto& callerResolver = ctx.resolver();
    Resolver resolver(*myScope, ctx.resolver().options());
    REVERT = ctx.pushResolver(resolver);

    Diagnostics sfinaeDgn;
    Context sfinaeCtx(ctx.module(), sfinaeDgn, resolver, Context::DisableCacheTemplateInstantiations);

    for ( auto& e : myPrototypes ) {
        if ( e.proto.decl->symbol().prototype().pattern().card() != paramlist.card() )
            continue;

        Substitutions substs(*e.proto.decl, paramlist);
        if ( !substs )
            continue;

        auto targetProto = e.proto.params;
        auto relativeCtx = &ctx;
        if ( !substs.empty() ) {
            auto substDecl = ctx.module().fabricate(ast::clone(e.proto.decl));
            substDecl->symbol().prototype().bindVariables(substs);
            auto result = sfinaeCtx.resolveDeclaration(*substDecl);
            if ( !result )
                continue;

            targetProto = &substDecl->symbol().prototype();
            relativeCtx = &sfinaeCtx;
        }

        if ( auto v = implicitViability(*relativeCtx, callerResolver, targetProto->pattern(), paramlist) )
            ret.append(std::move(v), e, std::move(substs));
    }

    ret.condense(ctx);

    return ret;
}

//
// Lookup

Lookup::Lookup(SymbolReference query)
    : myQuery(query)
{
}

Lookup::~Lookup() = default;

void Lookup::swap(Lookup& rhs)
{
    using kyfoo::swap;
    swap(myQuery, rhs.myQuery);
    swap(mySpaces, rhs.mySpaces);
    swap(mySet, rhs.mySet);
    swap(myDecl, rhs.myDecl);
}

Lookup::operator bool () const
{
    return myDecl || !mySet.empty();
}

void Lookup::appendTrace(SymbolSpace const& space)
{
    mySpaces.append(&space);
}

Lookup& Lookup::resolveTo(ViableSet set)
{
    mySet = std::move(set);

    if ( mySet.single() )
        return resolveTo(*mySet.single());

    return *this;
}

Lookup& Lookup::resolveTo(Declaration& decl)
{
    ENFORCE(!myDecl, "declaration reference stomped");

    myDecl = &decl;
    return *this;
}

Lookup& Lookup::append(Lookup rhs)
{
    mySpaces.appendRange(rhs.mySpaces());
    myDecl = rhs.myDecl;
    mySet.merge(std::move(rhs.mySet));

    rhs.mySpaces.clear();
    rhs.myDecl = nullptr;

    return *this;
}

SymbolReference Lookup::query() const
{
    return myQuery;
}

SymbolSpace const* Lookup::symSpace() const
{
    if ( mySpaces )
        return mySpaces.front();

    return nullptr;
}

Slice<SymbolSpace const* const> Lookup::trace() const
{
    return mySpaces();
}

ViableSet const& Lookup::viable() const
{
    return mySet;
}

ViableSet& Lookup::viable()
{
    return mySet;
}

Declaration const* Lookup::single() const
{
    return const_cast<Lookup*>(this)->single();
}

Declaration* Lookup::single()
{
    if ( myDecl )
        return myDecl;

    return mySet.single();
}

//
// misc

Via::Rank rank(Variance v, bool hasSubsts)
{
    if ( v.exact() )
        return hasSubsts ? Via::Parametric : Via::Exact;

    if ( v.covariant() )
        return Via::Covariant;

    return Via::Conversion;
}

ProcedureDeclaration const*
findImplicitConversion(Context& ctx, Expression const& dest, Expression const& src)
{
    auto d = resolveIndirections(dest);
    if ( !d )
        return nullptr;

    auto s = resolveIndirections(src);
    if ( !s )
        return nullptr;

    if ( auto dstDecl = getDeclaration(*d) ) {
        if ( auto dstBinder = getBinder(*dstDecl) ) {
            d = resolveIndirections(dstBinder->type());
            if ( !d )
                return nullptr;
        }
    }

    Lookup ret(SymbolReference("", slice(s)));
    auto hit = ctx.matchOverload(SymbolReference("implicitTo", slice(d)));
    for ( auto& via : hit.viable() ) {
        if ( via.rank() == Via::Conversion )
            continue;

        auto templ = via.instantiate(ctx)->as<TemplateDeclaration>();
        if ( !templ )
            continue;

        auto templDefn = templ->definition();
        if ( !templDefn )
            continue;

        auto const opts = Resolver::Narrow | Resolver::NoImplicitConversions;
        ret.append(ctx.matchOverload(*templDefn, opts, SymbolReference("", slice(s))));
    }

    ret.viable().condense(ctx);
    if ( auto single = ret.viable().single() )
        return single->as<ProcedureDeclaration>();

    return nullptr;
}

Viability implicitViability(Context& ctx,
                            Expression const& dest,
                            Expression const& src)
{
    return implicitViability(ctx, ctx.resolver(), dest, src);
}

Viability implicitViability(Context& ctx,
                            Resolver& implicitResolver,
                            Expression const& dest,
                            Expression const& src)
{
    auto v = variance(ctx, dest, src);
    if ( !v && !(ctx.resolver().options() & Resolver::NoImplicitConversions) ) {
        REVERT = ctx.pushResolver(implicitResolver);
        if ( auto proc = findImplicitConversion(ctx, dest, src) )
            return Viability(v, proc);
    }

    return Viability(v, nullptr);
}

OverloadViability implicitViability(Context& ctx,
                                    Resolver& implicitResolver,
                                    Slice<Expression const*> dest,
                                    Slice<Expression const*> src)
{
    OverloadViability ret;
    auto const card = dest.card();
    ENFORCE(card == src.card(), "overload arity mismatch");

    for ( uz i = 0; i < card; ++i )
        ret.append(implicitViability(ctx, implicitResolver, *dest[i], *src[i]));

    return ret;
}

} // namespace kyfoo::ast
