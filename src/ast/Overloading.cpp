#include <kyfoo/ast/Overloading.hpp>

#include <kyfoo/ast/Context.hpp>
#include <kyfoo/ast/Declarations.hpp>
#include <kyfoo/ast/Expressions.hpp>
#include <kyfoo/ast/Module.hpp>
#include <kyfoo/ast/Scopes.hpp>
#include <kyfoo/ast/Semantics.hpp>

namespace kyfoo::ast {

//
// ParameterViability

ParameterViability::ParameterViability(Variance v, ProcedureDeclaration const* conversion)
    : myVariance(v)
    , myConversion(conversion)
{
}

Variance ParameterViability::variance() const
{
    return myVariance;
}

ProcedureDeclaration const* ParameterViability::conversion() const
{
    return myConversion;
}

ParameterViability::operator bool() const
{
    return conversion() || myVariance;
}

//
// OverloadViability

void OverloadViability::append(Variance v, ProcedureDeclaration const* conversion)
{
    myViabilities.emplace_back(v, conversion);
}

std::vector<ParameterViability>::const_iterator OverloadViability::begin() const
{
    return myViabilities.begin();
}

std::vector<ParameterViability>::iterator OverloadViability::begin()
{
    return myViabilities.begin();
}

std::vector<ParameterViability>::const_iterator OverloadViability::end() const
{
    return myViabilities.end();
}

std::vector<ParameterViability>::iterator OverloadViability::end()
{
    return myViabilities.end();
}

bool OverloadViability::empty() const
{
    return myViabilities.empty();
}

uz OverloadViability::size() const
{
    return myViabilities.empty();
}

ParameterViability const& OverloadViability::operator [] (uz index) const
{
    return myViabilities[index];
}

ParameterViability& OverloadViability::operator [] (uz index)
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
    return all_of(myViabilities.begin(), myViabilities.end(), $);
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

    for ( uz i = 0; i < mySubsts.size(); ++i ) {
        if ( needsSubstitution(mySubsts.expr(i)) )
            throw std::runtime_error("cannot instantiate template with unbound symbol variable");
    }

    // use existing instantiation if it exists
    for ( uz i = 0; i < myProto->instances.size(); ++i ) {
        auto const& inst = myProto->instances[i];
        auto const& instVars = inst.params->symbolVariables();
        if ( instVars.size() != mySubsts.size() )
            throw std::runtime_error("invalid template instance");

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
    clone_map_t cloneMap;
    myProto->ownDeclarations.emplace_back(ast::clone(myProto->proto.decl, cloneMap));
    auto instanceDecl = myProto->ownDeclarations.back().get();
    instanceDecl->symbol().prototype().bindVariables(mySubsts);
    ctx.resolveDeclaration(*instanceDecl);

    if ( auto defn = getDefinition(*myProto->proto.decl) ) {
        if ( instanceDecl->symbol().prototype().isConcrete() ) {
            myProto->ownDefinitions.emplace_back(ast::clone(defn, cloneMap));
            auto instanceDefn = myProto->ownDefinitions.back().get();
            define(*instanceDecl, *instanceDefn);

            if ( instanceDecl != instanceDefn->declaration() )
                throw std::runtime_error("decl/defn mismatch");

            if ( instanceDefn->resolveSymbols(ctx.module(), ctx.diagnostics()).error() )
                return nullptr;
        }
    }

    myProto->instances.emplace_back(PatternsDecl{&instanceDecl->symbol().prototype(), instanceDecl});
    return myProto->instances.back().decl;
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
    return myVias.empty();
}

uz ViableSet::size() const
{
    return myVias.size();
}

std::vector<Via>::const_iterator ViableSet::begin() const
{
    return myVias.begin();
}

std::vector<Via>::const_iterator ViableSet::end() const
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

    if ( myVias.size() > 1 && myVias[0].rank() == myVias[1].rank() )
        return Ambiguous;

    if ( myVias[0].rank() == Via::Conversion )
        return NeedsConversion;

    return Single;
}

void ViableSet::append(OverloadViability&& viability, Prototype& proto, Substitutions&& substs)
{
    Via::Rank rank;
    auto variance = viability.variance();
    if ( variance.exact() )
        rank = substs.empty() ? Via::Exact : Via::Parametric;
    else if ( variance.covariant() )
        rank = Via::Covariant;
    else
        rank = Via::Conversion;

    Via c(rank, std::move(viability), proto, std::move(substs));
    myVias.emplace(upper_bound(begin(), end(), c), std::move(c));
}

void ViableSet::merge(ViableSet&& rhs)
{
    for ( auto& e : rhs.myVias )
        myVias.emplace(upper_bound(begin(), end(), e), std::move(e));

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

SymbolSpace::SymbolSpace(DeclarationScope* scope, std::string name)
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

std::string_view SymbolSpace::name() const
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
    myPrototypes.push_back(Prototype{ {&prototype, &declaration}, std::vector<PatternsDecl>() });
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

ViableSet SymbolSpace::findViableOverloads(Module& endModule, Diagnostics& dgn, Slice<Expression const*> paramlist)
{
    ViableSet ret;

    Resolver resolver(*myScope);
    Context primaryCtx(endModule, dgn, resolver);

    Diagnostics sfinaeDgn;
    Context sfinaeCtx(endModule, sfinaeDgn, resolver, Context::DisableCacheTemplateInstantiations);

    for ( auto& e : myPrototypes ) {
        if ( e.proto.decl->symbol().prototype().pattern().size() != paramlist.size() )
            continue;

        Substitutions substs(*e.proto.decl, paramlist);
        if ( !substs )
            continue;

        auto targetProto = e.proto.params;
        auto ctx = &primaryCtx;
        std::unique_ptr<Declaration> substDecl;
        if ( !substs.empty() ) {
            substDecl = ast::clone(e.proto.decl);
            substDecl->symbol().prototype().bindVariables(substs);
            auto result = sfinaeCtx.resolveDeclaration(*substDecl);
            if ( !result )
                continue;

            targetProto = &substDecl->symbol().prototype();
            ctx = &sfinaeCtx;
        }

        if ( auto v = implicitViability(*ctx, targetProto->pattern(), paramlist) )
            ret.append(std::move(v), e, std::move(substs));
    }

    ret.condense(primaryCtx);

    return ret;
}

//
// Lookup

Lookup::Lookup(SymbolReference query)
    : myQuery(query)
{
}

Lookup::Lookup(Lookup&& rhs)
    : myQuery(std::move(rhs.myQuery))
    , mySpaces(std::move(rhs.mySpaces))
    , mySet(std::move(rhs.mySet))
    , myDecl(rhs.myDecl)
{
    rhs.myDecl = nullptr;
}

Lookup& Lookup::operator = (Lookup&& rhs)
{
    Lookup(std::move(rhs)).swap(*this);
    return *this;
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
    mySpaces.push_back(&space);
}

Lookup& Lookup::resolveTo(ViableSet&& set)
{
    mySet = std::move(set);

    if ( mySet.single() )
        return resolveTo(*mySet.single());

    return *this;
}

Lookup& Lookup::resolveTo(Declaration& decl)
{
    if ( myDecl )
        throw std::runtime_error("declaration reference stomped");

    myDecl = &decl;
    return *this;
}

Lookup& Lookup::append(Lookup&& rhs)
{
    mySpaces.insert(end(mySpaces),
                    begin(rhs.mySpaces), end(rhs.mySpaces));
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
    if ( !mySpaces.empty() )
        return mySpaces.front();

    return nullptr;
}

Slice<SymbolSpace const* const> Lookup::trace() const
{
    return mySpaces;
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

ProcedureDeclaration const*
findImplicitConversion(Context& ctx, Expression const& dest, Expression const& src)
{
    auto d = resolveIndirections(dest);
    if ( !d )
        return nullptr;

    auto s = resolveIndirections(src);
    if ( !s )
        return nullptr;

    if ( auto dstDecl = getDeclaration(dest) ) {
        if ( auto dstBinder = getBinder(*dstDecl) ) {
            d = resolveIndirections(dstBinder->type());
            if ( !d )
                return nullptr;
        }
    }

    auto hit = ctx.matchOverload(SymbolReference("implicitTo", slice(d)));
    auto templ = hit.singleAs<TemplateDeclaration>();
    if ( !templ )
        return nullptr;

    auto templDefn = templ->definition();
    if ( !templDefn )
        return nullptr;

    Resolver templResolver(*templDefn, Resolver::Narrow);
    Context templCtx(ctx.module(), ctx.diagnostics(), templResolver);
    hit = templCtx.matchOverload(SymbolReference("", slice(s)));
    return hit.singleAs<ProcedureDeclaration>();
}

OverloadViability implicitViability(Context& ctx, Slice<Expression const*> dest, Slice<Expression const*> src)
{
    OverloadViability ret;
    auto const size = dest.size();
    if ( size != src.size() )
        throw std::runtime_error("overload arity mismatch");

    for ( uz i = 0; i < size; ++i ) {
        auto v = variance(ctx, *dest[i], *src[i]);
        if ( !v ) {
            if ( auto proc = findImplicitConversion(ctx, *dest[i], *src[i]) ) {
                ret.append(v, proc);
                continue;
            }
        }

        ret.append(v, nullptr);
    }

    return ret;
}

} // namespace kyfoo::ast
