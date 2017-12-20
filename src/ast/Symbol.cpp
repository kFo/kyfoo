#include <kyfoo/ast/Symbol.hpp>

#include <algorithm>
#include <tuple>

#include <kyfoo/Diagnostics.hpp>
#include <kyfoo/ast/Declarations.hpp>
#include <kyfoo/ast/Expressions.hpp>
#include <kyfoo/ast/Context.hpp>
#include <kyfoo/ast/Module.hpp>
#include <kyfoo/ast/Scopes.hpp>
#include <kyfoo/ast/Semantics.hpp>

namespace kyfoo {
    namespace ast {

    namespace {
        /**
         * Determines suitable bindings for any meta-variables in \a target
         * 
         * \param bindings Substitution table
         * \param target   Expression pattern with unbound meta-variables
         * \param query    Query expressions used as arguments
         * 
         * \precondition All expressions in \a query are resolved
         * 
         * \note No substitution is determined for any expression in \a target that is already resolved
         */
        bool findBindings(binding_set_t& bindings, PatternsDecl const& target, pattern_t const& query)
        {
            auto const& targetPattern = target.params->pattern();

            if ( auto proc = target.decl->as<ProcedureDeclaration>() ) {
                auto const paramCount = target.params->pattern().size();
                if ( paramCount != query.size() )
                    return false;

                for ( std::size_t i = 0; i < paramCount; ++i ) {
                    auto const ordinal = proc->ordinal(i);
                    if ( ordinal == -1 ) {
                        if ( !targetPattern[i]->declaration() )
                            if ( !matchStructural(bindings, *targetPattern[i], *query[i]) )
                                return false;
                    }

                    if ( proc->parameters()[ordinal]->dataType() )
                        continue;

                    auto const queryDecl = query[i]->declaration();
                    if ( !queryDecl )
                        return false;

                    if ( !matchStructural(bindings, *targetPattern[i], *query[i]) )
                        return false;
                }

                return true;
            }

            if ( !target.params->hasMetaVariables() )
                return true;

            return matchStructural(bindings, targetPattern, query);
        }
    } // namespace

//
// PatternsPrototype

PatternsPrototype::PatternsPrototype()
{
}

PatternsPrototype::PatternsPrototype(std::vector<std::unique_ptr<Expression>>&& pattern)
    : myPattern(std::move(pattern))
{
}

PatternsPrototype::PatternsPrototype(PatternsPrototype const&)
{
    // clone myPattern
    // clone myVariables
}

PatternsPrototype& PatternsPrototype::operator = (PatternsPrototype const& rhs)
{
    PatternsPrototype(rhs).swap(*this);
    return *this;
}

PatternsPrototype::PatternsPrototype(PatternsPrototype&& rhs)
    : myPattern(std::move(rhs.myPattern))
    , myVariables(std::move(rhs.myVariables))
{
}

PatternsPrototype& PatternsPrototype::operator = (PatternsPrototype&& rhs)
{
    this->~PatternsPrototype();
    new (this) PatternsPrototype(std::move(rhs));

    return *this;
}

PatternsPrototype::~PatternsPrototype() = default;

void PatternsPrototype::swap(PatternsPrototype& rhs)
{
    using std::swap;
    swap(myPattern, rhs.myPattern);
    swap(myVariables, rhs.myVariables);
}

void PatternsPrototype::io(IStream& stream) const
{
    stream.openArray("params");
    for ( auto const& p : myPattern )
        p->io(stream);
    stream.closeArray();
    stream.openArray("vars");
    for ( auto const& v : myVariables )
        v->io(stream);
    stream.closeArray();
}

IMPL_CLONE_NOBASE_BEGIN(PatternsPrototype, PatternsPrototype)
IMPL_CLONE_CHILD(myPattern)
IMPL_CLONE_CHILD(myVariables)
IMPL_CLONE_END
IMPL_CLONE_REMAP_NOBASE_BEGIN(PatternsPrototype)
IMPL_CLONE_REMAP(myPattern)
IMPL_CLONE_REMAP(myVariables)
IMPL_CLONE_REMAP_END

void PatternsPrototype::resolveVariables()
{
    if ( myVariables.empty() ) {
        for ( auto const& param : myPattern ) {
            auto fv = gatherMetaVariables(*param);
            for ( auto& p : fv ) {
                myVariables.push_back(std::make_unique<SymbolVariable>(*this, *p));
                p->setMetaVariable(myVariables.back().get());
            }
        }
    }
}

SymRes PatternsPrototype::resolveSymbols(Context& ctx)
{
    ScopeResolver resolver(ctx.resolver().scope());
    resolver.addSupplementaryPrototype(*this);
    auto save = ctx.changeResolver(resolver);
    auto ret = ctx.resolveExpressions(myPattern);
    ctx.changeResolver(*save);

    return ret;
}

void PatternsPrototype::resetPattern()
{
    for ( auto& e : myPattern )
        clearDeclarations(*e);
}

Slice<Expression*> PatternsPrototype::pattern() const
{
    return myPattern;
}

Slice<SymbolVariable*> PatternsPrototype::symbolVariables() const
{
    return myVariables;
}

void PatternsPrototype::bindVariables(binding_set_t const& bindings)
{
    if ( bindings.size() != myVariables.size() )
        throw std::runtime_error("template parameter binding mismatch");

    for ( std::size_t i = 0; i < bindings.size(); ++i ) {
        auto const& key = bindings.keys()[i];
        auto const& value = bindings.values()[i];
        auto var = findVariable(key->identifier().lexeme());
        if ( !var )
            throw std::runtime_error("template parameter binding mismatch");

        var->bindExpression(value);
    }
}

SymbolVariable* PatternsPrototype::findVariable(std::string const& identifier)
{
    for ( std::size_t i = 0; i < myVariables.size(); ++i )
        if ( myVariables[i]->identifier().lexeme() == identifier )
            return myVariables[i].get();

    return nullptr;
}

SymbolVariable const* PatternsPrototype::findVariable(std::string const& identifier) const
{
    return const_cast<PatternsPrototype*>(this)->findVariable(identifier);
}

SymbolVariable* PatternsPrototype::createVariable(PrimaryExpression const& primary)
{
    if ( auto symvar = findVariable(primary.token().lexeme()) )
        return symvar;

    myVariables.emplace_back(std::make_unique<SymbolVariable>(*this, primary));
    return myVariables.back().get();
}

bool PatternsPrototype::isConcrete() const
{
    for ( auto const& v : myVariables ) {
        auto expr = resolveIndirections(v->boundExpression());
        if ( !expr || !expr->declaration() )
            return false;

        if ( auto sv = expr->declaration()->as<SymbolVariable>() )
            if ( !sv->boundExpression() )
                return false;
    }

    return true;
}

bool PatternsPrototype::hasMetaVariables() const
{
    return !myVariables.empty();
}

//
// Symbol

Symbol::Symbol(lexer::Token const& identifier,
               PatternsPrototype&& params)
    : myIdentifier(identifier)
    , myPrototype(std::make_unique<PatternsPrototype>(std::move(params)))
{
}

Symbol::Symbol(lexer::Token const& identifier)
    : Symbol(identifier, PatternsPrototype())
{
}

Symbol::Symbol(std::unique_ptr<SymbolExpression> symExpr)
    : myIdentifier(symExpr->identifier())
    , myPrototype(std::make_unique<PatternsPrototype>(std::move(symExpr->internalExpressions())))
{
}

Symbol::Symbol(Symbol const& rhs)
    : myIdentifier(rhs.myIdentifier)
    , myPrototypeParent(&rhs)
{
    // clone myPrototype
}

Symbol& Symbol::operator = (Symbol const& rhs)
{
    Symbol(rhs).swap(*this);
    return *this;
}

Symbol::Symbol(Symbol&& rhs)
    : myIdentifier(std::move(rhs.myIdentifier))
    , myPrototype(std::move(rhs.myPrototype))
    , myPrototypeParent(rhs.myPrototypeParent)
{
    rhs.myPrototypeParent = nullptr;
}

Symbol& Symbol::operator = (Symbol&& rhs)
{
    this->~Symbol();
    new (this) Symbol(std::move(rhs));
    return *this;
}

Symbol::~Symbol() = default;

void Symbol::swap(Symbol& rhs)
{
    using std::swap;
    swap(myIdentifier, rhs.myIdentifier);
    swap(myPrototype, rhs.myPrototype);
    swap(myPrototypeParent, rhs.myPrototypeParent);
}

void Symbol::io(IStream& stream) const
{
    stream.next("symbol", myIdentifier);
    myPrototype->io(stream);
}

IMPL_CLONE_NOBASE_BEGIN(Symbol, Symbol)
IMPL_CLONE_CHILD(myPrototype)
IMPL_CLONE_END
IMPL_CLONE_REMAP_NOBASE_BEGIN(Symbol)
IMPL_CLONE_REMAP(myPrototype)
IMPL_CLONE_REMAP_END

SymRes Symbol::resolveSymbols(Context& ctx)
{
    myPrototype->resolveVariables();
    return myPrototype->resolveSymbols(ctx);
}

lexer::Token const& Symbol::identifier() const
{
    return myIdentifier;
}

lexer::Token& Symbol::identifier()
{
    return myIdentifier;
}

PatternsPrototype const& Symbol::prototype() const
{
    return *myPrototype;
}

PatternsPrototype& Symbol::prototype()
{
    return *myPrototype;
}

Symbol const* Symbol::prototypeParent() const
{
    return myPrototypeParent;
}

//
// SymbolReference

SymbolReference::SymbolReference(const char* name, pattern_t pattern)
    : myName(name)
    , myPattern(pattern)
{
}

SymbolReference::SymbolReference(std::string const& name, pattern_t pattern)
    : myName(name.c_str())
    , myPattern(pattern)
{
}

SymbolReference::SymbolReference(Symbol const& sym)
    : SymbolReference(sym.identifier().lexeme().c_str(), sym.prototype().pattern())
{
}

SymbolReference::SymbolReference(std::string const& name)
    : SymbolReference(name.c_str(), pattern_t())
{
}

SymbolReference::SymbolReference(const char* name)
    : SymbolReference(name, pattern_t())
{
}

SymbolReference::~SymbolReference() = default;

const char* SymbolReference::name() const
{
    return myName;
}

pattern_t const& SymbolReference::pattern() const
{
    return myPattern;
}

//
// CandidateSet

bool CandidateSet::empty() const
{
    return myCandidates.empty();
}

std::vector<Candidate>::const_iterator CandidateSet::begin() const
{
    return myCandidates.begin();
}

std::vector<Candidate>::const_iterator CandidateSet::end() const
{
    return myCandidates.end();
}

Candidate const& CandidateSet::operator[](std::size_t index) const
{
    return myCandidates[index];
}

Candidate& CandidateSet::operator[](std::size_t index)
{
    return myCandidates[index];
}

void CandidateSet::append(VarianceResult const& v, Prototype& proto, binding_set_t&& bindings)
{
    decltype(Candidate::rank) r;
    if ( v.exact() )
        r = bindings.empty() ? Candidate::Exact : Candidate::Parametric;
    else if ( v.covariant() )
        r = Candidate::Covariant;
    else
        throw std::runtime_error("invalid candidate match");

    auto lt = [](Candidate const& lhs, Candidate const& rhs) {
        auto const& lid = lhs.proto->proto.decl->symbol().identifier();
        auto const& rid = rhs.proto->proto.decl->symbol().identifier();
        return std::make_tuple(lhs.rank, lid.line(), lid.column())
             < std::make_tuple(rhs.rank, rid.line(), rid.column());
    };
    Candidate c{ r, &proto, std::move(bindings) };
    myCandidates.emplace(upper_bound(begin(), end(), c, lt), std::move(c));
}

//
// SymbolSpace

SymbolSpace::SymbolSpace(DeclarationScope* scope, std::string const& name)
    : myScope(scope)
    , myName(name)
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

void SymbolSpace::swap(SymbolSpace& rhs)
{
    using std::swap;
    swap(myScope, rhs.myScope);
    swap(myName, rhs.myName);
    swap(myPrototypes, rhs.myPrototypes);
}

std::string const& SymbolSpace::name() const
{
    return myName;
}

Slice<Prototype> SymbolSpace::prototypes() const
{
    return myPrototypes;
}

void SymbolSpace::append(PatternsPrototype const& prototype,
                         Declaration& declaration)
{
    myPrototypes.push_back(Prototype{ {&prototype, &declaration}, std::vector<PatternsDecl>() });
}

Declaration const* SymbolSpace::findEquivalent(pattern_t const& paramlist) const
{
    for ( auto const& e : myPrototypes )
        if ( matchEquivalent(e.proto.params->pattern(), paramlist) )
            return e.proto.decl;

    return nullptr;
}

Declaration* SymbolSpace::findEquivalent(pattern_t const& paramlist)
{
    return const_cast<Declaration*>(const_cast<SymbolSpace const*>(this)->findEquivalent(paramlist));
}

CandidateSet SymbolSpace::findCandidates(Module& endModule, Diagnostics& dgn, pattern_t const& paramlist)
{
    CandidateSet ret;
    ScopeResolver resolver(*myScope);
    Context ctx(endModule, dgn, resolver);

    Diagnostics sfinaeDgn;
    Context sfinaeCtx(endModule, sfinaeDgn, resolver, Context::DisableCacheTemplateInstantiations);

    for ( auto& e : myPrototypes ) {
        binding_set_t bindings;
        if ( !findBindings(bindings, e.proto, paramlist) )
            continue;

        if ( bindings.empty() ) {
            if ( auto v = variance(ctx, bindings, e.proto.params->pattern(), paramlist) )
                ret.append(v, e, std::move(bindings));
        }
        else {
            auto d = ast::clone(e.proto.decl);
            d->symbol().prototype().bindVariables(bindings);
            auto result = sfinaeCtx.resolveDeclaration(*d);
            if ( !result )
                continue;

            if ( auto v = variance(sfinaeCtx, bindings, d->symbol().prototype().pattern(), paramlist) )
                ret.append(v, e, std::move(bindings));
        }
    }

    return ret;
}

SymbolSpace::DeclInstance
SymbolSpace::findOverload(Module& endModule,
                          Diagnostics& dgn,
                          pattern_t const& paramlist)
{
    ScopeResolver resolver(*myScope);
    Context ctx(endModule, dgn, resolver);
    
    auto cset = findCandidates(endModule, dgn, paramlist);
    if ( cset.empty() )
        return { nullptr, nullptr };

    auto& c = cset[0];
    if ( c.proto->proto.params->isConcrete() )
        return { c.proto->proto.decl, nullptr };

    for ( auto param : paramlist )
        if ( hasIndirection(resolveIndirections(param)->declaration()->kind()) )
            return { c.proto->proto.decl, nullptr };

    return instantiate(ctx, *c.proto, std::move(c.bindings));
}

SymbolSpace::DeclInstance
SymbolSpace::instantiate(Context& ctx,
                         Prototype& proto,
                         binding_set_t&& bindingSet)
{
    for ( auto const& e : bindingSet.values() ) {
        auto decl = resolveIndirections(e)->declaration();
        if ( hasIndirection(decl->kind()) )
            throw std::runtime_error("cannot instantiate template with unbound symbol variable");
    }

    // use existing instantiation if it exists
    for ( std::size_t i = 0; i < proto.instances.size(); ++i ) {
        auto const& inst = proto.instances[i];
        auto const& instVars = inst.params->symbolVariables();
        if ( instVars.size() != bindingSet.size() )
            throw std::runtime_error("invalid template instance");

        auto l = begin(instVars);
        auto r = begin(bindingSet.values());
        while ( l != end(instVars) ) {
            auto lhs = (*l)->boundExpression();
            if ( !lhs )
                break;

            if ( !matchEquivalent(*lhs, **r) )
                break;

            ++l;
            ++r;
        }

        if ( l == end(instVars) )
            return { proto.proto.decl, inst.decl };
    }

    // create new instantiation
    clone_map_t cloneMap;
    auto instance = ast::clone(proto.proto.decl, cloneMap);
    auto instProto = reinterpret_cast<PatternsPrototype*>(cloneMap[proto.proto.params]);
    instProto->bindVariables(bindingSet);

    // todo: remove notion of "unresolve"
    if ( auto proc = instance->as<ProcedureDeclaration>() ) {
        proc->unresolvePrototypeSymbols();
        ctx.resolveDeclaration(*proc);
    }
    else {
        instProto->resetPattern();
        instProto->resolveSymbols(ctx);
    }

    ctx.resolveDeclaration(*instance);

    proto.instances.emplace_back(PatternsDecl{instProto, instance.get()});
    myScope->append(std::move(instance));
    return { proto.proto.decl, proto.instances.back().decl };
}

std::ostream& print(std::ostream& stream, Symbol const& sym)
{
    stream << sym.identifier().lexeme();
    if ( !sym.prototype().pattern().empty() ) {
        stream << "<";
        auto first = begin(sym.prototype().pattern());
        auto last = end(sym.prototype().pattern());
        if ( first != last )
            print(stream, **first);

        for ( ++first; first != last; ++first ) {
            stream << ", ";
            print(stream, **first);
        }

        stream << ">";
    }

    return stream;
}

    } // namespace ast
} // namespace kyfoo
