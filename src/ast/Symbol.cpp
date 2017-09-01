#include <kyfoo/ast/Symbol.hpp>

#include <kyfoo/Diagnostics.hpp>
#include <kyfoo/ast/Declarations.hpp>
#include <kyfoo/ast/Expressions.hpp>
#include <kyfoo/ast/Context.hpp>
#include <kyfoo/ast/Scopes.hpp>
#include <kyfoo/ast/Semantics.hpp>

namespace kyfoo {
    namespace ast {

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

void PatternsPrototype::resolveSymbols(Diagnostics& dgn, IResolver& resolver)
{
    Context ctx(dgn, resolver);

    if ( myVariables.empty() ) {
        for ( auto const& param : myPattern ) {
            auto fv = gatherFreeVariables(*param);
            for ( auto& p : fv ) {
                myVariables.push_back(std::make_unique<SymbolVariable>(*this, *p));
                p->setFreeVariable(myVariables.back().get());
            }
        }
    }

    ctx.resolveExpressions(myPattern);
}

Slice<Expression*> PatternsPrototype::pattern() const
{
    return myPattern;
}

Slice<SymbolVariable*> PatternsPrototype::symbolVariables() const
{
    return myVariables;
}

void PatternsPrototype::bindVariables(Context& ctx,
                                      binding_set_t const& bindings)
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

    ctx.resolveExpressions(myPattern);
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

bool PatternsPrototype::hasFreeVariables() const
{
    for ( auto const& e : myVariables )
        if ( !e->boundExpression() )
            return true;

    return false;
}

//
// SymbolPrototype

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

void Symbol::resolveSymbols(Diagnostics& dgn, IResolver& resolver)
{
    myPrototype->resolveSymbols(dgn, resolver);
}

lexer::Token const& Symbol::identifier() const
{
    return myIdentifier;
}

PatternsPrototype const& Symbol::prototype() const
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

SymbolReference::pattern_t const& SymbolReference::pattern() const
{
    return myPattern;
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

Slice<SymbolSpace::Prototype> SymbolSpace::prototypes() const
{
    return myPrototypes;
}

void SymbolSpace::append(Context& ctx,
                         PatternsPrototype const& prototype,
                         Declaration& declaration)
{
    auto i = begin(myPrototypes);
    for ( ; i != end(myPrototypes); ++i ) {
        if ( prototype.pattern().size() < i->proto.params->pattern().size() )
            break;

        auto v = variance(ctx, i->proto.params->pattern(), prototype.pattern());
        if ( v.equivalent() ) {
            auto& err = ctx.error(declaration) << "matches existing declaration";
            err.see(*i->proto.decl);
            return;
        }
        else if ( v.covariant() ) {
            i->instances.emplace_back(PatternsDecl{&prototype, &declaration});
            return;
        }
        else if ( v.contravariant() ) {
            auto proto = i->proto;
            i->proto = {&prototype, &declaration};
            i->instances.insert(begin(i->instances), proto);
            return;
        }
    }

    myPrototypes.insert(i, Prototype{ {&prototype, &declaration}, std::vector<PatternsDecl>() });
}

Declaration const* SymbolSpace::findEquivalent(Diagnostics& dgn,
                                               pattern_t const& paramlist) const
{
    ScopeResolver resolver(*myScope);
    Context ctx(dgn, resolver);
    for ( auto const& e : myPrototypes ) {
        if ( matchEquivalent(ctx, e.proto.params->pattern(), paramlist) )
            return e.proto.decl;
    }

    return nullptr;
}

Declaration* SymbolSpace::findEquivalent(Diagnostics& dgn,
                                         pattern_t const& paramlist)
{
    return const_cast<Declaration*>(const_cast<SymbolSpace const*>(this)->findEquivalent(dgn, paramlist));
}

SymbolSpace::DeclInstance
SymbolSpace::findCovariant(Diagnostics& dgn,
                           pattern_t const& paramlist)
{
    ScopeResolver resolver(*myScope);
    Context ctx(dgn, resolver);
    for ( auto& e : myPrototypes ) {
        binding_set_t bindings;
        if ( variance(ctx, bindings, e.proto.params->pattern(), paramlist) ) {
            if ( e.proto.params->isConcrete() )
                return { e.proto.decl, nullptr };

            for ( auto p : paramlist )
                if ( hasIndirection(resolveIndirections(p)->declaration()->kind()) )
                    return { e.proto.decl, nullptr };

            return instantiate(ctx, e, std::move(bindings));
        }
    }

    return { nullptr, nullptr };
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

            if ( !matchEquivalent(ctx, *lhs, **r) )
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
    instProto->bindVariables(ctx, bindingSet);

    if ( auto proc = instance->as<ProcedureDeclaration>() )
        proc->resolvePrototypeSymbols(ctx.diagnostics());

    instance->resolveSymbols(ctx.diagnostics());

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
