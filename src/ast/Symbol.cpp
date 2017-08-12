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
// ParametersPrototype

ParametersPrototype::ParametersPrototype()
{
}

ParametersPrototype::ParametersPrototype(std::vector<std::unique_ptr<Expression>>&& parameters)
    : myParameters(std::move(parameters))
{
}

ParametersPrototype::ParametersPrototype(ParametersPrototype const&)
{
    // clone myParameters
    // clone myVariables
}

ParametersPrototype& ParametersPrototype::operator = (ParametersPrototype const& rhs)
{
    ParametersPrototype(rhs).swap(*this);
    return *this;
}

ParametersPrototype::ParametersPrototype(ParametersPrototype&& rhs)
    : myParameters(std::move(rhs.myParameters))
    , myVariables(std::move(rhs.myVariables))
{
}

ParametersPrototype& ParametersPrototype::operator = (ParametersPrototype&& rhs)
{
    this->~ParametersPrototype();
    new (this) ParametersPrototype(std::move(rhs));

    return *this;
}

ParametersPrototype::~ParametersPrototype() = default;

void ParametersPrototype::swap(ParametersPrototype& rhs)
{
    using std::swap;
    swap(myParameters, rhs.myParameters);
    swap(myVariables, rhs.myVariables);
}

void ParametersPrototype::io(IStream& stream) const
{
    stream.openArray("params");
    for ( auto const& p : myParameters )
        p->io(stream);
    stream.closeArray();
    stream.openArray("vars");
    for ( auto const& v : myVariables )
        v->io(stream);
    stream.closeArray();
}

IMPL_CLONE_NOBASE_BEGIN(ParametersPrototype, ParametersPrototype)
IMPL_CLONE_CHILD(myParameters)
IMPL_CLONE_CHILD(myVariables)
IMPL_CLONE_END
IMPL_CLONE_REMAP_NOBASE_BEGIN(ParametersPrototype)
IMPL_CLONE_REMAP(myParameters)
IMPL_CLONE_REMAP(myVariables)
IMPL_CLONE_REMAP_END

void ParametersPrototype::resolveSymbols(Diagnostics& dgn, IResolver& resolver)
{
    Context ctx(dgn, resolver);

    for ( auto const& param : myParameters ) {
        auto fv = gatherFreeVariables(*param);
        for ( auto& p : fv ) {
            myVariables.push_back(std::make_unique<SymbolVariable>(*this, p->token()));
            p->setFreeVariable(myVariables.back().get());
        }
    }

    ctx.resolveExpressions(myParameters);
}

Slice<Expression*> ParametersPrototype::parameters() const
{
    return myParameters;
}

Slice<SymbolVariable*> ParametersPrototype::symbolVariables() const
{
    return myVariables;
}

void ParametersPrototype::bindVariables(Context& ctx,
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

    ctx.resolveExpressions(myParameters);
}

SymbolVariable* ParametersPrototype::findVariable(std::string const& identifier)
{
    for ( std::size_t i = 0; i < myVariables.size(); ++i )
        if ( myVariables[i]->identifier().lexeme() == identifier )
            return myVariables[i].get();

    return nullptr;
}

SymbolVariable const* ParametersPrototype::findVariable(std::string const& identifier) const
{
    return const_cast<ParametersPrototype*>(this)->findVariable(identifier);
}

SymbolVariable* ParametersPrototype::createVariable(lexer::Token const& identifier)
{
    if ( auto symvar = findVariable(identifier.lexeme()) )
        return symvar;

    myVariables.emplace_back(std::make_unique<SymbolVariable>(*this, identifier));
    return myVariables.back().get();
}

bool ParametersPrototype::isConcrete() const
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

bool ParametersPrototype::hasFreeVariables() const
{
    for ( auto const& e : myVariables )
        if ( !e->boundExpression() )
            return true;

    return false;
}

//
// SymbolPrototype

Symbol::Symbol(lexer::Token const& identifier,
               ParametersPrototype&& params)
    : myIdentifier(identifier)
    , myPrototype(std::make_unique<ParametersPrototype>(std::move(params)))
{
}

Symbol::Symbol(lexer::Token const& identifier)
    : Symbol(identifier, ParametersPrototype())
{
}

Symbol::Symbol(Symbol const& rhs)
    : myIdentifier(rhs.myIdentifier)
    , myPrototypeParent(rhs.myPrototypeParent)
{
    // clone myPrototype
}

Symbol& Symbol::operator = (Symbol const& rhs)
{
    Symbol(rhs).swap(*this);
    return *this;
}

Symbol::Symbol(Symbol&& rhs)
    : myIdentifier(rhs.myIdentifier)
    , myPrototype(std::move(rhs.myPrototype))
{
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
IMPL_CLONE_REMAP(myPrototypeParent)
IMPL_CLONE_REMAP_END

void Symbol::resolveSymbols(Diagnostics& dgn, IResolver& resolver)
{
    myPrototype->resolveSymbols(dgn, resolver);
}

lexer::Token const& Symbol::identifier() const
{
    return myIdentifier;
}

ParametersPrototype const& Symbol::prototype() const
{
    return *myPrototype;
}

Symbol const* Symbol::prototypeParent() const
{
    return myPrototypeParent;
}

//
// SymbolReference

SymbolReference::SymbolReference(std::string const& name, param_list_t parameters)
    : myName(&name)
    , myParameters(parameters)
{
}

SymbolReference::SymbolReference(Symbol const& sym)
    : SymbolReference(sym.identifier().lexeme(), sym.prototype().parameters())
{
}

SymbolReference::SymbolReference(std::string const& name)
    : SymbolReference(name, param_list_t())
{
}

SymbolReference::SymbolReference(const char* name)
    : SymbolReference(name, param_list_t())
{
}

SymbolReference::~SymbolReference() = default;

std::string const& SymbolReference::name() const
{
    return *myName;
}

SymbolReference::param_list_t const& SymbolReference::parameters() const
{
    return myParameters;
}

//
// SymbolSpace

SymbolSpace::SymbolSpace(DeclarationScope* scope, std::string const& name)
    : myScope(scope)
    , myName(name)
{
}

//SymbolSet::SymbolSet(SymbolSet const& rhs)
//    : myScope(rhs.myScope)
//    , mySymbol(rhs.mySymbol)
//    , mySet(rhs.mySet)
//{
//}
//
//SymbolSet& SymbolSet::operator = (SymbolSet const& rhs)
//{
//    SymbolSet(rhs).swap(*this);
//    return *this;
//}

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
                         ParametersPrototype const& prototype,
                         Declaration& declaration)
{
    auto i = begin(myPrototypes);
    for ( ; i != end(myPrototypes); ++i ) {
        if ( prototype.parameters().size() < i->proto.params->parameters().size() )
            break;

        auto v = variance(ctx, i->proto.params->parameters(), prototype.parameters());
        if ( v.equivalent() ) {
            auto& err = ctx.error(declaration) << "matches existing declaration";
            err.see(*i->proto.decl);
            return;
        }
        else if ( v.covariant() ) {
            break;
        }
    }

    myPrototypes.insert(i, Prototype{ {&prototype, &declaration}, std::vector<ParametersDecl>() });
}

Declaration const* SymbolSpace::findEquivalent(Diagnostics& dgn,
                                               param_list_t const& paramlist) const
{
    ScopeResolver resolver(*myScope);
    Context ctx(dgn, resolver);
    for ( auto const& e : myPrototypes ) {
        if ( matchEquivalent(ctx, e.proto.params->parameters(), paramlist) )
            return e.proto.decl;
    }

    return nullptr;
}

SymbolSpace::DeclInstance
SymbolSpace::findValue(Diagnostics& dgn,
                       param_list_t const& paramlist)
{
    ScopeResolver resolver(*myScope);
    Context ctx(dgn, resolver);
    for ( auto& e : myPrototypes ) {
        binding_set_t bindings;
        if ( variance(ctx, bindings, e.proto.params->parameters(), paramlist) ) {
            if ( e.proto.decl->symbol().prototype().isConcrete() )
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
    // use existing instantiation if it exists
    for ( std::size_t i = 0; i < proto.instances.size(); ++i ) {
        auto const& e = proto.instances[i];
        auto const& instParams = e.params->parameters();
        if ( instParams.size() != bindingSet.size() )
            continue;

        auto l = begin(instParams);
        auto r = begin(bindingSet.values());
        while ( l != end(instParams) ) {
            if ( !matchEquivalent(ctx, **l, **r) )
                break;

            ++l;
            ++r;
        }

        if ( l == end(instParams) )
            return { proto.proto.decl, e.decl };
    }

    // create new instantiation
    auto instance = ast::clone(proto.proto.decl);

    instance->symbol().myPrototype->bindVariables(ctx, bindingSet);

    if ( auto proc = instance->as<ProcedureDeclaration>() )
        proc->resolvePrototypeSymbols(ctx.diagnostics());

    instance->resolveSymbols(ctx.diagnostics());

    proto.instances.emplace_back(ParametersDecl{instance->symbol().myPrototype.get(), instance.get()});
    myScope->append(std::move(instance));
    return { proto.proto.decl, proto.instances.back().decl };
}

std::ostream& print(std::ostream& stream, Symbol const& sym)
{
    stream << sym.identifier().lexeme();
    if ( !sym.prototype().parameters().empty() ) {
        stream << "<";
        auto first = begin(sym.prototype().parameters());
        auto last = end(sym.prototype().parameters());
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
