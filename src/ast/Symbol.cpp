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
// Symbol

Symbol::Symbol(std::string const& name)
    : Symbol(lexer::Token(lexer::TokenKind::Identifier, 0, 0, name))
{
}

Symbol::Symbol(lexer::Token const& identifier,
               std::vector<std::unique_ptr<Expression>>&& parameters)
    : myIdentifier(identifier)
    , myParameters(std::move(parameters))
{
}

Symbol::Symbol(lexer::Token const& identifier)
    : myIdentifier(identifier)
{
}

Symbol::Symbol(Symbol const& rhs)
    : myIdentifier(rhs.myIdentifier)
{
}

Symbol& Symbol::operator = (Symbol const& rhs)
{
    Symbol(rhs).swap(*this);
    return *this;
}

Symbol::Symbol(Symbol&& rhs)
    : myIdentifier(std::move(rhs.myIdentifier))
    , myParameters(std::move(rhs.myParameters))
    , myVariables(std::move(rhs.myVariables))
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
    swap(myParameters, rhs.myParameters);
    swap(myVariables, rhs.myVariables);
}

void Symbol::io(IStream& stream) const
{
    stream.next("id", myIdentifier);
    stream.openArray("params");
    for ( auto const& p : myParameters )
        p->io(stream);
    stream.closeArray();
    stream.openArray("vars");
    for ( auto const& v : myVariables )
        v->io(stream);
    stream.closeArray();
}

IMPL_CLONE_NOBASE_BEGIN(Symbol, Symbol)
IMPL_CLONE_CHILD(myParameters)
IMPL_CLONE_CHILD(myVariables)
IMPL_CLONE_END
IMPL_CLONE_REMAP_NOBASE_BEGIN(Symbol)
IMPL_CLONE_REMAP(myParameters)
IMPL_CLONE_REMAP(myVariables)
IMPL_CLONE_REMAP_END

lexer::Token const& Symbol::identifier() const
{
    return myIdentifier;
}

std::string const& Symbol::name() const
{
    return myIdentifier.lexeme();
}

Symbol::paramlist_t const& Symbol::parameters() const
{
    return myParameters;
}

Slice<SymbolVariable*> Symbol::symbolVariables() const
{
    return myVariables;
}

Symbol const* Symbol::parentTemplate() const
{
    return myParentTemplate;
}

bool Symbol::isConcrete() const
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

bool Symbol::hasFreeVariables() const
{
    for ( auto const& e : myVariables )
        if ( !e->boundExpression() )
            return true;

    return false;
}

void Symbol::resolveSymbols(Diagnostics& dgn, IResolver& resolver)
{
    Context ctx(dgn, resolver);

    for ( auto const& param : myParameters ) {
        auto fv = gatherFreeVariables(*param);
        for ( auto& primary : fv ) {
            auto symVar = createVariable(primary->token().lexeme());
            if ( !symVar )
                ctx.error(*primary) << "invalid symbol variable";
            else
                primary->setFreeVariable(symVar);
        }
    }

    ctx.resolveExpressions(myParameters);
}

void Symbol::bindVariables(Context& ctx,
                           Symbol& parentTemplate,
                           binding_set_t const& bindings)
{
    if ( myParentTemplate )
        throw std::runtime_error("template can only be bound once");

    myParentTemplate = &parentTemplate;

    if ( bindings.size() != myVariables.size() )
        throw std::runtime_error("template parameter binding mismatch");

    for ( auto const& e : bindings ) {
        auto var = findVariable(e.first->name());
        if ( !var )
            throw std::runtime_error("template parameter binding mismatch");

        var->bindExpression(e.second);
    }

    ctx.resolveExpressions(myParameters);
}

SymbolVariable* Symbol::findVariable(std::string const& identifier)
{
    for ( auto& e : myVariables )
        if ( e->identifier().lexeme() == identifier )
            return e.get();

    return nullptr;
}

SymbolVariable const* Symbol::findVariable(std::string const& identifier) const
{
    return const_cast<Symbol*>(this)->findVariable(identifier);
}

SymbolVariable* Symbol::createVariable(std::string const& identifier)
{
    if ( auto symvar = findVariable(identifier) )
        return symvar;

    myVariables.emplace_back(std::make_unique<SymbolVariable>(*this, identifier));
    return myVariables.back().get();
}

//
// SymbolReference

SymbolReference::SymbolReference(Symbol const& symbol)
    : SymbolReference(symbol.name(), symbol.parameters())
{
}

SymbolReference::SymbolReference(std::string const& name)
    : SymbolReference(name, paramlist_t())
{
}

SymbolReference::SymbolReference(std::string const& name,
                                 paramlist_t parameters)
    : myName(&name)
    , myParameters(parameters)
{
}

SymbolReference::~SymbolReference() = default;

std::string const& SymbolReference::name() const
{
    return *myName;
}

SymbolReference::paramlist_t const& SymbolReference::parameters() const
{
    return myParameters;
}

//
// SymbolSet

SymbolSet::SymbolSet(DeclarationScope* scope, std::string const& name)
    : myScope(scope)
    , myName(name)
{
}

SymbolSet::SymbolSet(SymbolSet const& rhs)
    : myScope(rhs.myScope)
    , myName(rhs.myName)
    , mySet(rhs.mySet)
{
}

SymbolSet& SymbolSet::operator = (SymbolSet const& rhs)
{
    SymbolSet(rhs).swap(*this);
    return *this;
}

SymbolSet::SymbolSet(SymbolSet&& rhs)
    : myScope(rhs.myScope)
    , myName(std::move(rhs.myName))
    , mySet(std::move(rhs.mySet))
{
    rhs.myScope = nullptr;
}

SymbolSet& SymbolSet::operator = (SymbolSet&& rhs)
{
    this->~SymbolSet();
    new (this) SymbolSet(std::move(rhs));

    return *this;
}

SymbolSet::~SymbolSet() = default;

void SymbolSet::swap(SymbolSet& rhs)
{
    using std::swap;
    swap(myScope, rhs.myScope);
    swap(myName, rhs.myName);
    swap(mySet, rhs.mySet);
}

std::string const& SymbolSet::name() const
{
    return myName;
}

Slice<SymbolSet::SymbolTemplate> const SymbolSet::prototypes() const
{
    return mySet;
}

void SymbolSet::append(paramlist_t const& paramlist, Declaration& declaration)
{
    SymbolTemplate overload;

    auto const size = paramlist.size();
    overload.paramlist.reserve(size);
    for ( auto const& e : paramlist )
        overload.paramlist.push_back(e);

    overload.declaration = &declaration;
    mySet.push_back(overload);
}

Declaration* SymbolSet::findEquivalent(Diagnostics& dgn, SymbolReference::paramlist_t const& paramlist)
{
    ScopeResolver resolver(*myScope);
    Context ctx(dgn, resolver);
    for ( auto const& e : mySet ) {
        if ( matchEquivalent(ctx, e.paramlist, paramlist) )
            return e.declaration;
    }

    return nullptr;
}

Declaration const* SymbolSet::findEquivalent(Diagnostics& dgn, SymbolReference::paramlist_t const& paramlist) const
{
    return const_cast<SymbolSet*>(this)->findEquivalent(dgn, paramlist);
}

SymbolSet::TemplateInstance
SymbolSet::findValue(Diagnostics& dgn,
                     SymbolReference::paramlist_t const& paramlist)
{
    ScopeResolver resolver(*myScope);
    Context ctx(dgn, resolver);
    for ( auto& e : mySet ) {
        ValueMatcher m(ctx);
        if ( m.matchValue(e.paramlist, paramlist) ) {
            if ( e.declaration->symbol().isConcrete() )
                return { e.declaration, nullptr };

            if ( !m.rightBindings.empty() )
                return { e.declaration, nullptr };

            return instantiate(ctx, e, std::move(m.leftBindings));
        }
    }

    return { nullptr, nullptr };
}

SymbolSet::TemplateInstance const
SymbolSet::findValue(Diagnostics& dgn,
                     SymbolReference::paramlist_t const& paramlist) const
{
    return const_cast<SymbolSet*>(this)->findValue(dgn, paramlist);
}

SymbolSet::TemplateInstance
SymbolSet::instantiate(Context& ctx,
                       SymbolTemplate& proto,
                       binding_set_t&& bindingSet)
{
    // use existing instantiation if it exists
    for ( std::size_t i = 0; i < proto.instanceBindings.size(); ++i ) {
        auto const& e = proto.instanceBindings[i];
        if ( e.size() != bindingSet.size() )
            continue;

        auto l = begin(e);
        auto r = begin(bindingSet);
        while ( l != end(e) ) {
            if ( !matchEquivalent(ctx, *l->second, *r->second) )
                break;

            ++l;
            ++r;
        }

        if ( l == end(e) )
            return { proto.declaration, proto.instantiations[i] };
    }

    // create new instantiation
    auto instance = ast::clone(proto.declaration);

    instance->symbol().bindVariables(ctx, proto.declaration->symbol(), bindingSet);

    if ( auto proc = instance->as<ProcedureDeclaration>() )
        proc->resolvePrototypeSymbols(ctx.diagnostics());

    instance->resolveSymbols(ctx.diagnostics());

    proto.instanceBindings.emplace_back(std::move(bindingSet));
    proto.instantiations.push_back(instance.get());
    myScope->append(std::move(instance));
    return { proto.declaration, proto.instantiations.back() };
}

std::ostream& print(std::ostream& stream, Symbol const& sym)
{
    stream << sym.identifier().lexeme();
    if ( !sym.parameters().empty() ) {
        stream << "<";
        auto first = begin(sym.parameters());
        auto last = end(sym.parameters());
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
