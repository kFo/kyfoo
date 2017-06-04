#include <kyfoo/ast/Symbol.hpp>

#include <kyfoo/ast/Declarations.hpp>
#include <kyfoo/ast/Expressions.hpp>
#include <kyfoo/ast/Semantics.hpp>

namespace kyfoo {
    namespace ast {

namespace {

template <typename T>
bool compare(SymbolReference::paramlist_t lhs,
             SymbolReference::paramlist_t rhs,
             T& op)
{
    if ( lhs.size() != rhs.size() )
        return false;

    if ( lhs.empty() && rhs.empty() )
        return true;

    auto const size = lhs.size();
    for ( std::size_t i = 0; i < size; ++i ) {
        if ( op(*lhs[i], *rhs[i]) )
            return true;
    }

    return false;
}

bool equivalent(SymbolReference::paramlist_t lhs,
                SymbolReference::paramlist_t rhs)
{
    auto op = [](auto const& l, auto const& r) { return matchEquivalent(l, r); };
    return compare(lhs, rhs, op);
}

bool matchesPattern(SymbolReference::paramlist_t lhs,
                    SymbolReference::paramlist_t rhs)
{
    auto op = [](auto const& l, auto const& r) { return matchPattern(l, r); };
    return compare(lhs, rhs, op);
}

} // namespace

//
// Symbol

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

bool Symbol::operator == (Symbol const& rhs) const
{
    return name() == rhs.name() && equivalent(parameters(), rhs.parameters());
}

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

void Symbol::resolveSymbols(Diagnostics& dgn, IResolver& resolver)
{
    SymbolVariableCreatorFailoverResolver failover(resolver, *this);
    Context ctx(dgn, failover);
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

SymbolSet::SymbolSet(std::string const& name)
    : myName(name)
{
}

SymbolSet::SymbolSet(SymbolSet&& rhs)
    : myName(std::move(rhs.myName))
    , mySet(std::move(rhs.mySet))
{
}

SymbolSet& SymbolSet::operator = (SymbolSet&& rhs)
{
    this->~SymbolSet();
    new (this) SymbolSet(std::move(rhs));

    return *this;
}

SymbolSet::~SymbolSet() = default;

std::string const& SymbolSet::name() const
{
    return myName;
}

void SymbolSet::append(paramlist_t const& paramlist, Declaration& declaration)
{
    pair_t overload;

    auto const size = paramlist.size();
    overload.paramlist.reserve(size);
    for ( auto const& e : paramlist )
        overload.paramlist.push_back(e);

    overload.declaration = &declaration;
    mySet.push_back(overload);
}

Declaration* SymbolSet::findEquivalent(SymbolReference::paramlist_t const& paramlist)
{
    for ( auto const& e : mySet ) {
        if ( equivalent(e.paramlist, paramlist) )
            return e.declaration;
    }

    return nullptr;
}

Declaration const* SymbolSet::findEquivalent(SymbolReference::paramlist_t const& paramlist) const
{
    return const_cast<SymbolSet*>(this)->findEquivalent(paramlist);
}

Declaration* SymbolSet::findOverload(SymbolReference::paramlist_t const& paramlist)
{
    for ( auto const& e : mySet ) {
        if ( matchesPattern(e.paramlist, paramlist) )
            return e.declaration;
    }

    return nullptr;
}

Declaration const* SymbolSet::findOverload(SymbolReference::paramlist_t const& paramlist) const
{
    return const_cast<SymbolSet*>(this)->findOverload(paramlist);
}

    } // namespace ast
} // namespace kyfoo
