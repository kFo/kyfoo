#include <kyfoo/ast/Symbol.hpp>

#include <kyfoo/ast/Declarations.hpp>
#include <kyfoo/ast/Expressions.hpp>
#include <kyfoo/ast/Semantics.hpp>

namespace kyfoo {
    namespace ast {

//
// Symbol

Symbol::Symbol(lexer::Token const& identifier,
               std::vector<std::unique_ptr<Expression>>&& parameters)
    : myIdentifier(identifier)
    , myParameters(std::move(parameters))
{
}

Symbol::Symbol(lexer::Token const& identifier,
               std::unique_ptr<TupleExpression> symbolTuple)
    : myIdentifier(identifier)
    , myParameters(std::move(symbolTuple->expressions()))
{
    if ( symbolTuple->kind() != TupleKind::Symbol )
        throw std::runtime_error("symbol must be created with a symbol tuple");
}

Symbol::Symbol(std::unique_ptr<TupleExpression> symbolTuple)
    : myParameters(std::move(symbolTuple->expressions()))
{
}

Symbol::Symbol(lexer::Token const& identifier)
    : myIdentifier(identifier)
{
}

Symbol::Symbol(Symbol&& rhs)
    : myIdentifier(std::move(rhs.myIdentifier))
    , myParameters(std::move(rhs.myParameters))
    , myVariables(std::move(myVariables))
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
    for ( auto& e : myParameters )
        e->resolveSymbols(dgn, failover);
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
// SymbolSet

bool equal(SymbolSet::paramlist_t const& lhs, SymbolSet::paramlist_t const& rhs)
{
    if ( lhs.size() != rhs.size() )
        return false;

    auto const size = lhs.size();
    for ( std::size_t i = 0; i < size; ++i ) {
        if ( !difference(*lhs[i], *rhs[i]) )
            return false;
    }

    return true;
}

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
    mySet.emplace_back(pair_t{&paramlist, &declaration});
}

Declaration* SymbolSet::find(paramlist_t const& paramlist)
{
    for ( auto const& e : mySet ) {
        if ( equal(*e.paramlist, paramlist) )
            return e.declaration;
    }

    return nullptr;
}

    } // namespace ast
} // namespace kyfoo
