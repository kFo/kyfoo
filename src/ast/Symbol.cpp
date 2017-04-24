#include <kyfoo/ast/Symbol.hpp>

#include <kyfoo/ast/TypeExpressions.hpp>
#include <kyfoo/ast/ValueExpressions.hpp>

namespace kyfoo {
    namespace ast {

//
// Expression

const char* Expression::to_string(Kind kind)
{
    switch (kind) {
    case Kind::TypeExpression: return "TypeExpression";
    case Kind::ValueExpression: return "ValueExpression";
    }

    throw std::runtime_error("invalid type parameter");
}

Expression::Expression(std::unique_ptr<TypeExpression> typeExpression)
    : myKind(Kind::TypeExpression)
    , myPtr(typeExpression.release())
{
}

Expression::Expression(std::unique_ptr<ValueExpression> valueExpression)
    : myKind(Kind::ValueExpression)
    , myPtr(valueExpression.release())
{
}

Expression::Expression(Expression&& rhs)
    : myKind(rhs.myKind)
    , myPtr(rhs.myPtr.any)
{
    rhs.myPtr.any = nullptr;
}

Expression& Expression::operator = (Expression&& rhs)
{
    this->~Expression();
    new (this) Expression(std::move(rhs));

    return *this;
}

Expression::~Expression()
{
    if ( myKind == Kind::TypeExpression )
        delete myPtr.asTypeExpression;
    else
        delete myPtr.asExpression;
}

Expression::Kind Expression::kind() const
{
    return myKind;
}

TypeExpression* Expression::typeExpression()
{
    if ( myKind == Kind::TypeExpression )
        return myPtr.asTypeExpression;

    return nullptr;
}

TypeExpression const* Expression::typeExpression() const
{
    return const_cast<Expression*>(this)->typeExpression();
}

ValueExpression* Expression::valueExpression()
{
    if ( myKind == Kind::ValueExpression )
        return myPtr.asExpression;

    return nullptr;
}

ValueExpression const* Expression::valueExpression() const
{
    return const_cast<Expression*>(this)->valueExpression();
}

//
// SymbolParameter

SymbolParameter::SymbolParameter(std::unique_ptr<TypeExpression> typeExpression)
    : myKind(Kind::TypeExpression)
    , myPtr(typeExpression.release())
{
}

SymbolParameter::SymbolParameter(std::unique_ptr<ValueExpression> valueExpression)
    : myKind(Kind::ValueExpression)
    , myPtr(valueExpression.release())
{
}

SymbolParameter::SymbolParameter(Expression expression,
                                 std::unique_ptr<TypeExpression> typeConstraint)
    : myKind(Kind::ConstrainedExpression)
    , myPtr(std::make_unique<ConstrainedExpression>(ConstrainedExpression{std::move(expression), std::move(typeConstraint)}).release())
{
}

SymbolParameter::SymbolParameter(SymbolParameter&& rhs)
    : myKind(rhs.myKind)
    , myPtr(rhs.myPtr.any)
{
    rhs.myPtr.any = nullptr;
}

SymbolParameter& SymbolParameter::operator = (SymbolParameter&& rhs)
{
    this->~SymbolParameter();
    new (this) SymbolParameter(std::move(rhs));

    return *this;
}

SymbolParameter::~SymbolParameter()
{
    switch (myKind) {
    case Kind::ValueExpression: delete myPtr.asValueExpression; break;
    case Kind::TypeExpression: delete myPtr.asTypeExpression; break;
    case Kind::ConstrainedExpression: delete myPtr.asConstrainedExpression; break;
    }
}

SymbolParameter::Kind SymbolParameter::kind() const
{
    return myKind;
}

TypeExpression const* SymbolParameter::typeExpression() const
{
    if ( myKind == Kind::TypeExpression )
        return myPtr.asTypeExpression;

    return nullptr;
}

ValueExpression const* SymbolParameter::valueExpression() const
{
    if ( myKind == Kind::ValueExpression )
        return myPtr.asValueExpression;

    return nullptr;
}

ConstrainedExpression const* SymbolParameter::constrainedExpression() const
{
    if ( myKind == Kind::ConstrainedExpression )
        return myPtr.asConstrainedExpression;

    return nullptr;
}

//
// Symbol

Symbol::Symbol(lexer::Token const& identifier,
               std::vector<SymbolParameter>&& parameters)
    : myIdentifier(identifier)
    , myParameters(std::move(parameters))
{
}

Symbol::Symbol(lexer::Token const& identifier)
    : Symbol(identifier, {})
{
}

Symbol::Symbol(Symbol&& rhs)
    : myIdentifier(std::move(rhs.myIdentifier))
    , myParameters(std::move(rhs.myParameters))
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
    stream.next("identifier", myIdentifier);
    stream.openArray("parameters");
    for ( auto const& p : myParameters ) {
        if ( auto t = p.typeExpression() ) {
            t->io(stream);
        }
        else if ( auto v = p.valueExpression() ) {
            v->io(stream);
        }
        else {
            auto ce = p.constrainedExpression();
            if ( auto tt = ce->expression.typeExpression() )
                tt->io(stream);
            else if ( auto vv = ce->expression.valueExpression() )
                vv->io(stream);

            if ( auto c = ce->constraint.get() )
                stream.next("constraint", c);
        }
    }
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

std::vector<SymbolParameter> const& Symbol::parameters() const
{
    return myParameters;
}

//
// SymbolSet

bool equal(SymbolSet::paramlist_t const& lhs, SymbolSet::paramlist_t const& rhs)
{
    if ( lhs.size() != rhs.size() )
        return false;

    auto const size = lhs.size();
    for ( std::size_t i = 0; i < size; ++i ) {
        if ( lhs[i].kind() != rhs[i].kind() )
            return false;

        if ( auto l = lhs[i].constrainedExpression() ) {
            auto r = rhs[i].constrainedExpression();
            if ( (l->constraint != nullptr) != (r->constraint != nullptr) )
                return false;
        }
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
