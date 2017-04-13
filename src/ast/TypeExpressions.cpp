#include <kyfoo/ast/TypeExpressions.hpp>

#include <kyfoo/Error.hpp>

#include <kyfoo/ast/Declarations.hpp>
#include <kyfoo/ast/ValueExpressions.hpp>
#include <kyfoo/ast/Semantics.hpp>

namespace kyfoo {
    namespace ast {

//
// PrimaryTypeExpression

PrimaryTypeExpression::PrimaryTypeExpression() = default;

PrimaryTypeExpression::PrimaryTypeExpression(lexer::Token const& identifier)
    : PrimaryTypeExpression(identifier, {})
{
}

PrimaryTypeExpression::PrimaryTypeExpression(lexer::Token const& identifier,
                                             std::vector<TypeParameter>&& parameters)
    : myIdentifier(identifier)
    , myParameters(std::move(parameters))
{
}

void PrimaryTypeExpression::io(IStream& stream)
{
    stream.next("identifier", myIdentifier);
    stream.openArray("parameters");
    for ( auto&& e : myParameters ) {
        stream.next("kind", TypeParameter::to_string(e.kind()));
        if ( auto t = e.typeExpression() )
            stream.next("typeExpression", const_cast<TypeExpression*>(t));
        else
            stream.next("valueExpression", const_cast<ValueExpression*>(e.valueExpression()));
    }
    stream.closeArray();
}

void PrimaryTypeExpression::resolveSymbols(Diagnostics&)
{

}

lexer::Token const& PrimaryTypeExpression::identifier() const
{
    return myIdentifier;
}

TypeDeclaration const* PrimaryTypeExpression::typeDecl() const
{
    return myTypeDeclaration;
}

bool PrimaryTypeExpression::isSpecified() const
{
    return myIdentifier.kind() == lexer::TokenKind::Identifier;
}

//
// TypeParameter

TypeParameter::TypeParameter(std::unique_ptr<TypeExpression> typeExpression)
    : TypeParameter(typeExpression.release())
{
}

TypeParameter::TypeParameter(TypeExpression* typeExpression)
    : myKind(Kind::TypeExpression)
    , myPtr(typeExpression)
{
}

TypeParameter::TypeParameter(std::unique_ptr<ValueExpression> expression)
    : TypeParameter(expression.release())
{
}

TypeParameter::TypeParameter(ValueExpression* expression)
    : myKind(Kind::ValueExpression)
    , myPtr(expression)
{
}

TypeParameter::TypeParameter(TypeParameter&& rhs)
    : myKind(rhs.myKind)
    , myPtr(rhs.myPtr.any)
{
    rhs.myPtr.any = nullptr;
}

TypeParameter& TypeParameter::operator = (TypeParameter&& rhs)
{
    this->~TypeParameter();
    new (this) TypeParameter(std::move(rhs));

    return *this;
}

TypeParameter::~TypeParameter()
{
    if ( myKind == Kind::TypeExpression )
        delete myPtr.asTypeExpression;
    else
        delete myPtr.asExpression;
}

TypeParameter::Kind TypeParameter::kind() const
{
    return myKind;
}

TypeExpression const* TypeParameter::typeExpression() const
{
    if ( myKind == Kind::TypeExpression )
        return myPtr.asTypeExpression;

    return nullptr;
}

ValueExpression const* TypeParameter::valueExpression() const
{
    if ( myKind == Kind::ValueExpression )
        return myPtr.asExpression;

    return nullptr;
}

//
// TypeExpressionTuple

TypeExpressionTuple::TypeExpressionTuple(TupleKind kind)
    : myKind(kind)
{
}

TypeExpressionTuple::TypeExpressionTuple(TupleKind kind,
                                         std::vector<std::unique_ptr<TypeExpression>>&& members)
    : myKind(kind)
    , myMembers(std::move(members))
{
}

void TypeExpressionTuple::io(IStream& stream)
{
    std::string exprkind = typeid(*this).name();
    stream.next("exprkind", exprkind);

    auto kind = to_string(myKind);
    stream.next("kind", kind);
    stream.openArray("expressions");
    for ( auto&& e : myMembers )
        stream.next("expression", e);
    stream.closeArray();
}

void TypeExpressionTuple::resolveSymbols(Diagnostics&)
{
    // TODO
}

//
// ProcedureTypeExpression

ProcedureTypeExpression::ProcedureTypeExpression(std::vector<std::unique_ptr<TypeExpression>> parameterTypes,
                                                 std::unique_ptr<TypeExpression> returnType)
    : myParameters(std::move(parameterTypes))
    , myReturn(std::move(returnType))
{
}

void ProcedureTypeExpression::io(IStream& stream)
{
    stream.next("parameter", myParameters);
    stream.next("return", myReturn);
}

void ProcedureTypeExpression::resolveSymbols(Diagnostics&)
{

}

    } // namespace ast
} // namespace kyfoo
