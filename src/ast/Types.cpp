#include <kyfoo/ast/Types.hpp>

#include <kyfoo/Error.hpp>

#include <kyfoo/ast/Declarations.hpp>
#include <kyfoo/ast/Expressions.hpp>
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
}

void PrimaryTypeExpression::resolveSymbols(Semantics& semantics)
{
    if ( !isSpecified() )
        return; // intended to be inferred

    if ( auto d = semantics.scope()->lookup(myIdentifier.lexeme()) )
        if ( d->kind() != DeclKind::Type )
            throw Error(myIdentifier) << "does not refer to a type declaration";

    throw Error(myIdentifier) << "undeclared type";
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

TypeParameter::TypeParameter(std::unique_ptr<Expression> expression)
    : TypeParameter(expression.release())
{
}

TypeParameter::TypeParameter(Expression* expression)
    : myKind(Kind::Expression)
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

Expression const* TypeParameter::expression() const
{
    if ( myKind == Kind::Expression )
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

void TypeExpressionTuple::resolveSymbols(Semantics& /*semantics*/)
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

void ProcedureTypeExpression::resolveSymbols(Semantics& semantics)
{
    for ( auto&& e : myParameters )
        e->resolveSymbols(semantics);

    myReturn->resolveSymbols(semantics);
}

    } // namespace ast
} // namespace kyfoo
