#include <kyfoo/ast/TypeExpressions.hpp>

#include <kyfoo/Diagnostics.hpp>

#include <kyfoo/ast/Declarations.hpp>
#include <kyfoo/ast/ValueExpressions.hpp>
#include <kyfoo/ast/Semantics.hpp>
#include <kyfoo/ast/Symbol.hpp>

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
                                             std::vector<Expression>&& parameters)
    : myIdentifier(identifier)
    , myParameters(std::move(parameters))
{
}

void PrimaryTypeExpression::io(IStream& stream)
{
    stream.next("identifier", myIdentifier);
    stream.openArray("parameters");
    for ( auto&& e : myParameters ) {
        stream.next("kind", Expression::to_string(e.kind()));
        if ( auto t = e.typeExpression() )
            stream.next("typeExpression", const_cast<TypeExpression*>(t));
        else
            stream.next("valueExpression", const_cast<ValueExpression*>(e.valueExpression()));
    }
    stream.closeArray();
}

void PrimaryTypeExpression::resolveSymbols(Diagnostics& dgn, Resolver& resolver)
{
    if ( !isSpecified() )
        return;

    auto d = resolver.lookup(identifier().lexeme());
    if ( !d ) {
        dgn.undeclared(resolver.module(), identifier());
        return;
    }

    if ( d->kind() == DeclKind::Symbol ) {
        auto s = static_cast<SymbolDeclaration*>(d);
        if ( !s->typeExpression() ) {
            auto& err = dgn.error(resolver.module(), identifier()) << "is not a type";
            err.see(s);
        }

        s->typeExpression()->resolveSymbols(dgn, resolver);
    }
    else if ( d->kind() != DeclKind::Type ) {
        auto& err = dgn.error(resolver.module(), identifier()) << "is not a type";
        err.see(d);
    }

    for ( auto&& p : myParameters ) {
        if ( auto t = p.typeExpression() )
            t->resolveSymbols(dgn, resolver);
    }
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

void TypeExpressionTuple::resolveSymbols(Diagnostics& dgn, Resolver& resolver)
{
    for ( auto&& e : myMembers )
        e->resolveSymbols(dgn, resolver);
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

void ProcedureTypeExpression::resolveSymbols(Diagnostics& dgn, Resolver& resolver)
{
    for ( auto&& e : myParameters )
        e->resolveSymbols(dgn, resolver);

    myReturn->resolveSymbols(dgn, resolver);
}

    } // namespace ast
} // namespace kyfoo
