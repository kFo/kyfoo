#pragma once

#include <memory>
#include <vector>

#include <kyfoo/ast/Tuples.hpp>
#include <kyfoo/ast/Node.hpp>

namespace kyfoo {
    class Diagnostics;

    namespace ast {

class TypeDeclaration;
class TypeExpression;
class ValueExpression;

class Resolver;

class TypeExpression : public INode
{
    // IIO
public:
    void io(IStream& stream) = 0;

public:
    virtual void resolveSymbols(Diagnostics& dgn, Resolver& resolver) = 0;
};

class TypeArgument
{
public:
    enum class Kind
    {
        TypeExpression,
        ValueExpression,
    };

    static const char* to_string(Kind kind)
    {
        switch (kind) {
        case Kind::TypeExpression: return "TypeExpression";
        case Kind::ValueExpression: return "ValueExpression";
        }

        throw std::runtime_error("invalid type parameter");
    }

public:
    explicit TypeArgument(std::unique_ptr<TypeExpression> typeExpression);
    explicit TypeArgument(TypeExpression* typeExpression);
    explicit TypeArgument(std::unique_ptr<ValueExpression> expression);
    explicit TypeArgument(ValueExpression* expression);

    TypeArgument(TypeArgument const&) = delete;

    TypeArgument(TypeArgument&& rhs);
    TypeArgument& operator = (TypeArgument&& rhs);

    ~TypeArgument();

public:
    Kind kind() const;
    TypeExpression* typeExpression();
    ValueExpression* valueExpression();

private:
    Kind myKind;
    union Ptr {
        Ptr() : any(nullptr) {}
        Ptr(void* rhs) : any(rhs) {}
        Ptr(TypeExpression* rhs) : asTypeExpression(rhs) {}
        Ptr(ValueExpression* rhs) : asExpression(rhs) {}

        void* any;
        TypeExpression* asTypeExpression;
        ValueExpression* asExpression;
    } myPtr;
};

class PrimaryTypeExpression : public TypeExpression
{
public:
    explicit PrimaryTypeExpression();
    explicit PrimaryTypeExpression(lexer::Token const& identifier);
    PrimaryTypeExpression(lexer::Token const& identifier,
                          std::vector<TypeArgument>&& parameters);

    // IIO
public:
    void io(IStream& stream) override;

    // TypeExpression
public:
    void resolveSymbols(Diagnostics& dgn, Resolver& resolver) override;

public:
    lexer::Token const& identifier() const;
    TypeDeclaration const* typeDecl() const;
    bool isSpecified() const;

private:
    lexer::Token myIdentifier;
    std::vector<TypeArgument> myParameters;
    TypeDeclaration const* myTypeDeclaration = nullptr;
};

class TypeExpressionTuple : public TypeExpression
{
public:
    explicit TypeExpressionTuple(TupleKind kind);
    TypeExpressionTuple(TupleKind kind,
                        std::vector<std::unique_ptr<TypeExpression>>&& members);

    // IIO
public:
    void io(IStream& stream) override;

    // TypeExpression
public:
    void resolveSymbols(Diagnostics& dgn, Resolver& resolver) override;

private:
    TupleKind myKind;
    std::vector<std::unique_ptr<TypeExpression>> myMembers;
};

class ProcedureTypeExpression : public TypeExpression
{
public:
    ProcedureTypeExpression(std::vector<std::unique_ptr<TypeExpression>> parameterTypes,
                            std::unique_ptr<TypeExpression> returnType);

    // IIO
public:
    void io(IStream& stream) override;

    // TypeExpression
public:
    void resolveSymbols(Diagnostics& dgn, Resolver& resolver) override;

private:
    std::vector<std::unique_ptr<TypeExpression>> myParameters;
    std::unique_ptr<TypeExpression> myReturn;
};

    } // namespace ast
} // namespace kyfoo
