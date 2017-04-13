#pragma once

#include <memory>
#include <vector>

#include <kyfoo/ast/Tuples.hpp>
#include <kyfoo/ast/Node.hpp>

namespace kyfoo {
    namespace ast {

class TypeDeclaration;
class TypeExpression;
class ValueExpression;

class TypeExpression : public INode
{
    // IIO
public:
    void io(IStream& stream) = 0;

    // INode
public:
    void resolveSymbols(Diagnostics& dgn) = 0;
};

class TypeParameter
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
    explicit TypeParameter(std::unique_ptr<TypeExpression> typeExpression);
    explicit TypeParameter(TypeExpression* typeExpression);
    explicit TypeParameter(std::unique_ptr<ValueExpression> expression);
    explicit TypeParameter(ValueExpression* expression);

    TypeParameter(TypeParameter const&) = delete;

    TypeParameter(TypeParameter&& rhs);
    TypeParameter& operator = (TypeParameter&& rhs);

    ~TypeParameter();

public:
    Kind kind() const;
    TypeExpression const* typeExpression() const;
    ValueExpression const* valueExpression() const;

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
                          std::vector<TypeParameter>&& parameters);

    // IIO
public:
    void io(IStream& stream) override;

    // INode
public:
    void resolveSymbols(Diagnostics& dgn) override;

public:
    lexer::Token const& identifier() const;
    TypeDeclaration const* typeDecl() const;
    bool isSpecified() const;

private:
    lexer::Token myIdentifier;
    std::vector<TypeParameter> myParameters;
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

    // INode
public:
    void resolveSymbols(Diagnostics& dgn) override;

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

    // INode
public:
    void resolveSymbols(Diagnostics& dgn) override;

private:
    std::vector<std::unique_ptr<TypeExpression>> myParameters;
    std::unique_ptr<TypeExpression> myReturn;
};

    } // namespace ast
} // namespace kyfoo
