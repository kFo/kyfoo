#pragma once

#include <vector>

#include <kyfoo/ast/Node.hpp>

namespace kyfoo {
    namespace ast {

class Type : public INode
{
public:
    explicit Type() = default;

    explicit Type(lexer::Token const& identifier)
        : myIdentifier(identifier)
    {
    }

public:
    void io(IStream& stream) override
    {
        stream.next("identifier", myIdentifier);
    }

private:
    lexer::Token myIdentifier;
};

class ProcedureType : public Type
{
public:
    ProcedureType(std::vector<std::unique_ptr<Type>> parameterTypes,
                  std::unique_ptr<Type> returnType)
        : myParameters(std::move(parameterTypes))
        , myReturn(std::move(returnType))
    {
    }

public:
    void io(IStream& stream) override
    {
        stream.next("parameter", myParameters);
        stream.next("return", myReturn);
    }

private:
    std::vector<std::unique_ptr<Type>> myParameters;
    std::unique_ptr<Type> myReturn;
};

    } // namespace ast
} // namespace kyfoo
