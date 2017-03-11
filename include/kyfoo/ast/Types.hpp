#pragma once

#include <vector>

namespace kyfoo {
    namespace ast {

class Type
{
public:
    explicit Type() = default;

    explicit Type(lexer::Token const& identifier)
        : myIdentifier(identifier)
    {
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

private:
    std::vector<std::unique_ptr<Type>> myParameters;
    std::unique_ptr<Type> myReturn;
};

    } // namespace ast
} // namespace kyfoo
