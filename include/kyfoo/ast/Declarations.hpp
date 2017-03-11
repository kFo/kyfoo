#pragma once

#include <memory>

#include <kyfoo/lexer/Token.hpp>

#include <kyfoo/ast/Types.hpp>

namespace kyfoo {

    namespace lexer {
        class Scanner;
    }

    namespace ast {

class DeclarationScope;
class ProcedureScope;
class Expression;

class Declaration
{
protected:
    Declaration();
    Declaration(DeclarationScope const* parent);

public:
    ~Declaration();

public:
    void setParent(DeclarationScope& parent);

protected:
    DeclarationScope const* myParent = nullptr;
};

class SymbolDeclaration : public Declaration
{
public:
    SymbolDeclaration(lexer::Token identifier,
                      std::unique_ptr<Expression> value);
    ~SymbolDeclaration();

private:
    lexer::Token myIdentifier;
    std::unique_ptr<Expression> myValue;
};

class ProcedureParameter
{
public:
    explicit ProcedureParameter(lexer::Token identifier)
        : myIdentifier(identifier)
    {
    }

    ProcedureParameter(lexer::Token identifier,
                       std::unique_ptr<Type> type)
        : myIdentifier(identifier)
        , myType(std::move(type))
    {
    }

private:
    lexer::Token myIdentifier;
    std::unique_ptr<Type> myType;
};

class ProcedureDeclaration : public Declaration
{
public:
    ProcedureDeclaration(lexer::Token identifier,
                         std::vector<std::unique_ptr<ProcedureParameter>> parameters);
    ~ProcedureDeclaration();

public:
    ProcedureScope* definition();

public:
    void define(std::unique_ptr<ProcedureScope> definition);

private:
    lexer::Token myIdentifier;
    std::vector<std::unique_ptr<ProcedureParameter>> myParameters;
    std::unique_ptr<ProcedureScope> myDefinition;
};

    } // namespace ast
} // namespace kyfoo
