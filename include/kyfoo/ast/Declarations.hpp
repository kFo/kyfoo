#pragma once

#include <memory>

#include <kyfoo/lexer/Token.hpp>

#include <kyfoo/ast/Node.hpp>
#include <kyfoo/ast/Types.hpp>

namespace kyfoo {

    namespace lexer {
        class Scanner;
    }

    namespace ast {

class DeclarationScope;
class ProcedureScope;
class Expression;

class Declaration : public INode
{
protected:
    Declaration();
    Declaration(DeclarationScope const* parent);

public:
    ~Declaration();

    void io(IStream& stream) override;

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

public:
    void io(IStream& stream) override;

private:
    lexer::Token myIdentifier;
    std::unique_ptr<Expression> myValue;
};

class ProcedureParameter : public INode
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

public:
    void io(IStream& stream) override
    {
        stream.next("identifier", myIdentifier);
        stream.next("type", myType);
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
    void io(IStream& stream) override;

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
