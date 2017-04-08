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


enum class DeclKind
{
    Type,
    Symbol,
    Procedure,
    Variable,
};

class DeclarationScope;
class ProcedureScope;
class Expression;

class Declaration : public INode
{
protected:
    Declaration(DeclKind kind,
                lexer::Token const& identifier,
                DeclarationScope const* parent);

public:
    ~Declaration();

    // IIO
public:
    void io(IStream& stream) override;

    // INode
public:
    virtual void resolveSymbols(Semantics& semantics) = 0;

public:
    DeclKind kind() const;
    lexer::Token const& identifier() const;

public:
    void setParent(DeclarationScope& parent);

protected:
    DeclKind myKind;
    lexer::Token myIdentifier;
    DeclarationScope const* myParent = nullptr;
};

class TypeDeclaration : public Declaration
{
public:
    explicit TypeDeclaration(lexer::Token const& identifier);
    ~TypeDeclaration();

    // IIO
public:
    void io(IStream& stream) override;

    // INode
public:
    void resolveSymbols(Semantics& semantics) override;
};

class SymbolDeclaration : public Declaration
{
public:
    enum class Kind
    {
        Expression,
        TypeExpression,
    };

public:
    SymbolDeclaration(lexer::Token const& identifier,
                      std::unique_ptr<Expression> expression);
    SymbolDeclaration(lexer::Token const& identifier,
                      std::unique_ptr<TypeExpression> typeExpression);
    ~SymbolDeclaration();

    // IIO
public:
    void io(IStream& stream) override;

    // INode
public:
    void resolveSymbols(Semantics& semantics) override;

public:
    Expression* expression();
    TypeExpression* typeExpression();

private:
    Kind myKind;
    std::unique_ptr<INode> myNode;
};

class VariableDeclaration : public Declaration
{
public:
    VariableDeclaration(lexer::Token const& identifier,
                        std::unique_ptr<TypeExpression> type,
                        std::unique_ptr<Expression> expression);
    VariableDeclaration(lexer::Token const& identifier,
                        std::unique_ptr<Expression> expression);
    explicit VariableDeclaration(lexer::Token const& identifier);

    ~VariableDeclaration();

    // IIO
public:
    void io(IStream& stream) override;

    // INode
public:
    void resolveSymbols(Semantics& semantics) override;

public:
    TypeExpression const* typeExpression() const;
    Expression const* expression() const;

private:
    std::unique_ptr<TypeExpression> myTypeExpression;
    std::unique_ptr<Expression> myExpression;
};

class ProcedureParameter : public VariableDeclaration
{
public:
    explicit ProcedureParameter(lexer::Token const& identifier);
    ProcedureParameter(lexer::Token const& identifier,
                       std::unique_ptr<TypeExpression> type);

    // IIO
public:
    void io(IStream& stream) override;

    // INode
public:
    void resolveSymbols(Semantics& semantics) override;
};

class ProcedureDeclaration : public Declaration
{
public:
    ProcedureDeclaration(lexer::Token identifier,
                         std::vector<std::unique_ptr<ProcedureParameter>> parameters,
                         std::unique_ptr<ast::TypeExpression> returnTypeExpression);
    ~ProcedureDeclaration();

    // IIO
public:
    void io(IStream& stream) override;

    // INode
public:
    void resolveSymbols(Semantics& semantics) override;

public:
    ProcedureScope* definition();
    void define(std::unique_ptr<ProcedureScope> definition);

private:
    std::vector<std::unique_ptr<ProcedureParameter>> myParameters;
    std::unique_ptr<TypeExpression> myReturnTypeExpression;
    std::unique_ptr<ProcedureScope> myDefinition;
};

    } // namespace ast
} // namespace kyfoo
