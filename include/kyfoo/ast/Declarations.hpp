#pragma once

#include <memory>

#include <kyfoo/lexer/Token.hpp>

#include <kyfoo/ast/Node.hpp>
#include <kyfoo/ast/TypeExpressions.hpp>

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
    Import,
};

class DeclarationScope;
class ProcedureScope;
class ValueExpression;

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
    virtual void resolveSymbols(Diagnostics& dgn) = 0;

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
    TypeDeclaration(lexer::Token const& identifier,
                    std::vector<TypeParameter>&& parameters);
    ~TypeDeclaration();

    // IIO
public:
    void io(IStream& stream) override;

    // INode
public:
    void resolveSymbols(Diagnostics& dgn) override;

private:
    std::vector<TypeParameter> myParameters;
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
                      std::unique_ptr<ValueExpression> expression);
    SymbolDeclaration(lexer::Token const& identifier,
                      std::unique_ptr<TypeExpression> typeExpression);
    ~SymbolDeclaration();

    // IIO
public:
    void io(IStream& stream) override;

    // INode
public:
    void resolveSymbols(Diagnostics& dgn) override;

public:
    ValueExpression* expression();
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
                        std::unique_ptr<ValueExpression> expression);
    VariableDeclaration(lexer::Token const& identifier,
                        std::unique_ptr<ValueExpression> expression);
    explicit VariableDeclaration(lexer::Token const& identifier);

    ~VariableDeclaration();

    // IIO
public:
    void io(IStream& stream) override;

    // INode
public:
    void resolveSymbols(Diagnostics& dgn) override;

public:
    TypeExpression const* typeExpression() const;
    ValueExpression const* expression() const;

private:
    std::unique_ptr<TypeExpression> myTypeExpression;
    std::unique_ptr<ValueExpression> myExpression;
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
    void resolveSymbols(Diagnostics& dgn) override;
};

class ProcedureDeclaration : public Declaration
{
public:
    ProcedureDeclaration(lexer::Token const& identifier,
                         std::vector<std::unique_ptr<ProcedureParameter>> parameters,
                         std::unique_ptr<ast::TypeExpression> returnTypeExpression);
    ~ProcedureDeclaration();

    // IIO
public:
    void io(IStream& stream) override;

    // INode
public:
    void resolveSymbols(Diagnostics& dgn) override;

public:
    ProcedureScope* definition();
    void define(std::unique_ptr<ProcedureScope> definition);

private:
    std::vector<std::unique_ptr<ProcedureParameter>> myParameters;
    std::unique_ptr<TypeExpression> myReturnTypeExpression;
    std::unique_ptr<ProcedureScope> myDefinition;
};

class ImportDeclaration : public Declaration
{
public:
    explicit ImportDeclaration(lexer::Token const& identifier);
    ~ImportDeclaration();

    // IIO
public:
    void io(IStream& stream) override;

    // INode
public:
    void resolveSymbols(Diagnostics& dgn) override;
};
    } // namespace ast
} // namespace kyfoo
