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

#define DECLARATION_KINDS(X) \
    X(Type, "type") \
    X(Symbol, "symbol") \
    X(Procedure, "procedure") \
    X(Variable, "variable") \
    X(Import, "import")

enum class DeclKind
{
#define X(a,b) a,
    DECLARATION_KINDS(X)
#undef X
};

const char* to_string(DeclKind kind);

class DeclarationScope;
class ProcedureScope;
class ValueExpression;

class Declaration : public INode
{
protected:
    Declaration(DeclKind kind,
                lexer::Token const& identifier,
                DeclarationScope* scope);

public:
    ~Declaration();

    // IIO
public:
    void io(IStream& stream) override;

public:
    virtual void resolveSymbols(Diagnostics& dgn) = 0;

public:
    DeclKind kind() const;
    lexer::Token const& identifier() const;

public:
    DeclarationScope* scope();
    void setScope(DeclarationScope& parent);

protected:
    DeclKind myKind;
    lexer::Token myIdentifier;
    DeclarationScope* myScope = nullptr;
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

    // Declaration
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
        ValueExpression,
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

    // Declaration
public:
    void resolveSymbols(Diagnostics& dgn) override;

public:
    ValueExpression* valueExpression();
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

    // Declaration
public:
    void resolveSymbols(Diagnostics& dgn) override;

public:
    TypeExpression* typeExpression();
    ValueExpression* valueExpression();

private:
    std::unique_ptr<TypeExpression> myTypeExpression;
    std::unique_ptr<ValueExpression> myValueExpression;
};

class ProcedureDeclaration;
class ProcedureParameter : public VariableDeclaration
{
public:
    explicit ProcedureParameter(lexer::Token const& identifier);
    ProcedureParameter(lexer::Token const& identifier,
                       std::unique_ptr<TypeExpression> type);

    // IIO
public:
    void io(IStream& stream) override;

    // Declaration
public:
    void resolveSymbols(Diagnostics& dgn) override;

public:
    void setParent(ProcedureDeclaration* procDecl);
    ProcedureDeclaration* parent();

private:
    ProcedureDeclaration* myParent = nullptr;
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

    // Declaration
public:
    void resolveSymbols(Diagnostics& dgn) override;

public:
    ProcedureScope* definition();
    void define(std::unique_ptr<ProcedureScope> definition);

    std::vector<std::unique_ptr<ProcedureParameter>>& parameters();
    TypeExpression* returnType();

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

    // Declaration
public:
    void resolveSymbols(Diagnostics& dgn) override;
};
    } // namespace ast
} // namespace kyfoo
