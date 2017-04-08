#pragma once

#include <map>
#include <memory>
#include <tuple>

#include <kyfoo/Error.hpp>

#include <kyfoo/lexer/Scanner.hpp>
#include <kyfoo/lexer/Token.hpp>

#include <kyfoo/ast/Declarations.hpp>
#include <kyfoo/ast/Expressions.hpp>
#include <kyfoo/ast/Node.hpp>
#include <kyfoo/ast/Types.hpp>

namespace kyfoo {
    namespace ast {

using scope_depth_t = int;

class Declaration;
class SymbolDeclaration;
class ProcedureDeclaration;

class DeclarationScope : public INode
{
public:
    DeclarationScope(DeclarationScope* parent);
    ~DeclarationScope();

    // IIO
public:
    void io(IStream& stream) override;

    // INode
public:
    void resolveSymbols(Semantics& semantics) override;

public:
    void append(std::unique_ptr<Declaration> declaration);

    DeclarationScope* parent();
    scope_depth_t depth() const;
    lexer::indent_width_t indent() const;

    void setIndentWidth(lexer::indent_width_t width);

private:
    DeclarationScope* myParent = nullptr;

    std::vector<std::unique_ptr<Declaration>> myDeclarations;
    scope_depth_t myDepth = 0;               // Always defined by the parent scope
    lexer::indent_width_t myIndentWidth = 0; // Comes from the lexer's first token
};

class ProcedureScope : public DeclarationScope
{
public:
    explicit ProcedureScope(DeclarationScope* parent);
    ~ProcedureScope();

    // IIO
public:
    void io(IStream& stream) override;

    // INode
public:
    void resolveSymbols(Semantics& semantics) override;

public:
    void append(std::unique_ptr<Expression> expression);

private:
    std::vector<std::unique_ptr<Expression>> myExpressions;
};

    } // namespace ast
} // namespace kyfoo
