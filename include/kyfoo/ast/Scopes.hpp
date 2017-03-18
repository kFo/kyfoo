#pragma once

#include <map>
#include <memory>
#include <tuple>

#include <kyfoo/Error.hpp>

#include <kyfoo/lexer/Scanner.hpp>
#include <kyfoo/lexer/Token.hpp>

#include <kyfoo/ast/Expressions.hpp>
#include <kyfoo/ast/Node.hpp>
#include <kyfoo/ast/Types.hpp>

namespace kyfoo {
    namespace ast {

using scope_depth_t = int;

class SymbolTable
{
public:
    using symbol_t = std::tuple<lexer::Token const*, Type const*>;
    using symbol_list_t = std::vector<symbol_t>;

public:
    void append(lexer::Token const& token, Type const& type);

private:
    std::map<std::string, symbol_list_t> mySymbols;
};

class Declaration;
class SymbolDeclaration;
class ProcedureDeclaration;

class DeclarationScope : public INode
{
public:
    DeclarationScope(DeclarationScope* parent);
    ~DeclarationScope();

public:
    void io(IStream& stream) override
    {
        stream.openArray("declarations");
        for ( auto&& e : myDeclarations )
            stream.next("declaration", e);
        stream.closeArray();
    }

public:
    void append(std::unique_ptr<Declaration> declaration);

    DeclarationScope* parent();
    scope_depth_t depth() const;
    lexer::indent_width_t indent() const;

    void setIndentWidth(lexer::indent_width_t width);

private:
    DeclarationScope* myParent = nullptr;

    SymbolTable mySymbols;
    std::vector<std::unique_ptr<Declaration>> myDeclarations;
    scope_depth_t myDepth = 0;               // Always defined by the parent scope
    lexer::indent_width_t myIndentWidth = 0; // Comes from the lexer's first token
};

class ProcedureScope : public DeclarationScope
{
public:
    explicit ProcedureScope(DeclarationScope* parent);
    ~ProcedureScope();

public:
    void io(IStream& stream) override
    {
        DeclarationScope::io(stream);
        stream.next("expressions", myExpressions);
    }

public:
    void append(std::unique_ptr<Expression> expression);

private:
    std::vector<std::unique_ptr<Expression>> myExpressions;
};

    } // namespace ast
} // namespace kyfoo
