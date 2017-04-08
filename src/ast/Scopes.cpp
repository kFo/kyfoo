#include <kyfoo/ast/Scopes.hpp>

#include <cassert>

#include <kyfoo/ast/Declarations.hpp>
#include <kyfoo/ast/Expressions.hpp>
#include <kyfoo/ast/Semantics.hpp>

namespace kyfoo {
    namespace ast {

//
// DeclarationScope

DeclarationScope::DeclarationScope(DeclarationScope* parent)
    : myParent(parent)
    , myDepth(-1)
    , myIndentWidth(0)
{
}

DeclarationScope::~DeclarationScope() = default;

void DeclarationScope::io(IStream& stream)
{
    stream.next("declarations", myDeclarations);
}

void DeclarationScope::resolveSymbols(Semantics& semantics)
{
    semantics.pushScope();
    for ( auto&& e : myDeclarations ) {
        e->resolveSymbols(semantics);
    }
    semantics.popScope();
}

void DeclarationScope::append(std::unique_ptr<Declaration> declaration)
{
    myDeclarations.emplace_back(std::move(declaration));
}

DeclarationScope* DeclarationScope::parent()
{
    return myParent;
}

scope_depth_t DeclarationScope::depth() const
{
    return myDepth;
}

lexer::indent_width_t DeclarationScope::indent() const
{
    return myIndentWidth;
}

void DeclarationScope::setIndentWidth(lexer::indent_width_t width)
{
    if ( myIndentWidth )
        throw std::runtime_error("indent width assigned twice");

    myIndentWidth = width;
}

//
// ProcedureScope

ProcedureScope::ProcedureScope(DeclarationScope* parent)
    : DeclarationScope(parent)
{
}

ProcedureScope::~ProcedureScope() = default;

void ProcedureScope::io(IStream& stream)
{
    DeclarationScope::io(stream);
    stream.next("expressions", myExpressions);
}

void ProcedureScope::resolveSymbols(Semantics& semantics)
{
    for ( auto&& e : myExpressions )
        e->resolveSymbols(semantics);
}

void ProcedureScope::append(std::unique_ptr<Expression> expression)
{
    myExpressions.emplace_back(std::move(expression));
}

    } // namespace ast
} // namespace kyfoo
