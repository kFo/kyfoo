#include <kyfoo/ast/Scopes.hpp>

#include <cassert>

#include <kyfoo/ast/Declarations.hpp>
#include <kyfoo/ast/ValueExpressions.hpp>
#include <kyfoo/ast/Module.hpp>
#include <kyfoo/ast/Semantics.hpp>

namespace kyfoo {
    namespace ast {

//
// DeclarationScope

DeclarationScope::DeclarationScope(DeclarationScope* parent)
    : myParent(parent)
{
}

DeclarationScope::~DeclarationScope() = default;

void DeclarationScope::io(IStream& stream)
{
    stream.next("declarations", myDeclarations);
}

void DeclarationScope::resolveSymbols(Diagnostics&)
{
    
}

void DeclarationScope::append(std::unique_ptr<Declaration> declaration)
{
    myDeclarations.emplace_back(std::move(declaration));
}

void DeclarationScope::import(Module& module)
{
    for ( auto&& e : myImports )
        if ( e.module == &module )
            throw std::runtime_error("module imported twice: " + module.name());

    myImports.push_back({&module, nullptr});
}

DeclarationScope* DeclarationScope::parent()
{
    return myParent;
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

void ProcedureScope::resolveSymbols(Diagnostics&)
{

}

void ProcedureScope::append(std::unique_ptr<ValueExpression> expression)
{
    myExpressions.emplace_back(std::move(expression));
}

    } // namespace ast
} // namespace kyfoo
