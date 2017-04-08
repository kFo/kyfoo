#include <kyfoo/ast/Semantics.hpp>

#include <kyfoo/ast/Declarations.hpp>

namespace kyfoo {
    namespace ast {

//
// DeclarationTable

DeclarationTable::DeclarationTable(DeclarationTable* parent)
    : myParent(parent)
{
}

void DeclarationTable::append(Declaration* decl)
{
    myEntries[decl->identifier().lexeme()] = decl;
}

Declaration* DeclarationTable::find(std::string const& id)
{
    auto e = myEntries.find(id);
    if ( e != end(myEntries) )
        return e->second;

    return nullptr;
}

Declaration* DeclarationTable::lookup(std::string const& id)
{
    auto e = find(id);
    if ( e )
        return e;

    return myParent ? myParent->find(id) : nullptr;
}

//
// Semantics

DeclarationTable* Semantics::scope()
{
    return myTables.empty() ? nullptr : myTables.back().get();
}

DeclarationTable* Semantics::pushScope()
{
    auto parent = scope();
    myTables.push_back(std::make_unique<DeclarationTable>(parent));
    return scope();
}

void Semantics::popScope()
{
    myTables.pop_back();
}

    } // namespace ast
} // namespace kyfoo
