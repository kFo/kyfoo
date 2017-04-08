#pragma once

#include <string>
#include <map>
#include <memory>
#include <vector>

namespace kyfoo {
    namespace ast {

class Declaration;

class DeclarationTable
{
public:
    DeclarationTable(DeclarationTable* parent);

public:
    void append(Declaration* decl);
    Declaration* find(std::string const& id);
    Declaration* lookup(std::string const& id);

private:
    DeclarationTable* myParent = nullptr;
    std::map<std::string, Declaration*> myEntries;
};

class Semantics
{
public:
    DeclarationTable* scope();
    DeclarationTable* pushScope();
    void popScope();

private:
    std::vector<std::unique_ptr<DeclarationTable>> myTables;
};

    } // namespace ast
} // namespace kyfoo
