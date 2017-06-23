#pragma once

#include <memory>

#include <kyfoo/ast/Module.hpp>

namespace kyfoo {
    namespace ast {

class DataSumDeclaration;
class ModuleSet;

class AxiomsModule : public Module
{
public:
    AxiomsModule(ModuleSet* moduleSet, std::string const& name);
    ~AxiomsModule();

public:
    DataSumDeclaration const* emptyType() const;

private:
    std::unique_ptr<DataSumDeclaration> myEmptyType;
};

AxiomsModule* createAxiomsModule(ModuleSet* moduleSet);

    } // namespace ast
} // namespace kyfoo
