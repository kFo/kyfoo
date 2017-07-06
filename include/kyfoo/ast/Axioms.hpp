#pragma once

#include <memory>

#include <kyfoo/ast/Module.hpp>

namespace kyfoo {
    namespace ast {

class DataSumDeclaration;
class ProcedureDeclaration;
class ModuleSet;

class AxiomsModule : public Module
{
protected:
    friend class ModuleSet;
    AxiomsModule(ModuleSet* moduleSet, std::string const& name);
    
    bool init();

public:
    ~AxiomsModule();

public:
    DataSumDeclaration const* emptyType() const;
    DataSumDeclaration const* integerType() const;
    DataSumDeclaration const* integerTemplate() const;
    DataSumDeclaration const* pointerTemplate() const;

    ProcedureDeclaration const* addInstruction() const;

private:
    std::unique_ptr<DataSumDeclaration> myEmptyType;
    DataSumDeclaration const* myIntegerType = nullptr;
    DataSumDeclaration const* myIntegerTemplate = nullptr;
    DataSumDeclaration const* myPointerTemplate = nullptr;
    ProcedureDeclaration const* myAddInstruction = nullptr;
};

    } // namespace ast
} // namespace kyfoo
