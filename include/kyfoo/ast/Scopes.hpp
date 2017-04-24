#pragma once

#include <map>
#include <memory>
#include <tuple>

#include <kyfoo/Diagnostics.hpp>

#include <kyfoo/lexer/Scanner.hpp>
#include <kyfoo/lexer/Token.hpp>

#include <kyfoo/ast/Declarations.hpp>
#include <kyfoo/ast/ValueExpressions.hpp>
#include <kyfoo/ast/Node.hpp>
#include <kyfoo/ast/Symbol.hpp>
#include <kyfoo/ast/TypeExpressions.hpp>

namespace kyfoo {
    namespace ast {

using scope_depth_t = int;

class Declaration;
class SymbolDeclaration;
class ProcedureDeclaration;
class Module;

class DeclarationScope : public INode
{
public:
    explicit DeclarationScope(Module* module);
    explicit DeclarationScope(DeclarationScope* parent);
    ~DeclarationScope();

    // IIO
public:
    void io(IStream& stream) const override;

public:
    virtual void resolveImports(Diagnostics& dgn);
    virtual void resolveSymbols(Diagnostics& dgn);
    virtual Declaration* find(std::string const& identifier);

public:
    void append(std::unique_ptr<Declaration> declaration);
    void import(Module& module);
    SymbolSet* findSymbol(std::string const& name);
    SymbolSet* createSymbolSet(std::string const& name);

    Module* module();
    DeclarationScope* parent();

private:
    Module* myModule = nullptr;
    DeclarationScope* myParent = nullptr;
    std::vector<std::unique_ptr<Declaration>> myDeclarations;

    std::vector<SymbolSet> mySymbols;
    std::map<std::string, ImportDeclaration*> myImports;
};

class TypeScope : public DeclarationScope
{
public:
    TypeScope(DeclarationScope* parent,
              TypeDeclaration& declaration);
    ~TypeScope();

    // IIO
public:
    void io(IStream& stream) const override;

    // DeclarationScope
public:
    void resolveSymbols(Diagnostics& dgn) override;
    Declaration* find(std::string const& identifier) override;

private:
    TypeDeclaration* myTypeDeclaration = nullptr;
};

class ProcedureScope : public DeclarationScope
{
public:
    ProcedureScope(DeclarationScope* parent,
                   ProcedureDeclaration& declaration);
    ~ProcedureScope();

    // IIO
public:
    void io(IStream& stream) const override;

    // DeclarationScope
public:
    void resolveSymbols(Diagnostics& dgn) override;
    Declaration* find(std::string const& identifier) override;

public:
    void append(std::unique_ptr<ValueExpression> expression);

private:
    ProcedureDeclaration* myDeclaration = nullptr;
    std::vector<std::unique_ptr<ValueExpression>> myExpressions;
};

    } // namespace ast
} // namespace kyfoo
