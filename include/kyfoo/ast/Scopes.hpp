#pragma once

#include <map>
#include <memory>
#include <tuple>

#include <kyfoo/Diagnostics.hpp>

#include <kyfoo/lexer/Scanner.hpp>
#include <kyfoo/lexer/Token.hpp>

#include <kyfoo/ast/Declarations.hpp>
#include <kyfoo/ast/Expressions.hpp>
#include <kyfoo/ast/Node.hpp>
#include <kyfoo/ast/Symbol.hpp>

namespace kyfoo {
    namespace ast {

using scope_depth_t = int;

class Declaration;
class SymbolDeclaration;
class ProcedureDeclaration;
class Module;

class LookupHit
{
public:
    LookupHit() = default;

    LookupHit(SymbolSet const* symSet, Declaration const* decl)
        : myDecl(decl)
    {
        if ( symSet )
            mySymSets.push_back(symSet);
    }

    explicit LookupHit(SymbolVariable const* symVar)
        : myDecl(symVar)
    {
    }

    LookupHit(LookupHit const&) = delete;

    LookupHit(LookupHit&& rhs)
        : mySymSets(std::move(rhs.mySymSets))
        , myDecl(rhs.myDecl)
    {
        rhs.myDecl = nullptr;
    }

    LookupHit& operator = (LookupHit&& rhs)
    {
        LookupHit(std::move(rhs)).swap(*this);
        return *this;
    }

    ~LookupHit() = default;

    void swap(LookupHit& rhs)
    {
        using std::swap;
        swap(mySymSets, rhs.mySymSets);
        swap(myDecl, rhs.myDecl);
    }

    explicit operator bool () const
    {
        return myDecl;
    }

    LookupHit& lookup(SymbolSet const* symSet, Declaration const* decl)
    {
        if ( symSet )
            mySymSets.push_back(symSet);

        if ( myDecl )
            throw std::runtime_error("declaration reference stomped");

        myDecl = decl;
        return *this;
    }

    LookupHit& lookup(Declaration const* decl)
    {
        if ( myDecl )
            throw std::runtime_error("declaration reference stomped");

        myDecl = decl;
        return *this;
    }

    LookupHit& append(LookupHit&& rhs)
    {
        mySymSets.insert(end(mySymSets),
                         begin(rhs.mySymSets), end(rhs.mySymSets));
        myDecl = rhs.myDecl;

        rhs.mySymSets.clear();
        rhs.myDecl = nullptr;

        return *this;
    }

    template <typename T>
    T const* as() const
    {
        if ( myDecl )
            return myDecl->as<T>();

        return nullptr;
    }

    SymbolSet const* symSet() const
    {
        if ( !mySymSets.empty() )
            return mySymSets.front();

        return nullptr;
    }

    Declaration const* decl() const
    {
        return myDecl;
    }

    Slice<SymbolSet const*> trace() const
    {
        return mySymSets;
    }

private:
    std::vector<SymbolSet const*> mySymSets;
    Declaration const* myDecl = nullptr;
};

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

public:
    void setDeclaration(Declaration* declaration);
    void append(std::unique_ptr<Declaration> declaration);
    void import(Module& module);

    LookupHit findEquivalent(SymbolReference const& symbol) const;
    LookupHit findValue(SymbolReference const& symbol) const;
    LookupHit findProcedureOverload(SymbolReference const& procOverload) const;

    SymbolSet* createSymbolSet(std::string const& name);
    SymbolSet* createProcedureOverloadSet(std::string const& name);
    bool addSymbol(Diagnostics& dgn, Symbol const& sym, Declaration& decl);
    bool addProcedure(Diagnostics& dgn, Symbol const& sym, ProcedureDeclaration& procDecl);
    SymbolSet const* findSymbol(std::string const& identifier) const;
    SymbolSet const* findProcedure(std::string const& identifier) const;

    Module* module();
    Declaration* declaration();
    DeclarationScope* parent();

    Slice<Declaration*> childDeclarations() const;

protected:
    Module* myModule = nullptr;
    Declaration* myDeclaration = nullptr;
    DeclarationScope* myParent = nullptr;
    std::vector<std::unique_ptr<Declaration>> myDeclarations;

    std::vector<SymbolSet> mySymbols;
    std::vector<SymbolSet> myProcedureOverloads;
    std::map<std::string, ImportDeclaration*> myImports;
};

class DataSumScope : public DeclarationScope
{
public:
    DataSumScope(DeclarationScope* parent,
                 DataSumDeclaration& declaration);
    ~DataSumScope();

    // IIO
public:
    void io(IStream& stream) const override;

    // DeclarationScope
public:
    void resolveSymbols(Diagnostics& dgn) override;

private:
    DataSumDeclaration* myDataDeclaration = nullptr;
};

class DataProductScope : public DeclarationScope
{
public:
    DataProductScope(DeclarationScope* parent,
                     DataProductDeclaration& declaration);
    ~DataProductScope();

    // IIO
public:
    void io(IStream& stream) const override;

    // DeclarationScope
public:

private:
    DataProductDeclaration* myDataDeclaration = nullptr;
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

public:
    void append(std::unique_ptr<Expression> expression);

private:
    ProcedureDeclaration* myDeclaration = nullptr;
    std::vector<std::unique_ptr<Expression>> myExpressions;
};

    } // namespace ast
} // namespace kyfoo
