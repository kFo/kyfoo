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
#include <kyfoo/ast/Semantics.hpp>
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

    LookupHit(SymbolSpace const* symSpace, Declaration const* decl)
        : myDecl(decl)
    {
        if ( symSpace )
            mySpaces.push_back(symSpace);
    }

    explicit LookupHit(SymbolVariable const* symVar)
        : myDecl(symVar)
    {
    }

    LookupHit(LookupHit const&) = delete;

    LookupHit(LookupHit&& rhs)
        : mySpaces(std::move(rhs.mySpaces))
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
        swap(mySpaces, rhs.mySpaces);
        swap(myDecl, rhs.myDecl);
    }

    explicit operator bool () const
    {
        return myDecl;
    }

    LookupHit& lookup(SymbolSpace const* space, Declaration const* decl)
    {
        if ( space )
            mySpaces.push_back(space);

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
        mySpaces.insert(end(mySpaces),
                         begin(rhs.mySpaces), end(rhs.mySpaces));
        myDecl = rhs.myDecl;

        rhs.mySpaces.clear();
        rhs.myDecl = nullptr;

        return *this;
    }

    template <typename T>
    T const* as() const
    {
        if ( decl() )
            return decl()->as<T>();

        return nullptr;
    }

    SymbolSpace const* symSpace() const
    {
        if ( !mySpaces.empty() )
            return mySpaces.front();

        return nullptr;
    }

    Declaration const* decl() const
    {
        return myDecl;
    }

    Slice<SymbolSpace const*> trace() const
    {
        return mySpaces;
    }

private:
    std::vector<SymbolSpace const*> mySpaces;
    Declaration const* myDecl = nullptr;
};

class DeclarationScope : public INode
{
public:
    explicit DeclarationScope(Module& module);
    explicit DeclarationScope(DeclarationScope* parent);
    DeclarationScope(DeclarationScope& parent,
                     Declaration& decl);

protected:
    DeclarationScope(DeclarationScope const& rhs);
    DeclarationScope& operator = (DeclarationScope const& rhs);

public:
    DeclarationScope(DeclarationScope&&) = delete;

    ~DeclarationScope();

    void swap(DeclarationScope& rhs);

    // IIO
public:
    void io(IStream& stream) const override;

public:
    virtual DeclarationScope* clone(clone_map_t& map) const;
    virtual void cloneChildren(DeclarationScope& c, clone_map_t& map) const;
    virtual void remapReferences(clone_map_t const& map);

    virtual void resolveImports(Diagnostics& dgn);
    virtual void resolveSymbols(Diagnostics& dgn);

public:
    void setDeclaration(Declaration* declaration);
    void append(std::unique_ptr<Declaration> declaration);
    void import(Module& module);

    LookupHit findEquivalent(Diagnostics& dgn, SymbolReference const& symbol) const;
    LookupHit findValue(Diagnostics& dgn,
                        std::string const& name,
                        SymbolReference::pattern_t const& params);

    SymbolSpace* createSymbolSpace(Diagnostics& dgn, std::string const& name);
    bool addSymbol(Diagnostics& dgn,
                   Symbol const& sym,
                   Declaration& decl);
    SymbolSpace* findSymbolSpace(Diagnostics& dgn, std::string const& name);
    SymbolSpace const* findSymbolSpace(Diagnostics& dgn, std::string const& name) const;

    Module& module();
    Module const& module() const;

    Declaration* declaration();
    Declaration const* declaration() const;

    DeclarationScope* parent();
    DeclarationScope const* parent() const;

    Slice<Declaration*> childDeclarations() const;

protected:
    Module* myModule = nullptr;
    Declaration* myDeclaration = nullptr;
    DeclarationScope* myParent = nullptr;
    std::vector<std::unique_ptr<Declaration>> myDeclarations;

    std::vector<SymbolSpace> mySymbols;
    std::map<std::string, ImportDeclaration*> myImports;
};

class DataSumScope : public DeclarationScope
{
public:
    DataSumScope(DeclarationScope& parent,
                 DataSumDeclaration& declaration);

protected:
    DataSumScope(DataSumScope const& rhs);
    DataSumScope& operator = (DataSumScope const& rhs);

public:
    DataSumScope(DataSumScope&&) = delete;

    ~DataSumScope();

    void swap(DataSumScope& rhs);

    // IIO
public:
    void io(IStream& stream) const override;

    // DeclarationScope
public:
    DECL_CLONE_ALL(DeclarationScope)
    void resolveSymbols(Diagnostics& dgn) override;

public:
    DataSumDeclaration* declaration();
};

class DataProductScope : public DeclarationScope
{
public:
    DataProductScope(DeclarationScope& parent,
                     DataProductDeclaration& declaration);

protected:
    DataProductScope(DataProductScope const& rhs);
    DataProductScope& operator = (DataProductScope const& rhs);

public:
    DataProductScope(DataProductScope&&) = delete;

    ~DataProductScope();

    void swap(DataProductScope& rhs);

    // IIO
public:
    void io(IStream& stream) const override;

    // DeclarationScope
public:
    DECL_CLONE_ALL(DeclarationScope)
    void resolveSymbols(Diagnostics& dgn) override;

public:
    DataProductDeclaration* declaration();
    Slice<DataProductDeclaration::Field*> fields();
    const Slice<DataProductDeclaration::Field*> fields() const;

private:
    std::vector<DataProductDeclaration::Field*> myFields;
};

class ProcedureScope : public DeclarationScope
{
public:
    ProcedureScope(DeclarationScope& parent,
                   ProcedureDeclaration& declaration);

protected:
    ProcedureScope(ProcedureScope const& rhs);
    ProcedureScope& operator = (ProcedureScope const& rhs);

public:
    ProcedureScope(ProcedureScope&&) = delete;

    ~ProcedureScope();

    void swap(ProcedureScope& rhs);

    // IIO
public:
    void io(IStream& stream) const override;

    // DeclarationScope
public:
    DECL_CLONE_ALL(DeclarationScope)
    void resolveSymbols(Diagnostics& dgn) override;

public:
    ProcedureDeclaration* declaration();
    void append(std::unique_ptr<Expression> expression);

public:
    Slice<Expression*> expressions();
    const Slice<Expression*> expressions() const;

private:
    std::vector<std::unique_ptr<Expression>> myExpressions;
};

class TemplateScope : public DeclarationScope
{
public:
    TemplateScope(DeclarationScope& parent,
                  TemplateDeclaration& declaration);

protected:
    TemplateScope(TemplateScope const& rhs);
    TemplateScope& operator = (TemplateScope const& rhs);

public:
    TemplateScope(TemplateScope&&) = delete;

    ~TemplateScope();

    void swap(TemplateScope& rhs);

    // IIO
public:
    void io(IStream& stream) const override;

    // DeclarationScope
public:
    DECL_CLONE_ALL(DeclarationScope)
    void resolveSymbols(Diagnostics& dgn) override;

public:
    TemplateDeclaration* declaration();
};

    } // namespace ast
} // namespace kyfoo
