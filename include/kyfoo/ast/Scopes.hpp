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

    LookupHit(SymbolSpace const* symSpace, Declaration* decl)
        : myDecl(decl)
    {
        if ( symSpace )
            mySpaces.push_back(symSpace);
    }

    explicit LookupHit(SymbolVariable* symVar)
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

    LookupHit& lookup(SymbolSpace const* space, Declaration* decl)
    {
        if ( space )
            mySpaces.push_back(space);

        if ( myDecl )
            throw std::runtime_error("declaration reference stomped");

        myDecl = decl;
        return *this;
    }

    LookupHit& lookup(Declaration* decl)
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

    template <typename T>
    T* as()
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

    Declaration* decl()
    {
        return myDecl;
    }

    Slice<SymbolSpace const*> trace() const
    {
        return mySpaces;
    }

private:
    std::vector<SymbolSpace const*> mySpaces;
    Declaration* myDecl = nullptr;
};

#define SCOPE_KINDS(X)               \
    X(Declaration, DeclarationScope) \
    X(Procedure  , ProcedureScope)   \
    X(DataSum    , DataSumScope)     \
    X(DataProduct, DataProductScope) \
    X(Template   , TemplateScope)

class DeclarationScope : public INode
{
public:
    enum class Kind
    {
#define X(a,b) a,
        SCOPE_KINDS(X)
#undef X
    };

protected:
    DeclarationScope(Kind kind,
                     Module* module,
                     DeclarationScope* parent,
                     Declaration* decl);
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
    virtual void resolveSymbols(Module& endModule, Diagnostics& dgn);
    virtual void resolveAttributes(Module& endModule, Diagnostics& dgn);

public:
    void setDeclaration(Declaration* declaration);
    void append(std::unique_ptr<Declaration> declaration);
    void import(Module& module);
    void merge(DeclarationScope& rhs);

    LookupHit findEquivalent(SymbolReference const& symbol) const;
    LookupHit findOverload(Module& endModule, Diagnostics& dgn, SymbolReference const& sym) const;

    SymbolSpace* createSymbolSpace(Diagnostics& dgn, std::string const& name);
    bool addSymbol(Diagnostics& dgn,
                   Symbol const& sym,
                   Declaration& decl);

    SymbolSpace* findSymbolSpace(std::string const& name) const;

    Module& module();
    Module const& module() const;

    Declaration* declaration();
    Declaration const* declaration() const;

    DeclarationScope* parent();
    DeclarationScope const* parent() const;

    Slice<Declaration*> childDeclarations() const;

    template <typename T> T* as();
    template <typename T> T const* as() const;

protected:
    Kind myKind = Kind::Declaration;
    Module* myModule = nullptr;
    Declaration* myDeclaration = nullptr;
    DeclarationScope* myParent = nullptr;
    std::vector<std::unique_ptr<Declaration>> myDeclarations;

    mutable std::vector<SymbolSpace> mySymbols;
    mutable std::map<std::string, ImportDeclaration*> myImports;
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
    void resolveSymbols(Module& endModule, Diagnostics& dgn) override;

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
    void resolveSymbols(Module& endModule, Diagnostics& dgn) override;

public:
    void resolveConstructors(Diagnostics& dgn);
    std::unique_ptr<ProcedureDeclaration> createDefaultConstructor();

    void resolveDestructor(Module& endModule, Diagnostics& dgn);
    std::unique_ptr<ProcedureDeclaration> createDefaultDestructor();

    DataProductDeclaration* declaration();
    Slice<DataProductDeclaration::Field*> fields();
    const Slice<DataProductDeclaration::Field*> fields() const;
    ProcedureDeclaration const* destructor() const;

private:
    std::vector<DataProductDeclaration::Field*> myFields;
    ProcedureDeclaration const* myDestructor = nullptr;
};

class Statement
{
public:
    explicit Statement(std::unique_ptr<Expression> expr);

protected:
    Statement(Statement const& rhs);
    Statement& operator = (Statement const& rhs);

public:
    Statement(Statement&& rhs);
    Statement& operator = (Statement&& rhs);

    ~Statement();
    void swap(Statement& rhs);

public:
    Statement clone(clone_map_t& map) const;
    void remapReferences(clone_map_t const& map);

public:
    Expression const& expression() const;
    Expression& expression();

    Slice<VariableDeclaration*> const unnamedVariables() const;

    void resolveSymbols(Context& ctx);
    VariableDeclaration const* createUnnamed(ProcedureScope& scope, Declaration const& constraint);
    void appendUnnamed(ProcedureScope& scope, Expression const& expr);

private:
    std::unique_ptr<Expression> myExpression;
    std::vector<std::unique_ptr<VariableDeclaration>> myUnnamedVariables;
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
    void resolveSymbols(Module& endModule, Diagnostics& dgn) override;

public:
    ProcedureDeclaration* declaration();
    ProcedureDeclaration const* declaration() const;
    void append(std::unique_ptr<Expression> expression);

public:
    Slice<Statement> const statements() const;
    Slice<Statement> statements();

private:
    std::vector<Statement> myStatements;
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
    void resolveSymbols(Module& endModule, Diagnostics& dgn) override;

public:
    TemplateDeclaration* declaration();
};

#define X(a, b) template <> inline b* DeclarationScope::as<b>() { return myKind == DeclarationScope::Kind::a ? static_cast<b*>(this) : nullptr; }
SCOPE_KINDS(X)
#undef X

#define X(a, b) template <> inline b const* DeclarationScope::as<b>() const { return myKind == DeclarationScope::Kind::a ? static_cast<b const*>(this) : nullptr; }
SCOPE_KINDS(X)
#undef X

    } // namespace ast
} // namespace kyfoo
