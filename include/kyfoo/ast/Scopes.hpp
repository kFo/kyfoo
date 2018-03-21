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

class BasicBlock;
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

    Slice<SymbolSpace const* const> trace() const
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
    virtual SymRes resolveSymbols(Module& endModule, Diagnostics& dgn);
    virtual void resolveAttributes(Module& endModule, Diagnostics& dgn);

public:
    void setDeclaration(Declaration* declaration);
    void append(std::unique_ptr<Declaration> declaration);
    void append(std::unique_ptr<DeclarationScope> definition);
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

    Slice<Declaration const*> childDeclarations() const;

    Slice<DeclarationScope const*> childDefinitions() const;

    template <typename T> T* as();
    template <typename T> T const* as() const;

protected:
    Kind myKind = Kind::Declaration;
    Module* myModule = nullptr;
    Declaration* myDeclaration = nullptr;
    DeclarationScope* myParent = nullptr;
    std::vector<std::unique_ptr<Declaration>> myDeclarations;
    std::vector<std::unique_ptr<DeclarationScope>> myDefinitions;

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
    SymRes resolveSymbols(Module& endModule, Diagnostics& dgn) override;

public:
    DataSumDeclaration* declaration();

    Slice<DataSumDeclaration::Constructor*> constructors();
    Slice<DataSumDeclaration::Constructor const*> constructors() const;

private:
    std::vector<DataSumDeclaration::Constructor*> myCtors;
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
    SymRes resolveSymbols(Module& endModule, Diagnostics& dgn) override;

public:
    SymRes resolveConstructors(Module& endModule, Diagnostics& dgn);
    std::unique_ptr<ProcedureDeclaration> createDefaultConstructor();
    TemplateDeclaration* reflectBuilder(TemplateDeclaration const& ctorTempl);

    void resolveDestructor(Module& endModule, Diagnostics& dgn);
    std::unique_ptr<ProcedureDeclaration> createDefaultDestructor();

    DataProductDeclaration* declaration();

    Slice<DataProductDeclaration::Field*> fields();
    Slice<DataProductDeclaration::Field const*> fields() const;

    ProcedureDeclaration const* destructor() const;

private:
    std::vector<DataProductDeclaration::Field*> myFields;
    ProcedureDeclaration const* myDestructor = nullptr;
};

class ProcedureScope : public DeclarationScope
{
public:
    ProcedureScope(DeclarationScope& parent,
                   ProcedureDeclaration& declaration);
    ProcedureScope(DeclarationScope& parent,
                   ProcedureDeclaration& declaration,
                   BasicBlock* mergeBlock);
    ProcedureScope(DeclarationScope& parent,
                   ProcedureDeclaration& declaration,
                   BasicBlock* mergeBlock,
                   lexer::Token const& openToken,
                   lexer::Token const& label);

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
    SymRes resolveSymbols(Module& endModule, Diagnostics& dgn) override;

public:
    bool isJumpTarget() const;

    BasicBlock const* mergeBlock() const;
    BasicBlock* mergeBlock();

    lexer::Token const& openToken() const;
    lexer::Token const& label() const;

    ProcedureDeclaration* declaration();
    ProcedureDeclaration const* declaration() const;

    Slice<ProcedureScope*> childScopes();
    Slice<ProcedureScope const*> childScopes() const;

    Slice<BasicBlock*> basicBlocks();
    Slice<BasicBlock const*> basicBlocks() const;

public:
    void append(std::unique_ptr<Expression> expr);
    void appendConstruction(std::unique_ptr<VarExpression> expr);
    BasicBlock* createBasicBlock();
    void popBasicBlock();
    ProcedureScope* createChildScope(BasicBlock* mergeBlock,
                                     lexer::Token const& openToken,
                                     lexer::Token const& label);
    ProcedureScope* createChildScope(BasicBlock* mergeBlock);

private:
    BasicBlock* myMergeBlock = nullptr;
    lexer::Token myOpenToken;
    lexer::Token myLabel;

    std::vector<std::unique_ptr<ProcedureScope>> myChildScopes;
    std::vector<std::unique_ptr<BasicBlock>> myBasicBlocks;
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
    SymRes resolveSymbols(Module& endModule, Diagnostics& dgn) override;

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
