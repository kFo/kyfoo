#pragma once

#include <map>
#include <tuple>

#include <kyfoo/Diagnostics.hpp>
#include <kyfoo/Types.hpp>
#include <kyfoo/Utilities.hpp>

#include <kyfoo/lexer/Scanner.hpp>
#include <kyfoo/lexer/Token.hpp>

#include <kyfoo/ast/Declarations.hpp>
#include <kyfoo/ast/Expressions.hpp>
#include <kyfoo/ast/Node.hpp>
#include <kyfoo/ast/Semantics.hpp>
#include <kyfoo/ast/Symbol.hpp>

namespace kyfoo::ast {

class BasicBlock;
class Declaration;
class SymbolDeclaration;
class ProcedureDeclaration;
class Module;

#define SCOPE_KINDS(X)               \
    X(Base       , Scope           ) \
    X(Procedure  , ProcedureScope  ) \
    X(DataSum    , DataSumScope    ) \
    X(DataProduct, DataProductScope) \
    X(Template   , TemplateScope   )

class Scope : public INode
{
public:
    enum class Kind
    {
#define X(a,b) a,
        SCOPE_KINDS(X)
#undef X
    };

    friend class Context;
    friend class DataProductScope;
    template <typename T> friend class DefinableMixin;
    friend class Module;
    friend class Resolver;

protected:
    Scope(Kind kind,
                     Module* module,
                     Scope* parent);
public:
    explicit Scope(Module& module);
    explicit Scope(Scope* parent);

protected:
    Scope(Scope const& rhs);
    Scope& operator = (Scope const& rhs);

public:
    Scope(Scope&&) = delete;
    void operator = (Scope&&) = delete;

    ~Scope();

    void swap(Scope& rhs) noexcept;

    // IIO
public:
    void io(IStream& stream) const override;

public:
    virtual Scope* clone(clone_map_t& map) const;
    virtual void cloneChildren(Scope& c, clone_map_t& map) const;
    virtual void remapReferences(clone_map_t const& map);

protected:
    virtual void resolveImports(Diagnostics& dgn);
    SymRes resolveSymbols(Context& ctx);
    virtual SymRes resolveDeclarations(Context& ctx);
    virtual SymRes resolveDefinitions(Context& ctx);
    virtual SymRes resolveAttributes(Context& ctx);

public:
    void append(Box<Declaration> declaration);
    void append(Box<Scope> definition);
    void appendLambda(Box<ProcedureDeclaration> proc,
                      Box<ProcedureScope> defn);
    void import(Module& module);
    void merge(Scope& rhs);

    Lookup findEquivalent(SymbolReference const& symbol) const;

protected:
    void setDeclaration(DefinableDeclaration& declaration);

    Lookup findOverload(Context& ctx, SymbolReference const& sym) const;

    SymbolSpace* createSymbolSpace(Diagnostics& dgn, std::string_view name);
    bool addSymbol(Diagnostics& dgn,
                   Symbol const& sym,
                   Declaration& decl);

    SymbolSpace* findSymbolSpace(std::string_view name) const;

public:
    Module& module();
    Module const& module() const;

    DefinableDeclaration* declaration();
    DefinableDeclaration const* declaration() const;

    Scope* parent();
    Scope const* parent() const;

    Slice<Declaration const*> childDeclarations() const;
    Slice<Scope const*> childDefinitions() const;
    Slice<ProcedureDeclaration const*> childLambdas() const;

    template <typename T> T* as();
    template <typename T> T const* as() const;

protected:
    Kind myKind = Kind::Base;
    Module* myModule = nullptr;
    DefinableDeclaration* myDeclaration = nullptr;
    Scope* myParent = nullptr;
    std::vector<Box<Declaration>> myDeclarations;
    std::vector<Box<Scope>> myDefinitions;
    std::vector<Box<ProcedureDeclaration>> myLambdas;

    mutable std::vector<SymbolSpace> mySymbols;
    mutable std::map<std::string, ImportDeclaration*> myImports;
};

class DataSumScope : public Scope
{
public:
    DataSumScope(Scope& parent,
                 DataSumDeclaration& declaration);

protected:
    DataSumScope(DataSumScope const& rhs);
    DataSumScope& operator = (DataSumScope const& rhs);

public:
    DataSumScope(DataSumScope&&) = delete;

    ~DataSumScope();

    void swap(DataSumScope& rhs) noexcept;

    // IIO
public:
    void io(IStream& stream) const override;

    // DeclarationScope
public:
    DECL_CLONE_ALL(Scope)

protected:
    SymRes resolveDeclarations(Context& ctx) override;
    SymRes resolveDefinitions(Context& ctx) override;

public:
    DataSumDeclaration* declaration();

    Slice<DataSumDeclaration::Constructor*> constructors();
    Slice<DataSumDeclaration::Constructor const*> constructors() const;

private:
    std::vector<DataSumDeclaration::Constructor*> myCtors;
};

class DataProductScope : public Scope
{
public:
    DataProductScope(Scope& parent,
                     DataProductDeclaration& declaration);

protected:
    DataProductScope(DataProductScope const& rhs);
    DataProductScope& operator = (DataProductScope const& rhs);

public:
    DataProductScope(DataProductScope&&) = delete;

    ~DataProductScope();

    void swap(DataProductScope& rhs) noexcept;

    // IIO
public:
    void io(IStream& stream) const override;

    // DeclarationScope
public:
    DECL_CLONE_ALL(Scope)

protected:
    SymRes resolveDeclarations(Context& ctx) override;
    SymRes resolveDefinitions(Context& ctx) override;

public:
    DataProductDeclaration* declaration();

    Slice<DataProductDeclaration::Field*> fields();
    Slice<DataProductDeclaration::Field const*> fields() const;

private:
    std::vector<DataProductDeclaration::Field*> myFields;
};

class ProcedureScope : public Scope
{
public:
    using declaration_t = ProcedureDeclaration;

public:
    ProcedureScope(Scope& parent,
                   ProcedureDeclaration& declaration);
    ProcedureScope(Scope& parent,
                   ProcedureDeclaration& declaration,
                   BasicBlock* mergeBlock);

protected:
    ProcedureScope(Scope& parent,
                   ProcedureDeclaration& declaration,
                   BasicBlock* mergeBlock,
                   lexer::Token const& openToken,
                   lexer::Token const& label);

    ProcedureScope(ProcedureScope& parent,
                   BasicBlock* mergeBlock,
                   lexer::Token const& openToken,
                   lexer::Token const& label);

    ProcedureScope(ProcedureScope const& rhs);
    ProcedureScope& operator = (ProcedureScope const& rhs);

public:
    ProcedureScope(ProcedureScope&&) = delete;

    ~ProcedureScope();

    void swap(ProcedureScope& rhs) noexcept;

    // IIO
public:
    void io(IStream& stream) const override;

    // DeclarationScope
public:
    DECL_CLONE_ALL(Scope)

protected:
    SymRes resolveDeclarations(Context& ctx) override;
    SymRes resolveDefinitions(Context& ctx) override;

public:
    bool isJumpTarget() const;
    bool isTop() const;

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
    void append(Box<Expression> expr);
    BasicBlock* createBasicBlock();
    void popBasicBlock();
    ProcedureScope* createChildScope(BasicBlock* mergeBlock,
                                     lexer::Token const& openToken,
                                     lexer::Token const& label);

private:
    SymRes cacheVariableExtents(Context& ctx);

private:
    BasicBlock* myMergeBlock = nullptr;
    lexer::Token myOpenToken;
    lexer::Token myLabel;

    std::vector<Box<ProcedureScope>> myChildScopes;
    std::vector<Box<BasicBlock>> myBasicBlocks;

    mutable std::vector<Extent> myExtents;
};

class TemplateScope : public Scope
{
public:
    TemplateScope(Scope& parent,
                  TemplateDeclaration& declaration);

protected:
    TemplateScope(TemplateScope const& rhs);
    TemplateScope& operator = (TemplateScope const& rhs);

public:
    TemplateScope(TemplateScope&&) = delete;

    ~TemplateScope();

    void swap(TemplateScope& rhs) noexcept;

    // IIO
public:
    void io(IStream& stream) const override;

    // DeclarationScope
public:
    DECL_CLONE_ALL(Scope)

protected:
    SymRes resolveDeclarations(Context& ctx);
    SymRes resolveDefinitions(Context& ctx);

public:
    TemplateDeclaration* declaration();
};

#define X(a, b) template <> inline b* Scope::as<b>() { return myKind == Scope::Kind::a ? static_cast<b*>(this) : nullptr; }
SCOPE_KINDS(X)
#undef X

#define X(a, b) template <> inline b const* Scope::as<b>() const { return myKind == Scope::Kind::a ? static_cast<b const*>(this) : nullptr; }
SCOPE_KINDS(X)
#undef X

} // namespace kyfoo::ast
