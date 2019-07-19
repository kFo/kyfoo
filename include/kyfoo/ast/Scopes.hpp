#pragma once

#include <map>
#include <tuple>

#include <kyfoo/Diagnostics.hpp>
#include <kyfoo/Types.hpp>
#include <kyfoo/Utilities.hpp>

#include <kyfoo/lexer/Scanner.hpp>
#include <kyfoo/lexer/Token.hpp>

#include <kyfoo/ast/Expressions.hpp>
#include <kyfoo/ast/ControlFlow.hpp>
#include <kyfoo/ast/Clone.hpp>
#include <kyfoo/ast/Symbol.hpp>

namespace kyfoo::ast {

class BasicBlock;
class DataTypeDeclaration;
class Declaration;
class DefinableDeclaration;
class Field;
class ImportDeclaration;
class SymbolDeclaration;
class ProcedureDeclaration;
class Module;
class TemplateDeclaration;

#define SCOPE_KINDS(X)              \
    X(Base      , Scope           ) \
    X(Procedure , ProcedureScope  ) \
    X(DataType  , DataTypeScope   ) \
    X(Template  , TemplateScope   )

class Scope
{
public:
    enum class Kind
    {
#define X(a,b) a,
        SCOPE_KINDS(X)
#undef X
    };

    friend class Context;
    friend class DataTypeScope;
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

    KYFOO_DEBUG_VIRTUAL ~Scope();

    void swap(Scope& rhs) noexcept;

public:
    DECL_CLONE_ALL_NOBASE(Scope)

protected:
    void resolveImports(Diagnostics& dgn);
    SymRes resolveAttributes(Context& ctx);
    SymRes resolveDeclarations(Context& ctx);
    SymRes resolveDefinitions(Context& ctx);

public:
    void append(Box<Declaration> declaration);
    void append(Box<Scope> definition);
    void appendLambda(Box<ProcedureDeclaration> proc,
                      Box<ProcedureScope> defn);
    void import(Module& module);
    void merge(Scope& rhs);

    Lookup findEquivalent(SymbolReference const& symbol) const;

    SymbolVariable& createMetaVariable(lexer::Token const& tok);

protected:
    void setDeclaration(DefinableDeclaration& declaration);

    Lookup findOverload(Context& ctx, SymbolReference const& sym) const;

    bool addSymbol(Diagnostics& dgn, Symbol const& sym, Declaration& decl);
    SymbolSpace* createSymbolSpace(Diagnostics& dgn, stringv name);
    SymbolSpace* findSymbolSpace(stringv name) const;

public:
    Kind kind() const;

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
    std::vector<Box<SymbolVariable>> myMetaVariables;
    std::vector<std::string> myMetaVariableNames;

    // cache resolution
    mutable std::optional<SymRes> myAttribRes;
    mutable std::optional<SymRes> myDeclRes;
    mutable std::optional<SymRes> myDefnRes;

    mutable std::vector<SymbolSpace> mySymbols;
    mutable std::map<std::string, ImportDeclaration*> myImports;
};

class DataTypeScope : public Scope
{
public:
    friend class Context;

public:
    DataTypeScope(Scope& parent,
                     DataTypeDeclaration& declaration);

protected:
    DataTypeScope(DataTypeScope const& rhs);
    DataTypeScope& operator = (DataTypeScope const& rhs);

public:
    DataTypeScope(DataTypeScope&&) = delete;

    ~DataTypeScope() KYFOO_DEBUG_OVERRIDE;

    void swap(DataTypeScope& rhs) noexcept;

    // DeclarationScope
public:
    DECL_CLONE_ALL(Scope)

protected:
    SymRes resolveDeclarations(Context& ctx);
    SymRes resolveDefinitions(Context& ctx);

public:
    void appendField(Symbol symbol,
                     std::vector<Box<Expression>> constraints,
                     Box<Expression> init);

    void appendVariation(Symbol sym);

    DataTypeDeclaration* declaration();

    Slice<Field*> fields();
    Slice<Field const*> fields() const;

    Slice<DataTypeDeclaration*> variations();
    Slice<DataTypeDeclaration const*> variations() const;

private:
    std::vector<Field*> myFields;
    std::vector<DataTypeDeclaration*> myVariations;
};

class ProcedureScope : public Scope
{
public:
    friend class Context;
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
                   lexer::Token openToken,
                   lexer::Token label);

    ProcedureScope(ProcedureScope& parent,
                   BasicBlock* mergeBlock,
                   lexer::Token openToken,
                   lexer::Token label);

    ProcedureScope(ProcedureScope const& rhs);
    ProcedureScope& operator = (ProcedureScope const& rhs);

public:
    ProcedureScope(ProcedureScope&&) = delete;

    ~ProcedureScope() KYFOO_DEBUG_OVERRIDE;

    void swap(ProcedureScope& rhs) noexcept;

    // DeclarationScope
public:
    DECL_CLONE_ALL(Scope)

protected:
    SymRes resolveDeclarations(Context& ctx);
    SymRes resolveDefinitions(Context& ctx);

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

    BasicBlock* entryBlock();
    BasicBlock const* entryBlock() const;

    Expression const* deduceReturnType(Context& ctx);

public:
    void append(Box<Expression> expr);
    void append(Box<VariableDeclaration> var, Box<Expression> expr);

    BasicBlock* createBasicBlock();
    void popBasicBlock();
    ProcedureScope* createChildScope(BasicBlock* mergeBlock,
                                     lexer::Token openToken,
                                     lexer::Token label);

private:
    void appendStatement(Box<Statement> stmt);

    SymRes resolveReturn(Context& ctx);
    void cacheDominators();
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
    friend class Context;

public:
    TemplateScope(Scope& parent,
                  TemplateDeclaration& declaration);

protected:
    TemplateScope(TemplateScope const& rhs);
    TemplateScope& operator = (TemplateScope const& rhs);

public:
    TemplateScope(TemplateScope&&) = delete;

    ~TemplateScope() KYFOO_DEBUG_OVERRIDE;

    void swap(TemplateScope& rhs) noexcept;

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

inline Box<Scope> beginClone(Scope const& scope, CloneMap& map)
{
    switch (scope.kind()) {
#define X(a,b) case Scope::Kind::a: return static_cast<b const&>(scope).beginClone(map);
    SCOPE_KINDS(X)
#undef X
    }

    ENFORCEU("invalid scope type");
}

inline void remap(Scope& scope, CloneMap const& map)
{
    switch (scope.kind()) {
#define X(a,b) case Scope::Kind::a: return static_cast<b&>(scope).remapReferences(map);
    SCOPE_KINDS(X)
#undef X
    }

    ENFORCEU("invalid scope type");
}

} // namespace kyfoo::ast
