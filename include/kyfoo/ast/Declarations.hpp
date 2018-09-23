#pragma once

#include <kyfoo/Types.hpp>
#include <kyfoo/Utilities.hpp>

#include <kyfoo/lexer/Token.hpp>

#include <kyfoo/ast/Clone.hpp>
#include <kyfoo/ast/Symbol.hpp>
#include <kyfoo/ast/Expressions.hpp>

#include <kyfoo/codegen/Codegen.hpp>

namespace kyfoo {

    namespace lexer {
        class Scanner;
    }

    namespace ast {

#define DECLARATION_KINDS(X)                                                      \
    X(DataSum           , "data sum"           , DataSumDeclaration             ) \
    X(DataSumCtor       , "data sum ctor"      , DataSumDeclaration::Constructor) \
    X(DataProduct       , "data product"       , DataProductDeclaration         ) \
    X(Field             , "field"              , DataProductDeclaration::Field  ) \
    X(Symbol            , "symbol"             , SymbolDeclaration              ) \
    X(Procedure         , "procedure"          , ProcedureDeclaration           ) \
    X(ProcedureParameter, "procedure parameter", ProcedureParameter             ) \
    X(Variable          , "variable"           , VariableDeclaration            ) \
    X(Import            , "import"             , ImportDeclaration              ) \
    X(SymbolVariable    , "symbol variable"    , SymbolVariable                 ) \
    X(Template          , "template"           , TemplateDeclaration            )

enum class DeclKind
{
#define X(a,b,c) a,
    DECLARATION_KINDS(X)
#undef X
};

const char* to_string(DeclKind kind);

class Scope;
class DataSumScope;
class DataProductScope;
class Module;
class ProcedureScope;
class Statement;
class TemplateScope;
class ValueExpression;

class Declaration
{
public:
    friend class Context;

protected:
    Declaration(DeclKind kind,
                Symbol&& symbol,
                Scope* scope);

    Declaration(Declaration const& rhs);
    void operator = (Declaration const&) = delete;

    Declaration(Declaration&&) = delete;
    void operator = (Declaration&&) = delete;

public:
    KYFOO_DEBUG_VIRTUAL ~Declaration();

    void swap(Declaration& rhs) noexcept;

public:
    DECL_CLONE_ALL_NOBASE(Declaration)

protected:
    SymRes resolveSymbols(Context& ctx);

public:
    SymRes resolveAttributes(Context& ctx);

public:
    DeclKind kind() const;
    Symbol& symbol();
    Symbol const& symbol() const;
    lexer::Token const& token() const;

    template <typename T> T* as() = delete;
    template <typename T> T const* as() const = delete;

public:
    Scope& scope();
    Scope const& scope() const;
    void setScope(Scope& parent);

    void setAttributes(std::vector<Box<Expression>>&& exprs);
    Slice<Statement const> attributes() const;
    Slice<Statement> attributes();

    codegen::CustomData* codegenData();
    codegen::CustomData* codegenData() const;
    void setCodegenData(Box<codegen::CustomData> data);
    void setCodegenData(Box<codegen::CustomData> data) const;

protected:
    DeclKind myKind;
    Box<Symbol> mySymbol;
    Scope* myScope = nullptr;
    std::vector<Statement> myAttributes;
    mutable Box<codegen::CustomData> myCodeGenData;
};

class DefinableDeclaration : public Declaration
{
public:
    explicit DefinableDeclaration(DeclKind kind, Symbol&& symbol);

protected:
    DefinableDeclaration(DefinableDeclaration const& rhs);

public:
    ~DefinableDeclaration() KYFOO_DEBUG_OVERRIDE;

    void swap(DefinableDeclaration& rhs) noexcept;

    // Declaration
public:
    DECL_CLONE_ALL(Declaration)

protected:
    SymRes resolveSymbols(Context& ctx);

public:
    Scope* definition();
    Scope const* definition() const;

protected:
    Scope* myDefinition = nullptr;
};

template <typename T>
class DefinableMixin : public DefinableDeclaration
{
protected:
    using DefinableDeclaration::DefinableDeclaration;

public:
    void define(T& scope)
    {
        myDefinition = &scope;
        myDefinition->setDeclaration(*this);
    }

    T* definition()
    {
        return static_cast<T*>(myDefinition);
    }

    T const* definition() const
    {
        return static_cast<T const*>(myDefinition);
    }
};

class VariableDeclaration;

class DataSumDeclaration : public DefinableMixin<DataSumScope>
{
public:
    using base_t = DefinableMixin;
    friend class Context;

public:
    class Constructor : public Declaration
    {
    public:
        friend class Context;

    public:
        Constructor(Symbol&& symbol,
                    std::vector<Box<VariableDeclaration>>&& pattern);

    protected:
        Constructor(Constructor const& rhs);
        Constructor& operator = (Constructor const& rhs);

    public:
        Constructor(Constructor&&) = delete;

        ~Constructor() KYFOO_DEBUG_OVERRIDE;

        void swap(Constructor& rhs) noexcept;

        // Declaration
    public:
        DECL_CLONE_ALL(Declaration)

    protected:
        SymRes resolveSymbols(Context& ctx);

    public:
        void setParent(DataSumDeclaration* parent);
        DataSumDeclaration* parent();
        DataSumDeclaration const* parent() const;

    private:
        DataSumDeclaration* myParent = nullptr;
        std::vector<Box<VariableDeclaration>> myPattern;
    };

public:
    explicit DataSumDeclaration(Symbol&& symbol);

protected:
    DataSumDeclaration(DataSumDeclaration const& rhs);
    void operator = (DataSumDeclaration const& rhs) = delete;

public:
    DataSumDeclaration(DataSumDeclaration&&) = delete;

    ~DataSumDeclaration() KYFOO_DEBUG_OVERRIDE;

    void swap(DataSumDeclaration& rhs) noexcept;

    // Declaration
public:
    DECL_CLONE_ALL(Declaration)

protected:
    SymRes resolveSymbols(Context& ctx);
};

class Binder : public Declaration
{
protected:
    Binder(DeclKind kind,
           Symbol&& symbol,
           Scope* scope,
           std::vector<Box<Expression>> constraints);

    Binder(DeclKind kind,
           Symbol&& symbol,
           Scope* scope,
           Expression const* type);

    Binder(Binder const& rhs);
    Binder& operator = (Binder const& rhs) = delete;

    Binder(Binder&&) = delete;

public:
    ~Binder() KYFOO_DEBUG_OVERRIDE;

    void swap(Binder& rhs) noexcept;

    // Declaration
public:
    DECL_CLONE_ALL(Declaration)

protected:
    SymRes resolveSymbols(Context& ctx);

public:
    void addConstraint(Box<Expression> c);
    void addConstraints(std::vector<Box<Expression>>&& exprs);

    Slice<Expression*> constraints();
    Slice<Expression const*> const constraints() const;

    Expression const* type() const;

private:
    std::vector<Box<Expression>> myConstraints;
    Expression const* myType = nullptr;
};

class DataProductDeclaration : public DefinableMixin<DataProductScope>
{
public:
    using base_t = DefinableMixin;
    friend class Context;

public:
    class Field : public Binder
    {
    public:
        friend class Context;

    public:
        Field(Symbol&& symbol,
              std::vector<Box<Expression>> constraints,
              Box<Expression> init);

    protected:
        Field(Field const& rhs);
        Field& operator = (Field const& rhs);

    public:
        Field(Field&&) = delete;

        ~Field() KYFOO_DEBUG_OVERRIDE;

        void swap(Field& rhs) noexcept;

        // Declaration
    public:
        DECL_CLONE_ALL(Declaration)

    protected:
        SymRes resolveSymbols(Context& ctx);

    public:
        void setParent(DataProductDeclaration* dpDecl);
        DataProductDeclaration* parent();
        DataProductDeclaration const* parent() const;

    private:
        DataProductDeclaration* myParent = nullptr;
        Box<Expression> myInitializer;
    };

public:
    explicit DataProductDeclaration(Symbol&& symbol);

protected:
    DataProductDeclaration(DataProductDeclaration const& rhs);
    DataProductDeclaration& operator = (DataProductDeclaration const& rhs);

public:
    DataProductDeclaration(DataProductDeclaration&&) = delete;

    ~DataProductDeclaration() KYFOO_DEBUG_OVERRIDE;

    void swap(DataProductDeclaration& rhs) noexcept;

    // Declaration
public:
    DECL_CLONE_ALL(Declaration)

protected:
    SymRes resolveSymbols(Context& ctx);
};

class SymbolDeclaration : public Declaration
{
public:
    friend class Context;

public:
    SymbolDeclaration(Symbol&& symbol,
                      Box<Expression> expression);

protected:
    SymbolDeclaration(SymbolDeclaration const& rhs);
    SymbolDeclaration& operator = (SymbolDeclaration const& rhs);

public:
    SymbolDeclaration(SymbolDeclaration&&) = delete;

    ~SymbolDeclaration() KYFOO_DEBUG_OVERRIDE;

    void swap(SymbolDeclaration& rhs) noexcept;

    // Declaration
public:
    DECL_CLONE_ALL(Declaration)

protected:
    SymRes resolveSymbols(Context& ctx);

public:
    Expression* expression();
    Expression const* expression() const;

private:
    Box<Expression> myExpression;
};

class VariableDeclaration : public Binder
{
public:
    friend class Context;

public:
    VariableDeclaration(Symbol&& symbol,
                        ProcedureScope& scope,
                        std::vector<Box<Expression>> constraints);

    VariableDeclaration(Symbol&& symbol,
                        ProcedureScope& scope,
                        Expression const& type);

protected:
    VariableDeclaration(VariableDeclaration const& rhs);
    VariableDeclaration& operator = (VariableDeclaration const& rhs);

public:
    VariableDeclaration(VariableDeclaration&&) = delete;

    ~VariableDeclaration() KYFOO_DEBUG_OVERRIDE;

    void swap(VariableDeclaration& rhs) noexcept;

    // Declaration
public:
    DECL_CLONE_ALL(Declaration)

protected:
    SymRes resolveSymbols(Context& ctx);
};

class ProcedureDeclaration;
class ProcedureParameter : public Binder
{
public:
    friend class Context;
    friend class ProcedureDeclaration;

public:
    ProcedureParameter(Symbol&& symbol,
                       ProcedureDeclaration& proc,
                       std::vector<Box<Expression>>&& constraints);

    ProcedureParameter(Symbol&& symbol,
                       ProcedureDeclaration& proc,
                       Expression const* type);

protected:
    ProcedureParameter(ProcedureParameter const& rhs);
    ProcedureParameter& operator = (ProcedureParameter const& rhs);

public:
    ProcedureParameter(ProcedureParameter&&) = delete;

    ~ProcedureParameter() KYFOO_DEBUG_OVERRIDE;

    void swap(ProcedureParameter& rhs) noexcept;

    // Declaration
public:
    DECL_CLONE_ALL(Declaration)

protected:
    SymRes resolveSymbols(Context& ctx);
};

class ProcedureDeclaration : public DefinableMixin<ProcedureScope>
{
public:
    using base_t = DefinableMixin;
    friend class Context;

public:
    ProcedureDeclaration(Symbol&& symbol,
                         Box<Expression> returnExpression);

protected:
    ProcedureDeclaration(ProcedureDeclaration const& rhs);
    ProcedureDeclaration& operator = (ProcedureDeclaration const& rhs);

public:
    ProcedureDeclaration(ProcedureDeclaration&&) = delete;

    ~ProcedureDeclaration() KYFOO_DEBUG_OVERRIDE;

    void swap(ProcedureDeclaration& rhs) noexcept;

    // Declaration
public:
    DECL_CLONE_ALL(Declaration)

protected:
    SymRes resolveSymbols(Context& ctx);

public:
    ArrowExpression const* type() const;

    Expression* returnType();
    Expression const* returnType() const;

    Slice<ProcedureParameter*> parameters();
    Slice<ProcedureParameter const*> parameters() const;

    ProcedureParameter* result();
    ProcedureParameter const* result() const;

    Slice<int const> ordinals() const;
    ProcedureParameter* findParameter(std::string_view token);
    ProcedureParameter const* findParameter(std::string_view token) const;

private:
    Box<ArrowExpression> myType;
    Box<Expression> myReturnExpression; // todo: merge with myResult

    std::vector<Box<ProcedureParameter>> myParameters;
    std::vector<int> myOrdinals;

    Box<ProcedureParameter> myResult;
};

class ImportDeclaration : public Declaration
{
public:
    friend class Context;

public:
    explicit ImportDeclaration(Symbol&& sym);
    explicit ImportDeclaration(std::vector<lexer::Token>&& modulePath);

protected:
    ImportDeclaration(ImportDeclaration const& rhs);
    ImportDeclaration& operator = (ImportDeclaration const& rhs);

public:
    ImportDeclaration(ImportDeclaration&&) = delete;

    ~ImportDeclaration() KYFOO_DEBUG_OVERRIDE;

    void swap(ImportDeclaration& rhs) noexcept;

    // Declaration
public:
    DECL_CLONE_ALL(Declaration)

protected:
    SymRes resolveSymbols(Context& ctx);

private:
    std::vector<lexer::Token> myModulePath;
};

class SymbolVariable : public Declaration
{
public:
    friend class Context;

public:
    SymbolVariable(IdentifierExpression const& id,
                   Scope* scope,
                   PatternsPrototype& prototype,
                   Expression const* bindExpr);
    SymbolVariable(IdentifierExpression const& id,
                   Scope* scope,
                   PatternsPrototype& prototype);

protected:
    SymbolVariable(SymbolVariable const& rhs);
    SymbolVariable& operator = (SymbolVariable const& rhs);

public:
    SymbolVariable(SymbolVariable&&) = delete;

    ~SymbolVariable() KYFOO_DEBUG_OVERRIDE;

    void swap(SymbolVariable& rhs) noexcept;

    // Declaration
public:
    DECL_CLONE_ALL(Declaration)

protected:
    SymRes resolveSymbols(Context& ctx);

public:
    PatternsPrototype const& prototype() const;
    lexer::Token const& token() const;

    void bindExpression(Expression const* expr);
    Expression const* boundExpression() const;

private:
    PatternsPrototype* myPrototype = nullptr;
    std::vector<Expression const*> myConstraints;
    Expression const* myBoundExpression = nullptr;
};

class TemplateDeclaration : public DefinableMixin<TemplateScope>
{
public:
    using base_t = DefinableMixin;
    friend class Context;

public:
    explicit TemplateDeclaration(Symbol&& sym);

protected:
    TemplateDeclaration(TemplateDeclaration const& rhs);
    TemplateDeclaration& operator = (TemplateDeclaration const& rhs);

public:
    TemplateDeclaration(TemplateDeclaration&&) = delete;

    ~TemplateDeclaration() KYFOO_DEBUG_OVERRIDE;

    void swap(TemplateDeclaration& rhs) noexcept;

    // Declaration
public:
    DECL_CLONE_ALL(Declaration)

protected:
    SymRes resolveSymbols(Context& ctx);

public:
    void merge(TemplateDeclaration& rhs);
};

// sugar to avoid switching on DeclKind
#define X(a,b,c) template<> inline c* Declaration::as<c>() { return myKind == DeclKind::a ? static_cast<c*>(this) : nullptr; }
    DECLARATION_KINDS(X)
#undef X
#define X(a,b,c) template<> inline c const* Declaration::as<c>() const { return myKind == DeclKind::a ? static_cast<c const*>(this) : nullptr; }
    DECLARATION_KINDS(X)
#undef X

inline Box<Declaration> beginClone(Declaration const& decl, clone_map_t& map)
{
    switch (decl.kind()) {
#define X(a,b,c) case DeclKind::a: return static_cast<c const&>(decl).beginClone(map);
    DECLARATION_KINDS(X)
#undef X
    }

    throw std::runtime_error("invalid declaration type");
}

inline void remap(Declaration& decl, clone_map_t const& map)
{
    switch (decl.kind()) {
#define X(a,b,c) case DeclKind::a: return static_cast<c&>(decl).remapReferences(map);
    DECLARATION_KINDS(X)
#undef X
    }

    throw std::runtime_error("invalid declaration type");
}

bool isDataDeclaration(DeclKind kind);
bool isBinder(DeclKind kind);
Binder const* getBinder(Declaration const& decl);
bool isCallableDeclaration(DeclKind kind);
bool isMacroDeclaration(DeclKind kind);
Expression const* getType(Declaration const& decl);
bool hasIndirection(DeclKind kind);
bool hasIndirection(Expression const& expr);
TemplateDeclaration const* parentTemplate(ProcedureDeclaration const& proc);
bool isDtor(ProcedureDeclaration const& proc);
bool isDefinableDeclaration(DeclKind kind);
DefinableDeclaration const* getDefinableDeclaration(Declaration const& decl);
DefinableDeclaration* getDefinableDeclaration(Declaration& decl);
Scope const* getDefinition(Declaration const& decl);
Scope* getDefinition(Declaration& decl);
void define(Declaration& decl, Scope& defn);

std::ostream& print(std::ostream& stream, Declaration const& decl);

    } // namespace ast
} // namespace kyfoo
