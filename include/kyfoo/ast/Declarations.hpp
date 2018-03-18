#pragma once

#include <memory>

#include <kyfoo/lexer/Token.hpp>

#include <kyfoo/ast/Node.hpp>
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

class DeclarationScope;
class DataSumScope;
class DataProductScope;
class Module;
class ProcedureScope;
class Statement;
class TemplateScope;
class ValueExpression;

class Declaration : public INode
{
public:
    friend class Context;

protected:
    Declaration(DeclKind kind,
                Symbol&& symbol,
                DeclarationScope* scope);

    Declaration(Declaration const& rhs);
    void operator = (Declaration const&) = delete;

    Declaration(Declaration&&) = delete;

public:
    ~Declaration();

    void swap(Declaration& rhs);

    // IIO
public:
    void io(IStream& stream) const override;

public:
    virtual Declaration* clone(clone_map_t& map) const = 0;
    virtual void cloneChildren(Declaration& c, clone_map_t& map) const;
    virtual void remapReferences(clone_map_t const& map);

protected:
    virtual SymRes resolveSymbols(Context& ctx) = 0;

public:
    void resolveAttributes(Context& ctx);

public:
    DeclKind kind() const;
    Symbol& symbol();
    Symbol const& symbol() const;
    lexer::Token const& token() const;

    template <typename T> T* as() = delete;
    template <typename T> T const* as() const = delete;

public:
    DeclarationScope& scope();
    DeclarationScope const& scope() const;
    void setScope(DeclarationScope& parent);

    void setAttributes(std::vector<std::unique_ptr<Expression>>&& exprs);
    Slice<Statement> const attributes() const;
    Slice<Statement> attributes();

    codegen::CustomData* codegenData();
    codegen::CustomData* codegenData() const;
    void setCodegenData(std::unique_ptr<codegen::CustomData> data);
    void setCodegenData(std::unique_ptr<codegen::CustomData> data) const;

protected:
    DeclKind myKind;
    std::unique_ptr<Symbol> mySymbol;
    DeclarationScope* myScope = nullptr;
    std::vector<Statement> myAttributes;
    mutable std::unique_ptr<codegen::CustomData> myCodeGenData;
};

class VariableDeclaration;

class DataSumDeclaration : public Declaration
{
public:
    class Constructor : public Declaration
    {
    public:
        Constructor(Symbol&& symbol,
                    std::vector<std::unique_ptr<VariableDeclaration>>&& pattern);

    protected:
        Constructor(Constructor const& rhs);
        Constructor& operator = (Constructor const& rhs);

    public:
        Constructor(Constructor&&) = delete;

        ~Constructor();

        void swap(Constructor& rhs);

        // IIO
    public:
        void io(IStream& stream) const override;

        // Declaration
    public:
        DECL_CLONE_ALL(Declaration)

    protected:
        SymRes resolveSymbols(Context& ctx) override;

    public:
        void setParent(DataSumDeclaration* parent);
        DataSumDeclaration* parent();
        DataSumDeclaration const* parent() const;

    private:
        DataSumDeclaration* myParent = nullptr;
        std::vector<std::unique_ptr<VariableDeclaration>> myPattern;
    };

public:
    explicit DataSumDeclaration(Symbol&& symbol);

protected:
    DataSumDeclaration(DataSumDeclaration const& rhs);
    void operator = (DataSumDeclaration const& rhs);

public:
    DataSumDeclaration(DataSumDeclaration&&) = delete;

    ~DataSumDeclaration();

    void swap(DataSumDeclaration& rhs);

    // IIO
public:
    void io(IStream& stream) const override;

    // Declaration
public:
    DECL_CLONE_ALL(Declaration)

protected:
    SymRes resolveSymbols(Context& ctx) override;

public:
    void define(DataSumScope* scope);
    DataSumScope* definition();
    DataSumScope const* definition() const;

private:
    DataSumScope* myDefinition = nullptr;
};

class Binder : public Declaration
{
protected:
    Binder(DeclKind kind,
           Symbol&& symbol,
           DeclarationScope* scope,
           std::vector<std::unique_ptr<Expression>> constraints);

    Binder(DeclKind kind,
           Symbol&& symbol,
           DeclarationScope* scope,
           Expression const* type);

    Binder(Binder const& rhs);
    Binder& operator = (Binder const& rhs) = delete;

    Binder(Binder&&) = delete;

public:
    ~Binder();

    void swap(Binder& rhs);

    // IIO
public:
    void io(IStream& stream) const override;

    // Declaration
public:
    DECL_CLONE_ALL(Declaration)

protected:
    SymRes resolveSymbols(Context& ctx) override;

public:
    void addConstraint(std::unique_ptr<Expression> c);
    void addConstraints(std::vector<std::unique_ptr<Expression>>&& exprs);

    Slice<Expression*> constraints();
    Slice<Expression*> const constraints() const;

    Expression const* type() const;

private:
    std::vector<std::unique_ptr<Expression>> myConstraints;
    Expression const* myType = nullptr;
};

class DataProductDeclaration : public Declaration
{
public:
    class Field : public Binder
    {
    public:
        Field(Symbol&& symbol,
              std::vector<std::unique_ptr<Expression>> constraints,
              std::unique_ptr<Expression> init);

    protected:
        Field(Field const& rhs);
        Field& operator = (Field const& rhs);

    public:
        Field(Field&&) = delete;

        ~Field();

        void swap(Field& rhs);

        // IIO
    public:
        void io(IStream& stream) const override;

        // Declaration
    public:
        DECL_CLONE_ALL(Declaration)

    protected:
        SymRes resolveSymbols(Context& ctx) override;

    public:
        void setParent(DataProductDeclaration* dpDecl);
        DataProductDeclaration* parent();
        DataProductDeclaration const* parent() const;

    private:
        DataProductDeclaration* myParent = nullptr;
        std::unique_ptr<Expression> myInitializer;
    };

public:
    explicit DataProductDeclaration(Symbol&& symbol);

protected:
    DataProductDeclaration(DataProductDeclaration const& rhs);
    DataProductDeclaration& operator = (DataProductDeclaration const& rhs);

public:
    DataProductDeclaration(DataProductDeclaration&&) = delete;

    ~DataProductDeclaration();

    void swap(DataProductDeclaration& rhs);

    // IIO
public:
    void io(IStream& stream) const override;

    // Declaration
public:
    DECL_CLONE_ALL(Declaration)

protected:
    SymRes resolveSymbols(Context& ctx) override;

public:
    void define(DataProductScope* scope);
    DataProductScope* definition();
    DataProductScope const* definition() const;

private:
    DataProductScope* myDefinition = nullptr;
};

class SymbolDeclaration : public Declaration
{
public:
    SymbolDeclaration(Symbol&& symbol,
                      std::unique_ptr<Expression> expression);

protected:
    SymbolDeclaration(SymbolDeclaration const& rhs);
    SymbolDeclaration& operator = (SymbolDeclaration const& rhs);

public:
    SymbolDeclaration(SymbolDeclaration&&) = delete;

    ~SymbolDeclaration();

    void swap(SymbolDeclaration& rhs);

    // IIO
public:
    void io(IStream& stream) const override;

    // Declaration
public:
    DECL_CLONE_ALL(Declaration)

protected:
    SymRes resolveSymbols(Context& ctx) override;

public:
    Expression* expression();
    Expression const* expression() const;

private:
    std::unique_ptr<Expression> myExpression;
};

class VariableDeclaration : public Binder
{
public:
    VariableDeclaration(Symbol&& symbol,
                        ProcedureScope& scope,
                        std::vector<std::unique_ptr<Expression>> constraints);

    VariableDeclaration(Symbol&& symbol,
                        ProcedureScope& scope,
                        Expression const& type);

protected:
    VariableDeclaration(VariableDeclaration const& rhs);
    VariableDeclaration& operator = (VariableDeclaration const& rhs);

public:
    VariableDeclaration(VariableDeclaration&&) = delete;

    ~VariableDeclaration();

    void swap(VariableDeclaration& rhs);

    // IIO
public:
    void io(IStream& stream) const override;

    // Declaration
public:
    DECL_CLONE_ALL(Declaration)

protected:
    SymRes resolveSymbols(Context& ctx) override;
};

class ProcedureDeclaration;
class ProcedureParameter : public Binder
{
public:
    friend class ProcedureDeclaration;

    enum PassSemantics
    {
        ByValue,
        ByReference,
    };

public:
    ProcedureParameter(Symbol&& symbol,
                       ProcedureDeclaration& proc,
                       std::vector<std::unique_ptr<Expression>>&& constraints,
                       PassSemantics passSemantics);

    ProcedureParameter(Symbol&& symbol,
                       ProcedureDeclaration& proc,
                       Expression const* type,
                       PassSemantics passSemantics);

protected:
    ProcedureParameter(ProcedureParameter const& rhs);
    ProcedureParameter& operator = (ProcedureParameter const& rhs);

public:
    ProcedureParameter(ProcedureParameter&&) = delete;

    ~ProcedureParameter();

    void swap(ProcedureParameter& rhs);

    // IIO
public:
    void io(IStream& stream) const override;

    // Declaration
public:
    DECL_CLONE_ALL(Declaration)

protected:
    SymRes resolveSymbols(Context& ctx) override;

public:
    PassSemantics passSemantics() const;

private:
    PassSemantics myPassSemantics = ByValue;
};

class ProcedureDeclaration : public Declaration
{
public:
    ProcedureDeclaration(Symbol&& symbol,
                         std::unique_ptr<Expression> returnExpression);

protected:
    ProcedureDeclaration(ProcedureDeclaration const& rhs);
    ProcedureDeclaration& operator = (ProcedureDeclaration const& rhs);

public:
    ProcedureDeclaration(ProcedureDeclaration&&) = delete;

    ~ProcedureDeclaration();

    void swap(ProcedureDeclaration& rhs);

    // IIO
public:
    void io(IStream& stream) const override;

    // Declaration
public:
    DECL_CLONE_ALL(Declaration)

protected:
    SymRes resolveSymbols(Context& ctx) override;

public:
    ProcedureScope* definition();
    ProcedureScope const* definition() const;
    void define(ProcedureScope* definition);

    Expression const* type() const;

    Expression* returnType();
    Expression const* returnType() const;

    Slice<ProcedureParameter*> parameters();
    Slice<ProcedureParameter*> const parameters() const;
    ProcedureParameter* result();
    ProcedureParameter const* result() const;

    int ordinal(std::size_t index) const;
    ProcedureParameter* findParameter(std::string const& token);
    ProcedureParameter const* findParameter(std::string const& token) const;

private:
    std::unique_ptr<ArrowExpression> myType;
    std::unique_ptr<Expression> myReturnExpression; // todo: merge with myResult

    std::vector<std::unique_ptr<ProcedureParameter>> myParameters;
    std::vector<int> myOrdinals;

    std::unique_ptr<ProcedureParameter> myResult;

    ProcedureScope* myDefinition = nullptr;
};

class ImportDeclaration : public Declaration
{
public:
    explicit ImportDeclaration(Symbol&& sym);
    explicit ImportDeclaration(std::vector<lexer::Token>&& modulePath);

protected:
    ImportDeclaration(ImportDeclaration const& rhs);
    ImportDeclaration& operator = (ImportDeclaration const& rhs);

public:
    ImportDeclaration(ImportDeclaration&&) = delete;

    ~ImportDeclaration();

    void swap(ImportDeclaration& rhs);

    // IIO
public:
    void io(IStream& stream) const override;

    // Declaration
public:
    DECL_CLONE_ALL(Declaration)

protected:
    SymRes resolveSymbols(Context& ctx) override;

private:
    std::vector<lexer::Token> myModulePath;
};

class SymbolVariable : public Declaration
{
public:
    SymbolVariable(IdentifierExpression const& id,
                   DeclarationScope* scope,
                   PatternsPrototype& prototype,
                   Expression const* bindExpr);
    SymbolVariable(IdentifierExpression const& id,
                   DeclarationScope* scope,
                   PatternsPrototype& prototype);

protected:
    SymbolVariable(SymbolVariable const& rhs);
    SymbolVariable& operator = (SymbolVariable const& rhs);

public:
    SymbolVariable(SymbolVariable&&) = delete;

    ~SymbolVariable();

    void swap(SymbolVariable& rhs);

    // IIO
public:
    void io(IStream& stream) const;

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
    std::vector<Expression*> myConstraints;
    Expression const* myBoundExpression = nullptr;
};

class TemplateDeclaration : public Declaration
{
public:
    explicit TemplateDeclaration(Symbol&& sym);

protected:
    TemplateDeclaration(TemplateDeclaration const& rhs);
    TemplateDeclaration& operator = (TemplateDeclaration const& rhs);

public:
    TemplateDeclaration(TemplateDeclaration&&) = delete;

    ~TemplateDeclaration();

    void swap(TemplateDeclaration& rhs);

    // IIO
public:
    void io(IStream& stream) const override;

    // Declaration
public:
    DECL_CLONE_ALL(Declaration)

protected:
    SymRes resolveSymbols(Context& ctx) override;

public:
    TemplateScope* definition();
    TemplateScope const* definition() const;
    void define(TemplateScope* definition);

    void merge(TemplateDeclaration& rhs);

private:
    TemplateScope* myDefinition = nullptr;
};

// sugar to avoid switching on DeclKind
#define X(a,b,c) template<> inline c* Declaration::as<c>() { return myKind == DeclKind::a ? static_cast<c*>(this) : nullptr; }
    DECLARATION_KINDS(X)
#undef X
#define X(a,b,c) template<> inline c const* Declaration::as<c>() const { return myKind == DeclKind::a ? static_cast<c const*>(this) : nullptr; }
    DECLARATION_KINDS(X)
#undef X

bool isDataDeclaration(DeclKind kind);
bool isBinder(DeclKind kind);
bool isCallableDeclaration(DeclKind kind);
bool isMacroDeclaration(DeclKind kind);
Expression const* getType(Declaration const& decl);
bool hasIndirection(DeclKind kind);
bool hasIndirection(Expression const& expr);
TemplateDeclaration const* parentTemplate(ProcedureDeclaration const& proc);
bool isCtor(ProcedureDeclaration const& proc);
bool isDtor(ProcedureDeclaration const& proc);

std::ostream& print(std::ostream& stream, Declaration const& decl);

    } // namespace ast
} // namespace kyfoo
