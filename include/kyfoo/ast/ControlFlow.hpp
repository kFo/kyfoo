#pragma once

#include <memory>

#include <kyfoo/Slice.hpp>
#include <kyfoo/ast/Node.hpp>

#include <kyfoo/codegen/Codegen.hpp>

namespace kyfoo {
    namespace ast {

class BasicBlock;
class Context;
class Declaration;
class Expression;
class ProcedureScope;
class SymRes;
class AssignExpression;
class VariableDeclaration;

#define STATEMENT_KINDS(X)    \
    X(Expression  , Statement)

class Statement : public IIO
{
public:
    enum class Kind {
#define X(a,b) a,
        STATEMENT_KINDS(X)
#undef X
    };

    friend class Context;

public:
    explicit Statement(std::unique_ptr<Expression> expr);

protected:
    Statement(Kind kind, std::unique_ptr<Expression> expr);
    Statement(Statement const& rhs);
    Statement& operator = (Statement const& rhs);

public:
    Statement(Statement&& rhs);
    Statement& operator = (Statement&& rhs);

    ~Statement();
    void swap(Statement& rhs);

    // IIO
public:
    void io(IStream& stream) const override;

public:
    virtual Statement* clone(clone_map_t& map) const;
    virtual void cloneChildren(Statement& c, clone_map_t& map) const;
    virtual void remapReferences(clone_map_t const& map);

protected:
    virtual SymRes resolveSymbols(Context& ctx);

public:
    Kind kind() const;

    template <typename T> T* as() = delete;
    template <typename T> T const* as() const = delete;

    Expression const& expression() const;
    Expression& expression();

    Slice<VariableDeclaration const*> unnamedVariables() const;
    Slice<AssignExpression const*> assignExpressions() const;

    VariableDeclaration const* createUnnamedVariable(ProcedureScope& scope,
                                                     Expression const& type);
    std::unique_ptr<AssignExpression> appendUnnamedExpression(ProcedureScope& scope,
                                                              std::unique_ptr<Expression> expr);

private:
    Kind myKind;
    std::unique_ptr<Expression> myExpression;
    std::vector<std::unique_ptr<VariableDeclaration>> myUnnamedVariables;
    std::vector<AssignExpression const*> myAssignExpressions;
};

#define JUNCTION_KINDS(X)     \
    X(Branch, BranchJunction) \
    X(Return, ReturnJunction) \
    X(Jump  , JumpJunction  )

class Junction : public IIO
{
public:
    enum class Kind
    {
#define X(a,b) a,
        JUNCTION_KINDS(X)
#undef X
    };

protected:
    Junction(Kind kind);
    Junction(Junction const& rhs);
    Junction(Junction&&) = delete;
    
    void swap(Junction& rhs);

public:
    ~Junction();

public:
    void io(IStream& stream) const override;

    virtual Junction* clone(clone_map_t& map) const = 0;
    virtual void cloneChildren(Junction& c, clone_map_t& map) const;
    virtual void remapReferences(clone_map_t const& map);

    virtual SymRes resolveSymbols(Context& ctx, BasicBlock& bb) = 0;

public:
    Kind kind() const;

    template <typename T> T* as() = delete;
    template <typename T> T const* as() const = delete;

protected:
    Kind myKind;
};

class BranchJunction : public Junction
{
public:
    BranchJunction(lexer::Token const& token,
                   std::unique_ptr<Expression> condition);

protected:
    BranchJunction(BranchJunction const& rhs);
    BranchJunction& operator = (BranchJunction const& rhs);

public:
    ~BranchJunction();

    void swap(BranchJunction& rhs);

    // IIO
public:
    void io(IStream& stream) const override;

    // Junction
public:
    DECL_CLONE_ALL(Junction)

    SymRes resolveSymbols(Context& ctx, BasicBlock& bb) override;

public:
    lexer::Token const& token() const;

    Statement const* statement() const;
    Statement* statement();

    Expression const* condition() const;
    Expression* condition();

    BasicBlock* branch(std::size_t index);
    BasicBlock const* branch(std::size_t index) const;
    void setBranch(std::size_t index, BasicBlock* bb);

private:
    lexer::Token myToken;
    std::unique_ptr<Statement> myCondition;
    BasicBlock* myBranch[2]{ nullptr };
};

class ReturnJunction : public Junction
{
public:
    ReturnJunction(lexer::Token const& token,
                   std::unique_ptr<Expression> expression);

protected:
    ReturnJunction(ReturnJunction const& rhs);
    ReturnJunction& operator = (ReturnJunction const& rhs);

public:
    ~ReturnJunction();

    void swap(ReturnJunction& rhs);

    // IIO
public:
    void io(IStream& stream) const override;

    // Statement
public:
    DECL_CLONE_ALL(Junction)

    SymRes resolveSymbols(Context& ctx, BasicBlock& bb) override;

public:
    lexer::Token const& token() const;

    Statement const* statement() const;
    Statement* statement();

    Expression const* expression() const;
    Expression* expression();

private:
    lexer::Token myToken;
    std::unique_ptr<Statement> myExpression;
};

class JumpJunction : public Junction
{
public:
    enum class JumpKind
    {
        Loop,
        Break,
    };

public:
    JumpJunction(lexer::Token const& token,
                 JumpKind kind,
                 lexer::Token const& targetLabel);
    JumpJunction(JumpKind kind,
                 BasicBlock* target);

protected:
    JumpJunction(JumpJunction const& rhs);
    JumpJunction& operator = (JumpJunction const& rhs);

public:
    ~JumpJunction();

    void swap(JumpJunction& rhs);

    // IIO
public:
    void io(IStream& stream) const override;

    // Statement
public:
    DECL_CLONE_ALL(Junction)

    SymRes resolveSymbols(Context& ctx, BasicBlock& bb) override;

public:
    lexer::Token const& token() const;
    JumpKind jumpKind() const;
    lexer::Token const& targetLabel() const;

    BasicBlock const* targetBlock() const;
    BasicBlock* targetBlock();

private:
    lexer::Token myToken;
    JumpKind myJumpKind;
    lexer::Token myTargetLabel;

    BasicBlock* myTargetBlock = nullptr;
};

class BasicBlock : public IIO
{
public:
    explicit BasicBlock(ProcedureScope* scope);
    BasicBlock(BasicBlock const& rhs);
    ~BasicBlock();
    void swap(BasicBlock& rhs);

    // IIO
public:
    void io(IStream& stream) const override;

    BasicBlock* clone(clone_map_t& map) const;
    void cloneChildren(BasicBlock& c, clone_map_t& map) const;
    void remapReferences(clone_map_t const& map);

    SymRes resolveSymbols(Context& ctx);

public:
    ProcedureScope const* scope() const;
    ProcedureScope* scope();

    Slice<BasicBlock const*> incoming() const;
    Slice<BasicBlock*> incoming();

    Slice<Statement const*> statements() const;
    Slice<Statement*> statements();

    Junction const* junction() const;
    Junction* junction();

    bool empty() const;

    void appendIncoming(BasicBlock& from);

    void append(std::unique_ptr<Expression> expr);

    void setJunction(std::unique_ptr<Junction> junction);
    
    codegen::CustomData* codegenData();
    codegen::CustomData* codegenData() const;
    void setCodegenData(std::unique_ptr<codegen::CustomData> data);
    void setCodegenData(std::unique_ptr<codegen::CustomData> data) const;

private:
    ProcedureScope* myScope = nullptr;

    std::vector<BasicBlock*> myIncoming;
    std::vector<std::unique_ptr<Statement>> myStatements;
    std::unique_ptr<Junction> myJunction;

    mutable std::unique_ptr<codegen::CustomData> myCodeGenData;
};

class Extent
{
public:
    struct Usage {
        Expression const* expr;
        enum Kind {
            Read,
            Write,
            Ref,
            Move,
        } kind;
    };

    enum class Requirement {
        None,
        Defined,
    };

    enum class Provision {
        None,
        Defines,
        Moves,
        Refers,
    };

    struct Block {
        BasicBlock const* bb;
        std::vector<Usage> uses;
        std::vector<Block*> pred;
        std::vector<Block*> succ;
        Requirement in;
        Provision out;
    };

public:
    explicit Extent(Declaration const& decl);

public:
    Declaration const& declaration() const;
    Slice<Block const*> blocks() const;
    Slice<Block*> blocks();

public:
    void appendBlock(BasicBlock const& bb);
    void appendUsage(BasicBlock const& bb, Expression const& expr, Usage::Kind kind);

    void pruneEmptyBlocks();
    SymRes cacheLocalFlows(Context& ctx);

private:
    Declaration const* myDeclaration = nullptr;
    std::vector<std::unique_ptr<Block>> myBlocks;
};

class FlowTracer
{
public:
    enum Shape {
        None,
        Forward,
        Loop,
    };

public:
    explicit FlowTracer(BasicBlock const& head);

public:
    BasicBlock const* currentBlock() const;
    Shape advanceBlock();

    Shape advancePath();
    Slice<BasicBlock const*> currentPath() const;

private:
    Shape checkLoop();

private:
    std::vector<BasicBlock const*> myPath;
};

struct ExtentCompare {
    struct is_transparent;

    bool operator()(Extent const* lhs, Extent const* rhs) const {
        return &lhs->declaration() < &rhs->declaration();
    }

    bool operator()(Declaration const& lhs, Extent const* rhs) const {
        return &lhs < &rhs->declaration();
    }

    bool operator()(Extent const* lhs, Declaration const& rhs) const {
        return &lhs->declaration() < &rhs;
    }
};

#define X(a, b) template <> inline b* Statement::as<b>() { return myKind == Statement::Kind::a ? static_cast<b*>(this) : nullptr; }
STATEMENT_KINDS(X)
#undef X

#define X(a, b) template <> inline b const* Statement::as<b>() const { return myKind == Statement::Kind::a ? static_cast<b const*>(this) : nullptr; }
STATEMENT_KINDS(X)
#undef X

#define X(a, b) template <> inline b* Junction::as<b>() { return myKind == Junction::Kind::a ? static_cast<b*>(this) : nullptr; }
JUNCTION_KINDS(X)
#undef X

#define X(a, b) template <> inline b const* Junction::as<b>() const { return myKind == Junction::Kind::a ? static_cast<b const*>(this) : nullptr; }
JUNCTION_KINDS(X)
#undef X

    } // namespace ast
} // namespace kyfoo
