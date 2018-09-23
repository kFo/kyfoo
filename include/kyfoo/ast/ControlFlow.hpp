#pragma once

#include <memory>

#include <kyfoo/Slice.hpp>
#include <kyfoo/Types.hpp>
#include <kyfoo/Utilities.hpp>

#include <kyfoo/lexer/Token.hpp>
#include <kyfoo/ast/Clone.hpp>
#include <kyfoo/codegen/Codegen.hpp>

namespace kyfoo::ast {

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

class Statement
{
public:
    enum class Kind {
#define X(a,b) a,
        STATEMENT_KINDS(X)
#undef X
    };

    friend class Context;

public:
    explicit Statement(Box<Expression> expr);

protected:
    Statement(Kind kind, Box<Expression> expr);
    Statement(Statement const& rhs);
    Statement& operator = (Statement const& rhs);

public:
    Statement(Statement&& rhs);
    Statement& operator = (Statement&& rhs);

    ~Statement();
    void swap(Statement& rhs) noexcept;

public:
    DECL_CLONE_ALL_NOBASE(Statement)

protected:
    SymRes resolveSymbols(Context& ctx);

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
    Box<AssignExpression> appendUnnamedExpression(ProcedureScope& scope,
                                                              Box<Expression> expr);

private:
    Kind myKind;
    Box<Expression> myExpression;
    std::vector<Box<VariableDeclaration>> myUnnamedVariables;
    std::vector<AssignExpression const*> myAssignExpressions;
};

#define JUNCTION_KINDS(X)     \
    X(Branch, BranchJunction) \
    X(Return, ReturnJunction) \
    X(Jump  , JumpJunction  )

class Junction
{
public:
    friend class BasicBlock;

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
    
    void swap(Junction& rhs) noexcept;

public:
    KYFOO_DEBUG_VIRTUAL ~Junction();

public:
    DECL_CLONE_ALL_NOBASE(Junction)

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
    friend class BasicBlock;

public:
    BranchJunction(lexer::Token const& token,
                   lexer::Token const& label,
                   Box<Expression> condition);

protected:
    BranchJunction(BranchJunction const& rhs);
    BranchJunction& operator = (BranchJunction const& rhs);

public:
    ~BranchJunction() KYFOO_DEBUG_OVERRIDE;

    void swap(BranchJunction& rhs) noexcept;

    // Junction
public:
    DECL_CLONE_ALL(Junction)

protected:
    SymRes resolveSymbols(Context& ctx, BasicBlock& bb);

public:
    lexer::Token const& token() const;
    lexer::Token const& label() const;

    Statement const* statement() const;
    Statement* statement();

    Expression const* condition() const;
    Expression* condition();

    BasicBlock* branch(uz index);
    BasicBlock const* branch(uz index) const;
    void setBranch(uz index, BasicBlock* bb);

private:
    lexer::Token myToken;
    lexer::Token myLabel;
    Box<Statement> myCondition;
    BasicBlock* myBranch[2]{ nullptr };
};

class ReturnJunction : public Junction
{
public:
    friend class BasicBlock;

public:
    ReturnJunction(lexer::Token const& token,
                   Box<Expression> expression);

protected:
    ReturnJunction(ReturnJunction const& rhs);
    ReturnJunction& operator = (ReturnJunction const& rhs);

public:
    ~ReturnJunction() KYFOO_DEBUG_OVERRIDE;

    void swap(ReturnJunction& rhs) noexcept;

    // Junction
public:
    DECL_CLONE_ALL(Junction)

protected:
    SymRes resolveSymbols(Context& ctx, BasicBlock& bb);

public:
    lexer::Token const& token() const;

    Statement const* statement() const;
    Statement* statement();

    Expression const* expression() const;
    Expression* expression();

private:
    lexer::Token myToken;
    Box<Statement> myExpression;
};

class JumpJunction : public Junction
{
public:
    friend class BasicBlock;

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
    ~JumpJunction() KYFOO_DEBUG_OVERRIDE;

    void swap(JumpJunction& rhs) noexcept;

    // Junction
public:
    DECL_CLONE_ALL(Junction)

protected:
    SymRes resolveSymbols(Context& ctx, BasicBlock& bb);

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

class BasicBlock
{
public:
    explicit BasicBlock(ProcedureScope* scope);
    BasicBlock(BasicBlock const& rhs);
    ~BasicBlock();
    void swap(BasicBlock& rhs) noexcept;

public:
    DECL_CLONE_ALL_NOBASE(BasicBlock)

public:
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

    void append(Box<Expression> expr);

    void setJunction(Box<Junction> junction);
    
    codegen::CustomData* codegenData();
    codegen::CustomData* codegenData() const;
    void setCodegenData(Box<codegen::CustomData> data);
    void setCodegenData(Box<codegen::CustomData> data) const;

private:
    ProcedureScope* myScope = nullptr;

    std::vector<BasicBlock*> myIncoming;
    std::vector<Box<Statement>> myStatements;
    Box<Junction> myJunction;

    mutable Box<codegen::CustomData> myCodeGenData;
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
        Block(BasicBlock const* bb)
            : bb(bb)
        {
        }

        BasicBlock const* bb;
        std::vector<Usage> uses;
        std::vector<Block*> pred;
        std::vector<Block*> succ;
        Requirement in = Requirement::None;
        Provision out = Provision::None;
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
    std::vector<Box<Block>> myBlocks;
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

inline Box<Statement> beginClone(Statement const& stmt, clone_map_t& map)
{
    return stmt.beginClone(map);
}

inline void remap(Statement& stmt, clone_map_t const& map)
{
    return stmt.remapReferences(map);
}

#define X(a, b) template <> inline b* Junction::as<b>() { return myKind == Junction::Kind::a ? static_cast<b*>(this) : nullptr; }
JUNCTION_KINDS(X)
#undef X

#define X(a, b) template <> inline b const* Junction::as<b>() const { return myKind == Junction::Kind::a ? static_cast<b const*>(this) : nullptr; }
JUNCTION_KINDS(X)
#undef X

inline Box<Junction> beginClone(Junction const& junc, clone_map_t& map)
{
    switch (junc.kind()) {
#define X(a,b) case Junction::Kind::a: return static_cast<b const&>(junc).beginClone(map);
    JUNCTION_KINDS(X)
#undef X
    }

    throw std::runtime_error("invalid junction type");
}

inline void remap(Junction& junc, clone_map_t const& map)
{
    switch (junc.kind()) {
#define X(a,b) case Junction::Kind::a: return static_cast<b&>(junc).remapReferences(map);
    JUNCTION_KINDS(X)
#undef X
    }

    throw std::runtime_error("invalid junction type");
}

inline Box<BasicBlock> beginClone(BasicBlock const& bb, clone_map_t& map)
{
    return bb.beginClone(map);
}

inline void remap(BasicBlock& bb, clone_map_t const& map)
{
    return bb.remapReferences(map);
}

} // namespace kyfoo::ast
