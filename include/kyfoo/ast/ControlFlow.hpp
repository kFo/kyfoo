#pragma once

#include <memory>

#include <kyfoo/FlatSet.hpp>
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

#define STATEMENT_KINDS(X)             \
    X(Expression, ExpressionStatement) \
    X(Variable  , VariableStatement  )

class Statement
{
public:
    enum class Kind {
#define X(a,b) a,
        STATEMENT_KINDS(X)
#undef X
    };

    friend class Context;

protected:
    explicit Statement(Kind kind);
    Statement(Statement const& rhs);
    Statement& operator = (Statement const& rhs);

public:
    Statement(Statement&& rhs);
    Statement& operator = (Statement&& rhs);

    KYFOO_DEBUG_VIRTUAL ~Statement();
    void swap(Statement& rhs) noexcept;

public:
    DECL_CLONE_ALL_NOBASE(Statement)

protected:
    SymRes resolveSymbols(Context& ctx);

public:
    Kind kind() const;

    template <typename T> T* as() = delete;
    template <typename T> T const* as() const = delete;

    Slice<VariableDeclaration const*> unnamedVariables() const;
    Slice<AssignExpression const*> assignExpressions() const;

    VariableDeclaration const* createUnnamedVariable(ProcedureScope& scope,
                                                     Expression const& type);
    Box<AssignExpression> appendUnnamedExpression(ProcedureScope& scope,
                                                  Box<Expression> expr);

protected:
    void flattenTopSubexpression(Box<Expression>& topExpr) noexcept;

protected:
    Kind myKind;
    std::vector<Box<VariableDeclaration>> myUnnamedVariables;
    std::vector<AssignExpression const*> myAssignExpressions;
};

class ExpressionStatement : public Statement
{
public:
    friend class Context;

public:
    explicit ExpressionStatement(Box<Expression> expr);

protected:
    ExpressionStatement(ExpressionStatement const& rhs);
    ExpressionStatement& operator = (ExpressionStatement const& rhs);

public:
    ExpressionStatement(ExpressionStatement&& rhs);
    ExpressionStatement& operator = (ExpressionStatement&& rhs);

    ~ExpressionStatement() KYFOO_DEBUG_OVERRIDE;
    void swap(ExpressionStatement& rhs) noexcept;

public:
    DECL_CLONE_ALL_NOBASE(Statement)

protected:
    SymRes resolveSymbols(Context& ctx);

public:
    Expression const& expression() const;
    Expression& expression();

public:
    void changeExpression(Box<Expression> expr);
    Box<Expression> takeExpression();

private:
    Box<Expression> myExpression;
};

class VariableStatement : public Statement
{
public:
    friend class Context;

public:
    VariableStatement(VariableDeclaration& var, Box<Expression> expr);

protected:
    VariableStatement(VariableStatement const& rhs);
    VariableStatement& operator = (VariableStatement const& rhs);

public:
    VariableStatement(VariableStatement&& rhs);
    VariableStatement& operator = (VariableStatement&& rhs);

    ~VariableStatement() KYFOO_DEBUG_OVERRIDE;
    void swap(VariableStatement& rhs) noexcept;

public:
    DECL_CLONE_ALL(Statement)

protected:
    SymRes resolveSymbols(Context& ctx);

public:
    VariableDeclaration const& variable() const;
    Expression const* initializer() const;

private:
    VariableDeclaration* myVariable = nullptr;
    Box<Expression> myInitializer;
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
    BranchJunction(lexer::Token token,
                   lexer::Token label,
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
    Box<ExpressionStatement> myCondition;
    BasicBlock* myBranch[2]{ nullptr };
};

class ReturnJunction : public Junction
{
public:
    friend class BasicBlock;

public:
    ReturnJunction(lexer::Token token,
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

    ExpressionStatement const& statement() const;
    ExpressionStatement& statement();

    Expression const& expression() const;
    Expression& expression();

private:
    lexer::Token myToken;
    Box<ExpressionStatement> myStatement;
};

class JumpJunction : public Junction
{
public:
    friend class BasicBlock;

    enum class JumpKind
    {
        Continue,
        Break,
    };

public:
    JumpJunction(lexer::Token token,
                 JumpKind kind,
                 lexer::Token targetLabel);
    JumpJunction(BasicBlock* target);

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
    friend class ProcedureScope;

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

    void append(Box<Statement> stmt);

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

    mutable FlatSet<BasicBlock*> myDominators;
    mutable BasicBlock* myImmediateDominator = nullptr;
    mutable Box<codegen::CustomData> myCodeGenData;
};

class Extent
{
public:
    struct Usage {
        Statement const* stmt;
        Expression const* expr;
        enum Kind {
            Read,
            Write,
            Ref,
        } kind;
    };

    enum class Requirement {
        None,
        Defined,
    };

    enum class Provision {
        None,
        Defines,
    };

    struct Block {
        explicit Block(BasicBlock const* bb)
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
    Slice<Block*> firstUses();

public:
    void appendBlock(BasicBlock const& bb);
    void appendUsage(BasicBlock const& bb, Statement const& stmt, Usage::Kind kind);
    void appendUsage(BasicBlock const& bb, Statement const& stmt, Expression const& expr, Usage::Kind kind);

    void pruneEmptyBlocks();
    SymRes cacheLocalFlows(Context& ctx);

private:
    void ensureBlock(BasicBlock const& bb);

private:
    Declaration const* myDeclaration = nullptr;
    std::vector<Box<Block>> myBlocks;
    std::vector<Block*> myFirstUses;
};

class FlowTracer
{
public:
    enum Shape {
        None,
        Forward,
        Repeat,
    };

public:
    explicit FlowTracer(BasicBlock const& head);

public:
    BasicBlock const* currentBlock() const;
    Shape advanceBlock();

    Shape advancePath();
    Slice<BasicBlock const*> currentPath() const;

private:
    Shape checkRepetition();

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
    switch (stmt.kind()) {
#define X(a,b) case Statement::Kind::a: return static_cast<b const&>(stmt).beginClone(map);
    STATEMENT_KINDS(X)
#undef X
    }

    throw std::runtime_error("invalid statement kind");
}

inline void remap(Statement& stmt, clone_map_t const& map)
{
    switch (stmt.kind()) {
#define X(a,b) case Statement::Kind::a: return static_cast<b&>(stmt).remapReferences(map);
    STATEMENT_KINDS(X)
#undef X
    }

    throw std::runtime_error("invalid statement kind");
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

SymRes buildVariableExtents(Context& ctx, ProcedureScope& proc, std::vector<Extent>& extents);

} // namespace kyfoo::ast
