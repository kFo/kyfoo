#include <kyfoo/ast/ControlFlow.hpp>

#include <kyfoo/ast/Context.hpp>
#include <kyfoo/ast/Declarations.hpp>
#include <kyfoo/ast/Expressions.hpp>
#include <kyfoo/ast/Fabrication.hpp>
#include <kyfoo/ast/Scopes.hpp>
#include <kyfoo/ast/Semantics.hpp>

namespace kyfoo {
    namespace ast {

    namespace {
        const char* to_string(Statement::Kind kind)
        {
            switch ( kind ) {
#define X(a,b) case Statement::Kind::a: return #a;
            STATEMENT_KINDS(X)
#undef X
            }
            return "";
        }

        const char* to_string(Junction::Kind kind)
        {
            switch ( kind ) {
#define X(a,b) case Junction::Kind::a: return #a;
                JUNCTION_KINDS(X)
#undef X
            }
            return "";
        }

        BasicBlock* next(BasicBlock& bb)
        {
            auto s = bb.scope();
            auto blocks = s->basicBlocks();
            for ( std::size_t i = 0; i < blocks.size() - 1; ++i )
                if ( blocks[i] == &bb )
                    return blocks[i + 1];

            return nullptr;
        }

        BasicBlock* prev(BasicBlock& bb)
        {
            auto s = bb.scope();
            auto blocks = s->basicBlocks();
            if ( blocks.front() != &bb )
                for ( std::size_t i = 1; i < blocks.size(); ++i )
                    if ( blocks[i] == &bb )
                        return blocks[i - 1];

            return nullptr;
        }
    } // namespace

//
// Statement

Statement::Statement(std::unique_ptr<Expression> expr)
    : Statement(Kind::Expression, std::move(expr))
{
}

Statement::Statement(Kind kind, std::unique_ptr<Expression> expr)
    : myKind(kind)
    , myExpression(std::move(expr))
{
}

Statement::Statement(Statement const& rhs)
    : myKind(rhs.myKind)
{
}

Statement& Statement::operator = (Statement const& rhs)
{
    Statement(rhs).swap(*this);
    return *this;
}

Statement::Statement(Statement&& rhs)
    : myKind(rhs.myKind)
    , myExpression(std::move(rhs.myExpression))
    , myUnnamedVariables(std::move(rhs.myUnnamedVariables))
{
}

Statement& Statement::operator = (Statement&& rhs)
{
    new (this) Statement(std::move(rhs));
    rhs.~Statement();
    return *this;
}

Statement::~Statement() = default;

void Statement::swap(Statement& rhs)
{
    using std::swap;
    swap(myKind, rhs.myKind);
    swap(myExpression, rhs.myExpression);
    swap(myUnnamedVariables, rhs.myUnnamedVariables);
}

void Statement::io(IStream& stream) const
{
    stream.next("kind", to_string(myKind));
    stream.next("expr", myExpression);
}

IMPL_CLONE_NOBASE_BEGIN(Statement, Statement)
IMPL_CLONE_CHILD(myExpression)
IMPL_CLONE_CHILD(myUnnamedVariables)
IMPL_CLONE_CHILD(myAssignExpressions)
IMPL_CLONE_END
IMPL_CLONE_REMAP_NOBASE_BEGIN(Statement)
IMPL_CLONE_REMAP(myExpression)
IMPL_CLONE_REMAP(myUnnamedVariables)
IMPL_CLONE_REMAP(myAssignExpressions)
IMPL_CLONE_REMAP_END

SymRes Statement::resolveSymbols(Context& ctx)
{
    if ( !ctx.resolveExpression(myExpression) ) {
        ctx.error(*myExpression) << "unresolved symbols in sequence";
        return SymRes::Fail;
    }

    return SymRes::Success;
}

Statement::Kind Statement::kind() const
{
    return myKind;
}

Expression const& Statement::expression() const
{
    return *myExpression;
}

Expression& Statement::expression()
{
    return *myExpression;
}

Slice<VariableDeclaration const*> Statement::unnamedVariables() const
{
    return myUnnamedVariables;
}

Slice<AssignExpression const*> Statement::assignExpressions() const
{
    return myAssignExpressions;
}

VariableDeclaration const* Statement::createUnnamedVariable(ProcedureScope& scope,
                                                            Expression const& type)
{
    myUnnamedVariables.emplace_back(std::make_unique<VariableDeclaration>(Symbol(lexer::Token()), scope, type));
    return myUnnamedVariables.back().get();
}

VariableDeclaration const* Statement::appendUnnamedExpression(ProcedureScope& scope,
                                                              std::unique_ptr<Expression> expr)
{
    auto var = createUnnamedVariable(scope, *expr->type());
    myAssignExpressions.emplace_back(std::make_unique<AssignExpression>(*var, std::move(expr)));
    return var;
}

//
// Junction

Junction::Junction(Kind kind)
    : myKind(kind)
{
}

Junction::Junction(Junction const& rhs)
    : myKind(rhs.myKind)
{
}

Junction::~Junction() = default;

void Junction::swap(Junction& rhs)
{
    using std::swap;
    swap(myKind, rhs.myKind);
}

void Junction::io(IStream& stream) const
{
    stream.next("kind", to_string(myKind));
}

void Junction::cloneChildren(Junction&, clone_map_t&) const
{
    // nop
}

void Junction::remapReferences(clone_map_t const&)
{
    // nop
}

Junction::Kind Junction::kind() const
{
    return myKind;
}

//
// BranchJunction

BranchJunction::BranchJunction(lexer::Token const& token,
                                 std::unique_ptr<Expression> condition)
    : Junction(Kind::Branch)
    , myToken(token)
    , myCondition(std::make_unique<Statement>(std::move(condition)))
{
}

BranchJunction::BranchJunction(BranchJunction const& rhs)
    : Junction(rhs)
    , myToken(rhs.myToken)
    , myBranch{ rhs.myBranch[0], rhs.myBranch[1] }
{
}

BranchJunction& BranchJunction::operator = (BranchJunction const& rhs)
{
    BranchJunction(rhs).swap(*this);
    return *this;
}

BranchJunction::~BranchJunction() = default;

void BranchJunction::swap(BranchJunction& rhs)
{
    Junction::swap(rhs);
    using std::swap;
    swap(myToken, rhs.myToken);
    swap(myCondition, rhs.myCondition);
    swap(myBranch[0], rhs.myBranch[0]);
    swap(myBranch[1], rhs.myBranch[1]);
}

void BranchJunction::io(IStream& stream) const
{
    stream.next("condition", myCondition);
}

IMPL_CLONE_BEGIN(BranchJunction, Junction, Junction)
IMPL_CLONE_CHILD(myCondition)
IMPL_CLONE_END
IMPL_CLONE_REMAP_BEGIN(BranchJunction, Junction)
IMPL_CLONE_REMAP(myCondition)
IMPL_CLONE_REMAP(myBranch[0])
IMPL_CLONE_REMAP(myBranch[1])
IMPL_CLONE_REMAP_END

SymRes BranchJunction::resolveSymbols(Context& ctx, BasicBlock& bb)
{
    SymRes ret = SymRes::Success;
    if ( myCondition )
        ret |= ctx.resolveStatement(*myCondition);

    if ( !branch(0) ) {
        ctx.error(token()) << "is missing first branch";
        return SymRes::Fail;
    }
    else if ( !branch(1) ) {
        ctx.error(token()) << "is missing second branch";
        return SymRes::Fail;
    }

    branch(0)->appendIncoming(bb);
    branch(1)->appendIncoming(bb);

    return ret;
}

lexer::Token const& BranchJunction::token() const
{
    return myToken;
}

Statement const* BranchJunction::statement() const
{
    return myCondition.get();
}

Statement* BranchJunction::statement()
{
    return myCondition.get();
}

Expression const* BranchJunction::condition() const
{
    if ( myCondition )
        return &myCondition->expression();

    return nullptr;
}

Expression* BranchJunction::condition()
{
    if ( myCondition )
        return &myCondition->expression();

    return nullptr;
}

BasicBlock* BranchJunction::branch(std::size_t index)
{
    return myBranch[index];
}

BasicBlock const* BranchJunction::branch(std::size_t index) const
{
    return myBranch[index];
}

void BranchJunction::setBranch(std::size_t index, BasicBlock* bb)
{
    myBranch[index] = bb;
}

//
// ReturnJunction

ReturnJunction::ReturnJunction(lexer::Token const& token,
                               std::unique_ptr<Expression> expression)
    : Junction(Kind::Return)
    , myToken(token)
{
    if ( expression )
        myExpression = std::make_unique<Statement>(std::move(expression));
}

ReturnJunction::ReturnJunction(ReturnJunction const& rhs)
    : Junction(rhs)
{
}

ReturnJunction& ReturnJunction::operator = (ReturnJunction const& rhs)
{
    ReturnJunction(rhs).swap(*this);
    return *this;
}

ReturnJunction::~ReturnJunction() = default;

void ReturnJunction::swap(ReturnJunction& rhs)
{
    Junction::swap(rhs);
    using std::swap;
    swap(myExpression, rhs.myExpression);
}

void ReturnJunction::io(IStream& stream) const
{
    stream.next("expr", myExpression);
}

IMPL_CLONE_BEGIN(ReturnJunction, Junction, Junction)
IMPL_CLONE_CHILD(myExpression)
IMPL_CLONE_END
IMPL_CLONE_REMAP_BEGIN(ReturnJunction, Junction)
IMPL_CLONE_REMAP(myExpression)
IMPL_CLONE_REMAP_END

SymRes ReturnJunction::resolveSymbols(Context& ctx, BasicBlock& /*bb*/)
{
    if ( myExpression )
        return ctx.resolveStatement(*myExpression);

    return SymRes::Success;
}

lexer::Token const& ReturnJunction::token() const
{
    return myToken;
}

Statement const* ReturnJunction::statement() const
{
    return myExpression.get();
}

Statement* ReturnJunction::statement()
{
    return myExpression.get();
}

Expression const* ReturnJunction::expression() const
{
    if ( myExpression )
        return &myExpression->expression();

    return nullptr;
}

Expression* ReturnJunction::expression()
{
    if ( myExpression )
        return &myExpression->expression();

    return nullptr;
}

//
// JumpJunction

JumpJunction::JumpJunction(lexer::Token const& token,
                           JumpKind kind,
                           lexer::Token const& targetLabel)
    : Junction(Kind::Jump)
    , myToken(token)
    , myJumpKind(kind)
    , myTargetLabel(targetLabel)
{
}

JumpJunction::JumpJunction(JumpKind kind,
                           BasicBlock* target)
    : Junction(Kind::Jump)
    , myJumpKind(kind)
    , myTargetBlock(target)
{
}

JumpJunction::JumpJunction(JumpJunction const& rhs)
    : Junction(rhs)
    , myToken(rhs.myToken)
    , myJumpKind(rhs.myJumpKind)
    , myTargetLabel(rhs.myTargetLabel)
{
}

JumpJunction& JumpJunction::operator = (JumpJunction const& rhs)
{
    JumpJunction(rhs).swap(*this);
    return *this;
}

JumpJunction::~JumpJunction() = default;

void JumpJunction::swap(JumpJunction& rhs)
{
    Junction::swap(rhs);
    using std::swap;
    swap(myTargetLabel, rhs.myTargetLabel);
    swap(myTargetBlock, rhs.myTargetBlock);
}

void JumpJunction::io(IStream& stream) const
{
    stream.next("kind", myJumpKind == JumpKind::Loop ? "loop" : "break");
    stream.next("target", myTargetLabel);
}

IMPL_CLONE_BEGIN(JumpJunction, Junction, Junction)
IMPL_CLONE_END
IMPL_CLONE_REMAP_BEGIN(JumpJunction, Junction)
IMPL_CLONE_REMAP(myTargetBlock)
IMPL_CLONE_REMAP_END

SymRes JumpJunction::resolveSymbols(Context& ctx, BasicBlock& bb)
{
    if ( targetBlock() )
        return SymRes::Success;

    if ( myTargetLabel.kind() == lexer::TokenKind::Undefined ) {
        for ( auto p = bb.scope(); p; p = p->parent()->as<ProcedureScope>() ) {
            if ( p->isJumpTarget() ) {
                if ( jumpKind() == JumpKind::Break )
                    myTargetBlock = p->mergeBlock();
                else
                    myTargetBlock = p->basicBlocks().front();

                return SymRes::Success;
            }
        }

        ctx.error(token()) << "jump does not occur in a block";
        return SymRes::Fail;
    }

    for ( auto p = bb.scope(); p; p = p->parent()->as<ProcedureScope>() ) {
        if ( myTargetLabel.lexeme() == p->label().lexeme() ) {
            if ( jumpKind() == JumpKind::Break )
                myTargetBlock = p->mergeBlock();
            else
                myTargetBlock = p->basicBlocks().front();

            return SymRes::Success;
        }
    }

    ctx.error(myTargetLabel) << "block does not exist";
    return SymRes::Fail;
}

lexer::Token const& JumpJunction::token() const
{
    return myToken;
}

JumpJunction::JumpKind JumpJunction::jumpKind() const
{
    return myJumpKind;
}

lexer::Token const& JumpJunction::targetLabel() const
{
    return myTargetLabel;
}

BasicBlock const* JumpJunction::targetBlock() const
{
    return myTargetBlock;
}

BasicBlock* JumpJunction::targetBlock()
{
    return myTargetBlock;
}

//
// BasicBlock

BasicBlock::BasicBlock(ProcedureScope* scope)
    : myScope(scope)
{
}

BasicBlock::BasicBlock(BasicBlock const& rhs)
    : myScope(rhs.myScope)
    , myIncoming(rhs.myIncoming)
{
}

BasicBlock::~BasicBlock() = default;

void BasicBlock::swap(BasicBlock& rhs)
{
    using std::swap;
    swap(myScope, rhs.myScope);
    swap(myIncoming, rhs.myIncoming);
    swap(myStatements, rhs.myStatements);
    swap(myJunction, rhs.myJunction);
}

void BasicBlock::io(IStream& stream) const
{
    stream.openArray("statements");
    for ( auto const& e : myStatements ) {
        if ( auto exprStmt = e->as<Statement>() )
            stream.next("", exprStmt->expression());
    }
    stream.closeArray();
}

IMPL_CLONE_NOBASE_BEGIN(BasicBlock, BasicBlock)
IMPL_CLONE_CHILD(myStatements)
IMPL_CLONE_CHILD(myJunction)
IMPL_CLONE_END
IMPL_CLONE_REMAP_NOBASE_BEGIN(BasicBlock)
IMPL_CLONE_REMAP(myScope)
IMPL_CLONE_REMAP(myIncoming)
IMPL_CLONE_REMAP(myStatements)
IMPL_CLONE_REMAP(myJunction)
IMPL_CLONE_REMAP_END

SymRes BasicBlock::resolveSymbols(Context& ctx)
{
    SymRes ret = SymRes::Success;
    for ( auto& stmt : myStatements )
        ctx.resolveStatement(*stmt);

    if ( !junction() ) {
        ctx.error(*scope()->declaration()) << "expected terminating junction";
        return SymRes::Fail;
    }

    ret |= junction()->resolveSymbols(ctx, *this);
    return ret;
}

ProcedureScope const* BasicBlock::scope() const
{
    return myScope;
}

ProcedureScope* BasicBlock::scope()
{
    return myScope;
}

Slice<BasicBlock*> BasicBlock::incoming()
{
    return myIncoming;
}

Slice<Statement const*> BasicBlock::statements() const
{
    return myStatements;
}

Slice<Statement*> BasicBlock::statements()
{
    return myStatements;
}

Junction const* BasicBlock::junction() const
{
    return myJunction.get();
}

Junction* BasicBlock::junction()
{
    return myJunction.get();
}

bool BasicBlock::empty() const
{
    return myIncoming.empty()
        && myStatements.empty()
        && !myJunction;
}

void BasicBlock::appendIncoming(BasicBlock& from)
{
    myIncoming.push_back(&from);
}

void BasicBlock::append(std::unique_ptr<Expression> expr)
{
    myStatements.emplace_back(std::make_unique<Statement>(std::move(expr)));
}

void BasicBlock::setJunction(std::unique_ptr<Junction> junction)
{
    myJunction = std::move(junction);
}

codegen::CustomData* BasicBlock::codegenData()
{
    return myCodeGenData.get();
}

codegen::CustomData* BasicBlock::codegenData() const
{
    return myCodeGenData.get();
}

void BasicBlock::setCodegenData(std::unique_ptr<codegen::CustomData> data)
{
    if ( codegenData() )
        throw std::runtime_error("codegen data can only be set once");

    myCodeGenData = std::move(data);
}

void BasicBlock::setCodegenData(std::unique_ptr<codegen::CustomData> data) const
{
    return const_cast<BasicBlock*>(this)->setCodegenData(std::move(data));
}

    } // namespace ast
} // namespace kyfoo
