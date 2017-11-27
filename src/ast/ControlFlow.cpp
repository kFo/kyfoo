#include <kyfoo/ast/ControlFlow.hpp>

#include <kyfoo/ast/Expressions.hpp>
#include <kyfoo/ast/Declarations.hpp>
#include <kyfoo/ast/Context.hpp>
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
IMPL_CLONE_END
IMPL_CLONE_REMAP_NOBASE_BEGIN(Statement)
IMPL_CLONE_REMAP(myExpression)
IMPL_CLONE_REMAP(myUnnamedVariables)
IMPL_CLONE_REMAP_END

void Statement::resolveSymbols(Context& ctx)
{
    ctx.resolveExpression(myExpression);
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

Slice<VariableDeclaration*> const Statement::unnamedVariables() const
{
    return myUnnamedVariables;
}

VariableDeclaration const* Statement::createUnnamed(ProcedureScope& scope, Declaration const& constraint)
{
    myUnnamedVariables.emplace_back(std::make_unique<VariableDeclaration>(scope, constraint));
    return myUnnamedVariables.back().get();
}

void Statement::appendUnnamed(ProcedureScope& scope, Expression const& expr)
{
    auto decl = resolveIndirections(expr.declaration());
    if ( !decl )
        throw std::runtime_error("unnamed instance must have a type");

    Declaration const* dt = decl;
    if ( auto proc = decl->as<ProcedureDeclaration>() ) {
        if ( isCtor(*proc) )
            dt = proc->parameters()[0]->dataType();
        else
            dt = proc->returnType()->declaration();
    }

    createUnnamed(scope, *dt);
}

//
// ConstructionStatement

ConstructionStatement::ConstructionStatement(std::unique_ptr<VarExpression> expr)
    : Statement(Kind::Construction, std::move(expr))
{
}

ConstructionStatement::ConstructionStatement(ConstructionStatement const& rhs)
    : Statement(rhs)
{
}

ConstructionStatement& ConstructionStatement::operator = (ConstructionStatement const& rhs)
{
    ConstructionStatement(rhs).swap(*this);
    return *this;
}

ConstructionStatement::~ConstructionStatement() = default;

void ConstructionStatement::swap(ConstructionStatement& rhs)
{
    Statement::swap(rhs);
}

void ConstructionStatement::io(IStream& stream) const
{
    Statement::io(stream);
}

IMPL_CLONE_BEGIN(ConstructionStatement, Statement, Statement)
IMPL_CLONE_END
IMPL_CLONE_REMAP_BEGIN(ConstructionStatement, Statement)
IMPL_CLONE_REMAP_END

void ConstructionStatement::resolveSymbols(Context& ctx)
{
    Statement::resolveSymbols(ctx);
}

VarExpression const& ConstructionStatement::varExpression() const
{
    return static_cast<VarExpression const&>(expression());
}

VarExpression& ConstructionStatement::varExpression()
{
    return static_cast<VarExpression&>(expression());
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

void BranchJunction::resolveSymbols(Context& ctx, BasicBlock& bb)
{
    if ( myCondition )
        ctx.resolveStatement(*myCondition);

    if ( !branch(0) ) {
        ctx.error(token()) << "is missing first branch";
        return;
    }
    else if ( !branch(1) ) {
        ctx.error(token()) << "is missing second branch";
        return;
    }

    branch(0)->appendIncoming(bb);
    branch(1)->appendIncoming(bb);
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

void ReturnJunction::resolveSymbols(Context& ctx, BasicBlock& /*bb*/)
{
    if ( myExpression )
        ctx.resolveStatement(*myExpression);
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

void JumpJunction::resolveSymbols(Context& ctx, BasicBlock& bb)
{
    if ( targetBlock() )
        return;

    if ( myTargetLabel.kind() == lexer::TokenKind::Undefined ) {
        if ( bb.incoming().empty() ) {
            ctx.error(token()) << "cannot jump outside a procedure-scope";
            return;
        }

        auto parentBlock = *bb.incoming().front();

        myTargetBlock = myJumpKind == JumpKind::Break ? &parentBlock : next(parentBlock);
        if ( !myTargetBlock ) {
            ctx.error(token()) << "missing merge block";
            return;
        }

        return;
    }

    for ( auto p = &bb; p; ) {
        if ( myTargetLabel.lexeme() == p->label().lexeme() ) {
            myTargetBlock = p;
            return;
        }

        if ( p->incoming().empty() ) {
            ctx.error(myTargetLabel) << "does not identify a jump label";
            return;
        }

        p = p->incoming().front();
    }
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

BasicBlock::BasicBlock(ProcedureScope* scope, lexer::Token const& label)
    : myScope(scope)
    , myLabel(label)
{
}

BasicBlock::BasicBlock(ProcedureScope* scope)
    : BasicBlock(scope, lexer::Token())
{
}

BasicBlock::BasicBlock(BasicBlock const& rhs)
    : myScope(rhs.myScope)
    , myLabel(rhs.myLabel)
    , myIncoming(rhs.myIncoming)
{
}

BasicBlock::~BasicBlock() = default;

void BasicBlock::swap(BasicBlock& rhs)
{
    using std::swap;
    swap(myScope, rhs.myScope);
    swap(myLabel, rhs.myLabel);
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

void BasicBlock::resolveSymbols(Context& ctx)
{
    for ( auto& stmt : myStatements )
        ctx.resolveStatement(*stmt);

    if ( !junction() ) {
        ctx.error(*scope()->declaration()) << "expected terminating junction";
        return;
    }

    junction()->resolveSymbols(ctx, *this);
}

ProcedureScope const* BasicBlock::scope() const
{
    return myScope;
}

ProcedureScope* BasicBlock::scope()
{
    return myScope;
}

lexer::Token const& BasicBlock::label() const
{
    return myLabel;
}

Slice<BasicBlock*> BasicBlock::incoming()
{
    return myIncoming;
}

Slice<Statement*> const BasicBlock::statements() const
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
        && !myJunction
        && myLabel.kind() == lexer::TokenKind::Undefined;
}

void BasicBlock::appendIncoming(BasicBlock& from)
{
    myIncoming.push_back(&from);
}

void BasicBlock::append(std::unique_ptr<Expression> expr)
{
    myStatements.emplace_back(std::make_unique<Statement>(std::move(expr)));
}

void BasicBlock::appendConstruction(std::unique_ptr<VarExpression> expr)
{
    myStatements.emplace_back(std::make_unique<ConstructionStatement>(std::move(expr)));
}

void BasicBlock::setJunction(std::unique_ptr<Junction> junction)
{
    myJunction = std::move(junction);
}

    } // namespace ast
} // namespace kyfoo
