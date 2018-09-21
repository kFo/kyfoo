#include <kyfoo/ast/ControlFlow.hpp>

#include <kyfoo/Utilities.hpp>

#include <kyfoo/ast/Context.hpp>
#include <kyfoo/ast/Declarations.hpp>
#include <kyfoo/ast/Expressions.hpp>
#include <kyfoo/ast/Fabrication.hpp>
#include <kyfoo/ast/Scopes.hpp>
#include <kyfoo/ast/Semantics.hpp>

namespace kyfoo::ast {

#if 0
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
            for ( uz i = 0; i < blocks.size() - 1; ++i )
                if ( blocks[i] == &bb )
                    return blocks[i + 1];

            return nullptr;
        }

        BasicBlock* prev(BasicBlock& bb)
        {
            auto s = bb.scope();
            auto blocks = s->basicBlocks();
            if ( blocks.front() != &bb )
                for ( uz i = 1; i < blocks.size(); ++i )
                    if ( blocks[i] == &bb )
                        return blocks[i - 1];

            return nullptr;
        }
    } // namespace
#endif

//
// Statement

Statement::Statement(Box<Expression> expr)
    : Statement(Kind::Expression, std::move(expr))
{
}

Statement::Statement(Kind kind, Box<Expression> expr)
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

void Statement::swap(Statement& rhs) noexcept
{
    using kyfoo::swap;
    swap(myKind, rhs.myKind);
    swap(myExpression, rhs.myExpression);
    swap(myUnnamedVariables, rhs.myUnnamedVariables);
}

IMPL_CLONE_NOBASE_BEGIN(Statement, Statement)
IMPL_CLONE_CHILD(myExpression)
IMPL_CLONE_CHILD(myUnnamedVariables)
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

    if ( auto ass = myExpression->as<AssignExpression>() ) {
        if ( auto id = ass->left().as<IdentifierExpression>() ) {
            if ( id->token().kind() == lexer::TokenKind::Undefined ) {
                auto e = find_if(begin(myUnnamedVariables), end(myUnnamedVariables),
                                 [&](auto const& rhs) { return rhs.get() == id->declaration(); });
                if ( e != end(myUnnamedVariables) ) {
                    myUnnamedVariables.erase(e);
                    myAssignExpressions.erase(find(begin(myAssignExpressions), end(myAssignExpressions),
                                                   ass));
                    myExpression = ass->takeRight();
                }
            }
        }
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
    myUnnamedVariables.emplace_back(mk<VariableDeclaration>(Symbol(lexer::Token()), scope, type));
    return myUnnamedVariables.back().get();
}

Box<AssignExpression> Statement::appendUnnamedExpression(ProcedureScope& scope,
                                                                     Box<Expression> expr)
{
    auto var = createUnnamedVariable(scope, *expr->type());
    auto ret = mk<AssignExpression>(*var, std::move(expr));
    myAssignExpressions.push_back(ret.get());
    return ret;
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

void Junction::swap(Junction& rhs) noexcept
{
    using kyfoo::swap;
    swap(myKind, rhs.myKind);
}

IMPL_CLONE_NOBASE_BEGIN(Junction, Junction)
IMPL_CLONE_END
IMPL_CLONE_REMAP_NOBASE_BEGIN(Junction)
IMPL_CLONE_REMAP_END

Junction::Kind Junction::kind() const
{
    return myKind;
}

//
// BranchJunction

BranchJunction::BranchJunction(lexer::Token const& token,
                               lexer::Token const& label,
                               Box<Expression> condition)
    : Junction(Kind::Branch)
    , myToken(token)
    , myLabel(label)
    , myCondition(mk<Statement>(std::move(condition)))
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

void BranchJunction::swap(BranchJunction& rhs) noexcept
{
    Junction::swap(rhs);
    using kyfoo::swap;
    swap(myToken, rhs.myToken);
    swap(myCondition, rhs.myCondition);
    swap(myBranch[0], rhs.myBranch[0]);
    swap(myBranch[1], rhs.myBranch[1]);
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

    if ( !branch(1) )
        setBranch(1, branch(0)->scope()->mergeBlock());

    branch(0)->appendIncoming(bb);
    branch(1)->appendIncoming(bb);

    return ret;
}

lexer::Token const& BranchJunction::token() const
{
    return myToken;
}

lexer::Token const& BranchJunction::label() const
{
    return myLabel;
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

BasicBlock* BranchJunction::branch(uz index)
{
    return myBranch[index];
}

BasicBlock const* BranchJunction::branch(uz index) const
{
    return myBranch[index];
}

void BranchJunction::setBranch(uz index, BasicBlock* bb)
{
    myBranch[index] = bb;
}

//
// ReturnJunction

ReturnJunction::ReturnJunction(lexer::Token const& token,
                               Box<Expression> expression)
    : Junction(Kind::Return)
    , myToken(token)
{
    if ( expression )
        myExpression = mk<Statement>(std::move(expression));
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

void ReturnJunction::swap(ReturnJunction& rhs) noexcept
{
    Junction::swap(rhs);
    using kyfoo::swap;
    swap(myExpression, rhs.myExpression);
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
    , myTargetBlock(rhs.myTargetBlock)
{
}

JumpJunction& JumpJunction::operator = (JumpJunction const& rhs)
{
    JumpJunction(rhs).swap(*this);
    return *this;
}

JumpJunction::~JumpJunction() = default;

void JumpJunction::swap(JumpJunction& rhs) noexcept
{
    Junction::swap(rhs);
    using kyfoo::swap;
    swap(myTargetLabel, rhs.myTargetLabel);
    swap(myTargetBlock, rhs.myTargetBlock);
}

IMPL_CLONE_BEGIN(JumpJunction, Junction, Junction)
IMPL_CLONE_END
IMPL_CLONE_REMAP_BEGIN(JumpJunction, Junction)
IMPL_CLONE_REMAP(myTargetBlock)
IMPL_CLONE_REMAP_END

SymRes JumpJunction::resolveSymbols(Context& ctx, BasicBlock& bb)
{
    if ( targetBlock() ) {
        targetBlock()->appendIncoming(bb);
        return SymRes::Success;
    }

    if ( myTargetLabel.kind() == lexer::TokenKind::Undefined ) {
        for ( auto p = bb.scope(); p; p = p->parent()->as<ProcedureScope>() ) {
            if ( p->isJumpTarget() ) {
                if ( jumpKind() == JumpKind::Break )
                    myTargetBlock = p->mergeBlock();
                else
                    myTargetBlock = p->basicBlocks().front();

                myTargetBlock->appendIncoming(bb);

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

            myTargetBlock->appendIncoming(bb);

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

void BasicBlock::swap(BasicBlock& rhs) noexcept
{
    using kyfoo::swap;
    swap(myScope, rhs.myScope);
    swap(myIncoming, rhs.myIncoming);
    swap(myStatements, rhs.myStatements);
    swap(myJunction, rhs.myJunction);
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
        ret |= ctx.resolveStatement(*stmt);

    if ( !junction() ) {
        ctx.error(*scope()->declaration()) << "expected terminating junction";
        return SymRes::Fail;
    }

    switch (junction()->kind()) {
#define X(a,b) case Junction::Kind::a: ret |= static_cast<b*>(junction())->resolveSymbols(ctx, *this); return ret;
        JUNCTION_KINDS(X)
#undef X
    }

    throw std::runtime_error("invalid junction");
}

ProcedureScope const* BasicBlock::scope() const
{
    return myScope;
}

ProcedureScope* BasicBlock::scope()
{
    return myScope;
}

Slice<BasicBlock const*> BasicBlock::incoming() const
{
    return myIncoming;
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
    if ( find(begin(myIncoming), end(myIncoming), &from) == end(myIncoming) )
        myIncoming.push_back(&from);
}

void BasicBlock::append(Box<Expression> expr)
{
    myStatements.emplace_back(mk<Statement>(std::move(expr)));
}

void BasicBlock::setJunction(Box<Junction> junction)
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

void BasicBlock::setCodegenData(Box<codegen::CustomData> data)
{
    if ( codegenData() )
        throw std::runtime_error("codegen data can only be set once");

    myCodeGenData = std::move(data);
}

void BasicBlock::setCodegenData(Box<codegen::CustomData> data) const
{
    return const_cast<BasicBlock*>(this)->setCodegenData(std::move(data));
}

//
// Extent

Extent::Extent(Declaration const& decl)
    : myDeclaration(&decl)
{
}

Declaration const& Extent::declaration() const
{
    return *myDeclaration;
}

Slice<Extent::Block const*> Extent::blocks() const
{
    return myBlocks;
}

Slice<Extent::Block*> Extent::blocks()
{
    return myBlocks;
}

void Extent::appendBlock(BasicBlock const& bb)
{
    myBlocks.emplace_back(mk<Block>(&bb));
}

void Extent::appendUsage(BasicBlock const& bb, Expression const& expr, Usage::Kind kind)
{
    if ( myBlocks.empty() || myBlocks.back()->bb != &bb )
        appendBlock(bb);

    myBlocks.back()->uses.push_back(Usage{&expr, kind});
}

void Extent::pruneEmptyBlocks()
{
    for ( auto b = begin(myBlocks); b != end(myBlocks); ) {
        if ( !(*b)->uses.empty() ) {
            ++b;
            continue;
        }

        for ( auto p : (*b)->pred ) {
            for ( auto s : (*b)->succ ) {
                if ( find(p->succ.begin(), p->succ.end(), s) == p->succ.end() ) {
                    p->succ.push_back(s);
                    s->pred.push_back(p);
                }
            }

            p->succ.erase(find(p->succ.begin(), p->succ.end(), b->get()));
        }

        for ( auto s : (*b)->succ )
            s->pred.erase(find(s->pred.begin(), s->pred.end(), b->get()));

        b = myBlocks.erase(b);
    }
}

SymRes Extent::cacheLocalFlows(Context& ctx)
{
    if ( myBlocks.empty() ) {
        ctx.error(*myDeclaration) << "declared but never used";
        return SymRes::Fail;
    }

    for ( auto& b : myBlocks ) {
        switch( b->uses[0].kind ) {
        case Usage::Read:
        case Usage::Move:
            b->in = Requirement::Defined;
            break;
        
        default:
            b->in = Requirement::None;
        }

        bool hasValue = true;
        auto provision = Provision::None;
        for ( uz i = 1; i < b->uses.size(); ++i ) {
            auto const& u = b->uses[i];
            switch (u.kind) {
            case Usage::Read:
                if ( !hasValue ) {
                    ctx.error(*u.expr) << "use of undefined value";
                    return SymRes::Fail;
                }
                break;

            case Usage::Write:
                hasValue = true;
                provision = Provision::Defines;
                break;

            case Usage::Ref:
                // liberal assumption that the ref defines it
                // todo: interprocedural analysis
                hasValue = true;
                provision = Provision::Refers;
                break;

            case Usage::Move:
                if ( !hasValue ) {
                    ctx.error(*u.expr) << "use of moved value";
                    return SymRes::Fail;
                }
                hasValue = false;
                provision = Provision::Moves;
                break;
            }
        }

        b->out = provision;
    }

    std::set<Extent::Block*> visited;
    std::vector<Extent::Block*> topo;
    topo.reserve(myBlocks.size());
    for ( auto& b : myBlocks ) {
        if ( b->pred.empty() )
            topo.push_back(b.get());
    }

    if ( auto param = myDeclaration->as<ProcedureParameter>() ) {
        for ( auto b : topo )
            b->in = Requirement::None;
    }

    for ( uz i = 0, len = topo.size(); i != len; ++i ) {
        for ( auto s : topo[i]->succ ) {
            auto iter = visited.lower_bound(s);
            if ( iter == end(visited) || *iter != s ) {
                visited.insert(iter, s);
                topo.push_back(s);
            }
        }

        len = topo.size();
    }

    for ( auto b : topo ) {
        if ( b->in == Requirement::Defined ) {
            for ( auto p : b->pred ) {
                if ( p->out != Provision::Defines && p->out != Provision::Refers ) {
                    if ( p == b ) {
                        if ( p->out == Provision::Moves ) {
                            ctx.error(*p->uses.back().expr) << "is not defined on all loop iterations";
                            return SymRes::Fail;
                        }

                        continue;
                    }

                    (ctx.error(*b->uses[0].expr) << "is not defined on all incoming paths")
                        .see(*p->bb->scope(), *p->uses[0].expr);
                    return SymRes::Fail;
                }
            }
        }

        if ( b->out == Provision::None ) {
            b->out = Provision::Defines;
        }
    }

    return SymRes::Success;
}

//
// FlowTracer

FlowTracer::FlowTracer(BasicBlock const& head)
    : myPath({ &head })
{
}

BasicBlock const* FlowTracer::currentBlock() const
{
    return myPath.back();
}

FlowTracer::Shape FlowTracer::advanceBlock()
{
    auto bb = myPath.back();
    if ( auto br = bb->junction()->as<BranchJunction>() ) {
        myPath.push_back(br->branch(0));
        return checkLoop();
    }
    else if ( auto ret = bb->junction()->as<ReturnJunction>() ) {
        return None;
    }
    else if ( auto j = bb->junction()->as<JumpJunction>() ) {
        myPath.push_back(j->targetBlock());
        return checkLoop();
    }

    throw std::runtime_error("invalid junction");
}

FlowTracer::Shape FlowTracer::advancePath()
{
    while ( myPath.size() > 1 ) {
        auto bb = myPath[myPath.size() - 1];
        auto pred = myPath[myPath.size() - 2];
        if ( auto br = pred->junction()->as<BranchJunction>() ) {
            if ( bb == br->branch(0) ) {
                myPath.back() = br->branch(1);
                return checkLoop();
            }
        }

        myPath.pop_back();
    }

    return None;
}

Slice<BasicBlock const*> FlowTracer::currentPath() const
{
    return myPath;
}

FlowTracer::Shape FlowTracer::checkLoop()
{
    auto bb = myPath.back();
    for ( auto i = myPath.size() - 2; ~i; --i )
        if ( myPath[i] == bb )
            return Loop;

    return Forward;
}

} // namespace kyfoo::ast
