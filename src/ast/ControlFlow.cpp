#include <kyfoo/ast/ControlFlow.hpp>

#include <kyfoo/Algorithms.hpp>
#include <kyfoo/Utilities.hpp>

#include <kyfoo/ast/Context.hpp>
#include <kyfoo/ast/Declarations.hpp>
#include <kyfoo/ast/Expressions.hpp>
#include <kyfoo/ast/Fabrication.hpp>
#include <kyfoo/ast/Overloading.hpp>
#include <kyfoo/ast/Scopes.hpp>
#include <kyfoo/ast/Semantics.hpp>
#include <kyfoo/ast/Variance.hpp>
#include <kyfoo/ast/Visitors.hpp>

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
    } // namespace
#endif

//
// Statement

Statement::Statement(Kind kind)
    : myKind(kind)
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

Statement::Statement(Statement&& rhs) = default;

Statement& Statement::operator = (Statement&& rhs) = default;

Statement::~Statement() = default;

void Statement::swap(Statement& rhs) noexcept
{
    using kyfoo::swap;
    swap(myKind, rhs.myKind);
    swap(myTempVariables, rhs.myTempVariables);
    swap(myTempExpressions, rhs.myTempExpressions);
}

IMPL_CLONE_NOBASE_BEGIN(Statement, Statement)
IMPL_CLONE_CHILD(myTempVariables)
IMPL_CLONE_CHILD(myTempExpressions)
IMPL_CLONE_END
IMPL_CLONE_REMAP_NOBASE_BEGIN(Statement)
IMPL_CLONE_REMAP(myTempVariables)
IMPL_CLONE_REMAP(myTempExpressions)
IMPL_CLONE_REMAP_END

SymRes Statement::resolveSymbols(Context&)
{
    return SymRes::Success;
}

Statement::Kind Statement::kind() const
{
    return myKind;
}

Slice<VariableDeclaration const*> Statement::tempVariables() const
{
    return myTempVariables;
}

Slice<Expression const*> Statement::tempExpressions() const
{
    return myTempExpressions;
}

VariableDeclaration const& Statement::createTemp(ProcedureScope& scope,
                                                 Expression const& type,
                                                 Box<Expression> expr)
{
    myTempVariables.append(mk<VariableDeclaration>(Symbol(lexer::Token()), scope, type));
    myTempExpressions.append(std::move(expr));
    return *myTempVariables.back();
}

//
// ExpressionStatement

ExpressionStatement::ExpressionStatement(Box<Expression> expr)
    : Statement(Kind::Expression)
    , myExpression(std::move(expr))
{
}

ExpressionStatement::ExpressionStatement(ExpressionStatement const& rhs)
    : Statement(rhs)
{
}

ExpressionStatement& ExpressionStatement::operator = (ExpressionStatement const& rhs)
{
    ExpressionStatement(rhs).swap(*this);
    return *this;
}

ExpressionStatement::ExpressionStatement(ExpressionStatement&& rhs) = default;
ExpressionStatement& ExpressionStatement::operator = (ExpressionStatement&& rhs) = default;

ExpressionStatement::~ExpressionStatement() = default;

void ExpressionStatement::swap(ExpressionStatement& rhs) noexcept
{
    Statement::swap(rhs);
    using kyfoo::swap;
    swap(myExpression, rhs.myExpression);
}

IMPL_CLONE_BEGIN(ExpressionStatement, Statement, Statement)
IMPL_CLONE_CHILD(myExpression)
IMPL_CLONE_END
IMPL_CLONE_REMAP_BEGIN(ExpressionStatement, Statement)
IMPL_CLONE_REMAP(myExpression)
IMPL_CLONE_REMAP_END

SymRes ExpressionStatement::resolveSymbols(Context& ctx)
{
    if ( !ctx.resolveExpression(myExpression) ) {
        ctx.error(diag::unresolved_symbol, *myExpression);
        return SymRes::Fail;
    }

    return SymRes::Success;
}

Expression const& ExpressionStatement::expression() const
{
    return *myExpression;
}

Expression& ExpressionStatement::expression()
{
    return *myExpression;
}

void ExpressionStatement::changeExpression(Box<Expression> expr)
{
    myExpression = std::move(expr);
}

Box<Expression> ExpressionStatement::takeExpression()
{
    return std::move(myExpression);
}

//
// VariableStatement

VariableStatement::VariableStatement(VariableDeclaration& var, Box<Expression> expr)
    : Statement(Kind::Variable)
    , myVariable(&var)
    , myInitializer(std::move(expr))
{
}

VariableStatement::VariableStatement(VariableStatement const& rhs)
    : Statement(rhs)
    , myVariable(rhs.myVariable)
{
}

VariableStatement& VariableStatement::operator = (VariableStatement const& rhs)
{
    VariableStatement(rhs).swap(*this);
    return *this;
}

VariableStatement::VariableStatement(VariableStatement&& rhs) = default;

VariableStatement& VariableStatement::operator = (VariableStatement&& rhs) = default;

VariableStatement::~VariableStatement() = default;

void VariableStatement::swap(VariableStatement& rhs) noexcept
{
    Statement::swap(rhs);
    using kyfoo::swap;
    swap(myVariable, rhs.myVariable);
    swap(myInitializer, rhs.myInitializer);
}

IMPL_CLONE_BEGIN(VariableStatement, Statement, Statement)
IMPL_CLONE_CHILD(myInitializer)
IMPL_CLONE_END
IMPL_CLONE_REMAP_BEGIN(VariableStatement, Statement)
IMPL_CLONE_REMAP(myVariable)
IMPL_CLONE_REMAP(myInitializer)
IMPL_CLONE_REMAP_END

SymRes VariableStatement::resolveSymbols(Context& ctx)
{
    if ( !myInitializer ) {
        if ( !myVariable->type() ) {
            ctx.error(diag::no_type, *myVariable);
            return SymRes::Fail;
        }

        return SymRes::Success;
    }

    auto ret = ctx.resolveExpression(myInitializer);
    if ( !ret )
        return ret;

    if ( myVariable->type() ) {
        auto t = refType(*myVariable->type());
        ENFORCE(t, "variables must be typed as ref");

        if ( variance(ctx, *t, *myInitializer) )
            return ret;

        ProcedureDeclaration const* proc = findImplicitConversion(ctx, *t, *myInitializer);
        if ( proc ) {
            myInitializer = createApply(createIdentifier(*proc), std::move(myInitializer));
            ENFORCE(ctx.resolveExpression(myInitializer), "implicit conversion error");

            if ( variance(ctx, *t, *myInitializer) )
                return ret;
        }

        ctx.error(diag::no_conversion, *myVariable);
            // todo: conversion .directObject(*t);
        return SymRes::Fail;
    }

    myVariable->appendConstraint(clone(*myInitializer->type()));
    ret |= ctx.resolveDeclaration(*myVariable);
    if ( !ret )
        return ret;

    if ( !myVariable->type() ) {
        ctx.error(diag::no_type, *myVariable);
        return SymRes::Fail;
    }

    return ret;
}

VariableDeclaration const& VariableStatement::variable() const
{
    return *myVariable;
}

Expression const* VariableStatement::initializer() const
{
    return myInitializer.get();
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

BranchJunction::BranchJunction(lexer::Token token,
                               lexer::Token label,
                               Box<Expression> condition)
    : Junction(Kind::Branch)
    , myToken(std::move(token))
    , myLabel(std::move(label))
    , myCondition(mk<ExpressionStatement>(std::move(condition)))
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

    if ( !branch(1) )
        setBranch(1, branch(0)->scope()->mergeBlock());

    branch(1)->appendIncoming(bb);

    if ( !isMatch() ) {
        if ( !branch(0) ) {
            ctx.error(diag::no_branch, token());
            return SymRes::Fail;
        }

        branch(0)->appendIncoming(bb);
        return ret;
    }
    else if ( branch(0) ) {
        ctx.error(diag::no_match_initial_branch, token());
        return SymRes::Fail;
    }

    ab<BranchJunction*> branches;
    for ( auto br = branch(1)->junction()->as<BranchJunction>(); br; ) {
        ret |= ctx.resolveJunction(*br, bb);
        if ( !ret )
            continue;

        if ( br->isElse() )
            branches.append(br);

        if ( !br->branch(1) )
            break;

        br = br->branch(1)->junction()->as<BranchJunction>();
    }

    if ( !ret )
        return ret;

    if ( !branches ) {
        ctx.error(diag::no_match_branches, token());
        return SymRes::Fail;
    }

    auto dt = ast::as<DataTypeDeclaration>(condition()->type());
    if ( !dt ) {
        ctx.error(diag::expected_datatype, *condition());
        return SymRes::Fail;
    }

    auto defn = dt->definition();
    if ( !defn ) {
        ctx.error(diag::no_definition, *dt); // todo: wrong subj
        return SymRes::Fail;
    }

    if ( !defn->variations() ) {
        ctx.error(diag::no_match_datatype_variations, *dt); // todo: wrong subj
        return SymRes::Fail;
    }

    ab<DataTypeDeclaration const*> variationsFound(Replicate<DataTypeDeclaration const*>{defn->variations().card()});
    for ( auto br : branches ) {
        if ( !br->condition() ) {
            ctx.error(diag::no_match_initial_branch, br->token());
            ret |= SymRes::Fail;
            continue;
        }

        auto v = ast::as<DataTypeDeclaration>(br->condition());
        if ( !v ) {
            ctx.error(diag::expected_variation, *br->condition())
                .see(*dt);
            ret |= SymRes::Fail;
            continue;
        }

        auto const index = indexOf(defn->variations(), v);
        if ( index == defn->variations().card() ) {
            ctx.error(diag::expected_constructor, *v)
                .see(*dt);
            return SymRes::Fail;
        }

        variationsFound[index] = v;
    }

    if ( !ret )
        return ret;

    for ( uz i = 0; i < variationsFound.card(); ++i ) {
        if ( !variationsFound[i] ) {
            ctx.error(diag::missing_match_variation, token())
                .see(*defn->variations()[i]);
            ret |= SymRes::Fail;
        }
    }

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

bool BranchJunction::isMatch() const
{
    return myToken.kind() == lexer::TokenKind::ColonQuestion
        || myToken.kind() == lexer::TokenKind::ColonQuestionAngle;
}

bool BranchJunction::isElse() const
{
    return myToken.kind() == lexer::TokenKind::ColonSlash
        || myToken.kind() == lexer::TokenKind::ColonSlashAngle;
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

ReturnJunction::ReturnJunction(lexer::Token token,
                               Box<Expression> expression)
    : Junction(Kind::Return)
    , myToken(std::move(token))
    , myStatement(mk<ExpressionStatement>(std::move(expression)))
{
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
    swap(myStatement, rhs.myStatement);
}

IMPL_CLONE_BEGIN(ReturnJunction, Junction, Junction)
IMPL_CLONE_CHILD(myStatement)
IMPL_CLONE_END
IMPL_CLONE_REMAP_BEGIN(ReturnJunction, Junction)
IMPL_CLONE_REMAP(myStatement)
IMPL_CLONE_REMAP_END

SymRes ReturnJunction::resolveSymbols(Context& ctx, BasicBlock& /*bb*/)
{
    if ( myStatement )
        return ctx.resolveStatement(*myStatement);

    return SymRes::Success;
}

lexer::Token const& ReturnJunction::token() const
{
    return myToken;
}

ExpressionStatement const& ReturnJunction::statement() const
{
    return *myStatement;
}

ExpressionStatement& ReturnJunction::statement()
{
    return *myStatement;
}

Expression const& ReturnJunction::expression() const
{
    return myStatement->expression();
}

Expression& ReturnJunction::expression()
{
    return myStatement->expression();
}

//
// JumpJunction

JumpJunction::JumpJunction(lexer::Token token,
                           JumpKind kind,
                           lexer::Token targetLabel)
    : Junction(Kind::Jump)
    , myToken(std::move(token))
    , myJumpKind(std::move(kind))
    , myTargetLabel(targetLabel)
{
}

JumpJunction::JumpJunction(BasicBlock* target)
    : Junction(Kind::Jump)
    , myJumpKind(JumpKind::Continue)
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

        ctx.error(diag::no_block_jump, token());
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

    ctx.error(diag::no_block, myTargetLabel);
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
        if ( isUnit(*scope()->declaration()->returnType()) ) {
            lexer::Token tok;
            if ( myStatements )
                tok = front(*myStatements.back());
            setJunction(mk<ReturnJunction>(tok, createEmptyExpression(tok.location())));
        }
        else {
            ctx.error(diag::expected_terminal_junction, *scope()->declaration());
            return SymRes::Fail;
        }
    }

    ret |= ctx.resolveJunction(*junction(), *this);
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
    return !myIncoming
        && !myStatements
        && !myJunction;
}

void BasicBlock::appendIncoming(BasicBlock& from)
{
    if ( !scan(myIncoming, &from) )
        myIncoming.append(&from);
}

void BasicBlock::append(Box<Statement> stmt)
{
    myStatements.append(std::move(stmt));
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
    ENFORCE(!codegenData(), "codegen data can only be set once");

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

Slice<Extent::Block*> Extent::firstUses()
{
    return myFirstUses;
}

void Extent::appendBlock(BasicBlock const& bb)
{
    myBlocks.append(mk<Block>(&bb));

    // assumes graph construction begins at the entry
    if ( !myFirstUses )
        myFirstUses.append(myBlocks.back().get());
}

void Extent::appendUsage(BasicBlock const& bb, Statement const& stmt, Usage::Kind kind)
{
    ensureBlock(bb);
    myBlocks.back()->uses.append(Usage{&stmt, nullptr, kind});
}

void Extent::appendUsage(BasicBlock const& bb, Statement const& stmt, Expression const& expr, Usage::Kind kind)
{
    ensureBlock(bb);
    myBlocks.back()->uses.append(Usage{&stmt, &expr, kind});
}

void Extent::pruneEmptyBlocks()
{
    for ( auto b = begin(myBlocks); b != end(myBlocks); ) {
        if ( (*b)->uses ) {
            ++b;
            continue;
        }

        for ( auto p : (*b)->pred ) {
            for ( auto s : (*b)->succ ) {
                if ( !scan(p->succ, s) ) {
                    p->succ.append(s);
                    s->pred.append(p);
                }
            }

            p->succ.zap(b->get());
        }

        for ( auto s : (*b)->succ )
            s->pred.zap(b->get());

        if ( myFirstUses.zap(b->get()) ) {
            for ( auto s : (*b)->succ ) {
                if ( !scan(myFirstUses, s) )
                    myFirstUses.append(s);
            }
        }

        b = myBlocks.remove(b);
    }
}

SymRes Extent::cacheLocalFlows(Context& ctx)
{
    if ( !myBlocks ) {
        ctx.error(diag::unused, *myDeclaration);
        return SymRes::Fail;
    }

    for ( auto& b : myBlocks ) {
        bool hasValue = false;
        b->in = Requirement::None;
        for ( auto& u : b->uses ) {
            switch (u.kind) {
            case Usage::Read:
                if ( !hasValue )
                    b->in = Requirement::Defined;

                break;

            case Usage::Write:
            // liberal assumption that the ref defines it
            // todo: interprocedural analysis
            case Usage::Ref:
                hasValue = true;
                b->out = Provision::Defines;
                break;
            }
        }
    }

    // Live value propagation

    auto providesValue = [](Block* b) {
        return b->out == Provision::Defines;
    };

    // Parameters are considered as defined
    if ( auto param = myDeclaration->as<ProcedureParameter>() ) {
        for ( auto e : myFirstUses ) {
            e->in = Requirement::None;
            e->out = Provision::Defines;
        }
    }

    FlatSet<Block*> visited;
    ab<Block*> stack;
    ab<Block*> frontier;
    for ( auto entryBlock : myFirstUses ) {
        frontier.append(entryBlock);
        while ( frontier ) {
            stack.append(frontier.back());
            frontier.pop();

            auto b = stack.back();
            if ( auto [_, isNew] = visited.insert(b); !isNew )
                continue;

            auto const pv = providesValue(b);
            for ( auto s : b->succ ) {
                if ( pv && !providesValue(s) )
                    s->out = Provision::Defines;

                if ( scan(stack, s) ) {
                    // cycle -- skip
                    continue;
                }

                frontier.append(s);
            }
        }
    }

    // Live value check

    for ( auto& b : myBlocks ) {
        if ( b->in != Requirement::Defined )
            continue;

        for ( auto p : b->pred ) {
            if ( !providesValue(p) ) {
                auto err = ctx.error(diag::missing_control_flow_definition,
                    b->uses[0].expr ? Report::Subject(*b->uses[0].expr)
                                    : Report::Subject(*b->uses[0].stmt));
                if ( p->uses[0].expr )
                    err.see(*p->bb->scope(), *p->uses[0].expr);
                else
                    err.see(*myDeclaration);

                return SymRes::Fail;
            }
        }
    }

    return SymRes::Success;
}

void Extent::ensureBlock(BasicBlock const& bb)
{
    if ( !myBlocks || myBlocks.back()->bb != &bb )
        appendBlock(bb);
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
        myPath.append(br->branch(0) ? br->branch(0) : br->branch(1));
        return checkRepetition();
    }
    else if ( auto ret = bb->junction()->as<ReturnJunction>() ) {
        return None;
    }
    else if ( auto j = bb->junction()->as<JumpJunction>() ) {
        myPath.append(j->targetBlock());
        return checkRepetition();
    }

    ENFORCEU("invalid junction");
}

FlowTracer::Shape FlowTracer::advancePath()
{
    while ( myPath.card() > 1 ) {
        auto bb = myPath[myPath.card() - 1];
        auto pred = myPath[myPath.card() - 2];
        if ( auto br = pred->junction()->as<BranchJunction>() ) {
            if ( bb == br->branch(0) ) {
                myPath.back() = br->branch(1);
                return checkRepetition();
            }
        }

        myPath.pop();
    }

    return None;
}

Slice<BasicBlock const*> FlowTracer::currentPath() const
{
    return myPath;
}

FlowTracer::Shape FlowTracer::checkRepetition()
{
    auto bb = myPath.back();
    for ( auto i = myPath.card() - 2; ~i; --i )
        if ( myPath[i] == bb )
            return Repeat;

    return Forward;
}

//
// misc

template <typename Dispatcher>
struct Sequencer
{
    using Result = void;
    using ExtentSet = std::set<Extent*, ExtentCompare>;

    Dispatcher& dispatch;

    Context& ctx;
    ExtentSet& extents;
    BasicBlock const& basicBlock;
    Statement const* currentStmt = nullptr;
    bool refCtx = false;
    bool writeCtx = false;

    Sequencer(Dispatcher& dispatch,
              Context& ctx,
              ExtentSet& extents,
              BasicBlock const& bb)
        : dispatch(dispatch)
        , ctx(ctx)
        , extents(extents)
        , basicBlock(bb)
    {
    }

    Result recurse(Slice<Expression const*> exprs)
    {
        for ( auto e : exprs )
            dispatch(*e);
    }

    Extent::Usage::Kind currentUsage()
    {
        if ( writeCtx )
            return Extent::Usage::Write;

        if ( refCtx )
            return Extent::Usage::Ref;
        
        return Extent::Usage::Read;
    }

    Result stmtExpression(ExpressionStatement const& estmt)
    {
        currentStmt = &estmt;
        recurse(estmt.tempExpressions());
        return dispatch(estmt.expression());
    }

    Result stmtVariable(VariableStatement const& vstmt)
    {
        if ( auto expr = vstmt.initializer() ) {
            currentStmt = &vstmt;
            recurse(vstmt.tempExpressions());
            dispatch(*expr);

            if ( auto ext = extents.find(vstmt.variable()); ext != end(extents) )
                (*ext)->appendUsage(basicBlock, vstmt, Extent::Usage::Write);
        }
    }

    Result juncBranch(BranchJunction const& br)
    {
        if ( br.statement() ) {
            currentStmt = br.statement();
            return dispatch(*br.condition());
        }
    }

    Result juncReturn(ReturnJunction const& ret)
    {
        currentStmt = &ret.statement();
        return dispatch(ret.expression());
    }

    Result juncJump(JumpJunction const&)
    {
        // nop
    }

    Result exprLiteral(LiteralExpression const&)
    {
        // nop
    }

    Result exprIdentifier(IdentifierExpression const& id)
    {
        if ( auto d = id.declaration() ) {
            auto ext = extents.find(*d);
            if ( ext != extents.end() )
                (*ext)->appendUsage(basicBlock, *currentStmt, id, currentUsage());
        }
    }

    Result exprTuple(TupleExpression const& t)
    {
        recurse(t.expressions());
    }

    Result exprApply(ApplyExpression const& a)
    {
        check_point writeCtx;
        writeCtx = false;

        {
            check_point refCtx;
            refCtx = true;
            dispatch(*a.subject());
        }

        auto args = a.arguments();
        if ( auto proc = a.procedure() ) {
            auto o = proc->ordinals();
            auto p = proc->parameters();
            check_point refCtx;
            for ( uz i = 0; i < args.card(); ++i ) {
                refCtx = o[i] >= 0 && refType(*p[o[i]]->type());
                dispatch(*args[i]);
            }

            return;
        }

        recurse(args);
    }

    Result exprSymbol(SymbolExpression const& s)
    {
        check_point refCtx;
        check_point writeCtx;
        refCtx = writeCtx = false;

        recurse(s.expressions());
        exprIdentifier(s);
    }

    Result exprDot(DotExpression const& d)
    {
        {
            check_point writeCtx;
            writeCtx = false;

            check_point refCtx;
            refCtx = true;
            for ( auto m : d.expressions()(0, $ - 1) )
                dispatch(*m);
        }

        dispatch(*d.expressions().back());
    }

    Result exprAssign(AssignExpression const& v)
    {
        check_point writeCtx;
        check_point refCtx;

        dispatch(v.right());
        
        writeCtx = true;
        dispatch(v.left());
    }

    Result exprLambda(LambdaExpression const&)
    {
        // nop
    }

    Result exprArrow(ArrowExpression const& a)
    {
        check_point writeCtx;
        check_point refCtx;
        refCtx = writeCtx = false;

        dispatch(a.from());
        dispatch(a.to());
    }

    Result exprUniverse(UniverseExpression const&)
    {
        // nop
    }
};

SymRes buildVariableExtents(Context& ctx, ProcedureScope& proc, ab<Extent>& extents)
{
    // todo: crawl over declarations searching for uses
    /*for ( auto sym : declaration()->symbol().prototype().symbolVariables() )
        myExtents.emplace_back(*sym);*/

    for ( auto param : proc.declaration()->parameters() )
        extents.append(*param);

    ycomb(
        [&extents](auto rec, ProcedureScope const& scope) -> void {
            for ( auto d : scope.childDeclarations() )
                if ( d->kind() == Declaration::Kind::Variable )
                    extents.append(*d);

            for ( auto s : scope.childScopes() )
                rec(*s);
        })(proc);

    Sequencer<ShallowApply<Sequencer>>::ExtentSet extentSet;
    for ( auto& ext : extents )
        extentSet.insert(&ext);

    for ( FlowTracer trace(*proc.entryBlock()); ; ) {
        auto bb = trace.currentBlock();
        for ( auto& ext : extents )
            ext.appendBlock(*bb);

        ShallowApply<Sequencer> op(ctx, extentSet, *bb);
        for ( auto stmt : bb->statements() )
            op(*stmt);

        op(*bb->junction());

        if ( trace.advanceBlock() != FlowTracer::Forward ) {
            auto flow = trace.advancePath();
            while ( flow == FlowTracer::Repeat )
                flow = trace.advancePath();

            if ( flow == FlowTracer::None )
                break;
        }
    }

    // Flow connectivity
    for ( auto& ext : extents ) {
        for ( auto b : ext.blocks() ) {
            for ( auto incoming : b->bb->incoming() ) {
                for ( auto b2 : ext.blocks() ) {
                    if ( b2->bb == incoming ) {
                        b2->succ.append(b);
                        b->pred.append(b2);
                        break;
                    }
                }
            }
        }
    }

    for ( auto& ext : extents )
        ext.pruneEmptyBlocks();

    SymRes ret;
    for ( auto& ext : extents )
        ret |= ext.cacheLocalFlows(ctx);

    return ret;
}

} // namespace kyfoo::ast
