#include <kyfoo/codegen/LLVM.hpp>

#include <experimental/filesystem>

#pragma warning(push, 0)
#include <llvm/ADT/APFloat.h>
#include <llvm/ADT/STLExtras.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Verifier.h>

#include <llvm/Support/FileSystem.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/TargetRegistry.h>
#include <llvm/Support/TargetSelect.h>

#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>
#pragma warning(pop)

#include <kyfoo/Diagnostics.hpp>

#include <kyfoo/lexer/Token.hpp>
#include <kyfoo/lexer/TokenKind.hpp>

#include <kyfoo/ast/Axioms.hpp>
#include <kyfoo/ast/Declarations.hpp>
#include <kyfoo/ast/Expressions.hpp>
#include <kyfoo/ast/Fabrication.hpp>
#include <kyfoo/ast/Module.hpp>
#include <kyfoo/ast/Scopes.hpp>
#include <kyfoo/ast/Semantics.hpp>

#include <kyfoo/codegen/Codegen.hpp>

namespace kyfoo {
    namespace codegen {

template <typename T>
struct LLVMCustomData : public CustomData
{
};

template<>
struct LLVMCustomData<ast::Module> : public CustomData
{
    std::unique_ptr<llvm::Module> module;
};

template<>
struct LLVMCustomData<ast::VariableDeclaration> : public CustomData
{
    llvm::AllocaInst* inst = nullptr;
};

template<>
struct LLVMCustomData<ast::ProcedureDeclaration> : public CustomData
{
    llvm::FunctionType* type = nullptr;
    llvm::Function* defn = nullptr;
    llvm::AllocaInst* returnInst = nullptr;
};

template<>
struct LLVMCustomData<ast::ProcedureParameter> : public CustomData
{
    llvm::Argument* arg = nullptr;
};

template<>
struct LLVMCustomData<ast::DataSumDeclaration> : public CustomData
{
    llvm::Type* type = nullptr;
};

template<>
struct LLVMCustomData<ast::DataProductDeclaration> : public CustomData
{
    llvm::StructType* type = nullptr;
};

template<>
struct LLVMCustomData<ast::DataProductDeclaration::Field> : public CustomData
{
    std::uint32_t index = 0;
};

template<>
struct LLVMCustomData<ast::Expression> : public CustomData
{
    ast::VariableDeclaration* tmp = nullptr;
};

template<>
struct LLVMCustomData<ast::BasicBlock> : public CustomData
{
    llvm::BasicBlock* bb = nullptr;
    llvm::BasicBlock* parentCleanup = nullptr;
};

template <typename T>
LLVMCustomData<T>* customData(T const& ast)
{
    return static_cast<LLVMCustomData<T>*>(ast.codegenData());
}

llvm::Type* toType(ast::Declaration const& decl)
{
    auto d = resolveIndirections(&decl);
    if ( auto ds = d->as<ast::DataSumDeclaration>() )
        return customData(*ds)->type;

    if ( auto dp = d->as<ast::DataProductDeclaration>() )
        return customData(*dp)->type;

    return nullptr;
}

llvm::Type* toType(llvm::LLVMContext& context, ast::Expression const& expr)
{
    auto e = resolveIndirections(&expr);
    if ( auto decl = getDeclaration(*e) )
        return toType(*decl);

    if ( auto t = e->as<ast::TupleExpression>() ) {
        if ( t->expressions().empty() && t->kind() == ast::TupleKind::Open )
            return llvm::Type::getVoidTy(context);

        llvm::Type* elementType = nullptr;
        if ( t->expressions().size() > 1 ) {
            std::vector<llvm::Type*> types;
            types.reserve(t->expressions().size());
            for ( auto const& te : t->expressions() )
                types.push_back(toType(context, *te));

            elementType = llvm::StructType::get(context, types);
        }
        else {
            elementType = toType(context, *t->expressions()[0]);
        }

        if ( t->elementsCount() > 1 )
            return llvm::ArrayType::get(elementType, t->elementsCount());

        return elementType;
    }

    if ( auto a = e->as<ast::ArrowExpression>() ) {
        enum { NotVarArg = false };
        std::vector<llvm::Type*> params;
        if ( auto tup = a->from().as<ast::TupleExpression>() ) {
            if ( tup->expressions().empty() )
                return llvm::PointerType::getUnqual(llvm::FunctionType::get(toType(context, a->to()), NotVarArg));

            if ( tup->kind() == ast::TupleKind::Open ) {
                params.reserve(tup->expressions().size());
                for ( auto texpr : tup->expressions() )
                    params.push_back(toType(context, *texpr));
            }
        }

        if ( params.empty() )
            params.push_back(toType(context, a->from()));

        return llvm::PointerType::getUnqual(llvm::FunctionType::get(toType(context, a->to()), params, NotVarArg));
    }

    return nullptr;
}

int log2(std::uint32_t n)
{
    int ret = 0;
    while ( n >>= 1 )
        ++ret;
    return ret;
}

std::uint32_t nextPower2(std::uint32_t n)
{
    --n;
    n |= n >> 1;
    n |= n >> 2;
    n |= n >> 4;
    n |= n >> 8;
    n |= n >> 16;
    ++n;

    return n;
}

//
// InitCodeGenPass

template <typename Dispatcher>
struct InitCodeGenPass
{
    using result_t = void;
    Dispatcher& dispatch;
    Diagnostics& dgn;
    ast::Module const& mod;

    ast::ProcedureDeclaration const* procContext = nullptr;

    InitCodeGenPass(Dispatcher& dispatch, Diagnostics& dgn, ast::Module const& mod)
        : dispatch(dispatch)
        , dgn(dgn)
        , mod(mod)
    {
    }

    result_t declDataSum(ast::DataSumDeclaration const& decl)
    {
        if ( !decl.symbol().prototype().isConcrete() || decl.codegenData() )
            return;

        decl.setCodegenData(std::make_unique<LLVMCustomData<ast::DataSumDeclaration>>());

        if ( auto defn = decl.definition() ) {
            for ( auto& e : defn->childDeclarations() )
                dispatch(*e);

            for ( auto& e : defn->childLambdas() )
                dispatch(*e);
        }
    }

    result_t declDataSumCtor(ast::DataSumDeclaration::Constructor const& decl)
    {
        if ( !decl.symbol().prototype().isConcrete() || decl.codegenData() )
            return;

        decl.setCodegenData(std::make_unique<LLVMCustomData<ast::DataSumDeclaration::Constructor>>());
    }

    result_t declDataProduct(ast::DataProductDeclaration const& decl)
    {
        if ( !decl.symbol().prototype().isConcrete() || decl.codegenData() )
            return;

        decl.setCodegenData(std::make_unique<LLVMCustomData<ast::DataProductDeclaration>>());

        if ( auto defn = decl.definition() ) {
            auto const& fields = defn->fields();
            for ( std::size_t i = 0; i < fields.size(); ++i ) {
                fields[i]->setCodegenData(std::make_unique<LLVMCustomData<ast::DataProductDeclaration::Field>>());
                customData(*fields[i])->index = static_cast<std::uint32_t>(i);
            }

            for ( auto& e : defn->childDeclarations() )
                dispatch(*e);

            for ( auto& e : defn->childLambdas() )
                dispatch(*e);
        }
    }

    result_t declField(ast::DataProductDeclaration::Field const&)
    {
        // nop
    }

    result_t declSymbol(ast::SymbolDeclaration const& s)
    {
        if ( s.expression() ) {
            auto decl = getDeclaration(s.expression());
            if ( decl )
                dispatch(*decl);
        }
    }

    result_t traceProc(ast::ProcedureScope const& scope)
    {
        for ( auto& e : scope.childDeclarations() )
            dispatch(*e);

        for ( auto& e : scope.childLambdas() )
            dispatch(*e);

        for ( auto const& e : scope.childScopes() )
            traceProc(*e);
    }

    result_t declProcedure(ast::ProcedureDeclaration const& decl)
    {
        if ( !decl.symbol().prototype().isConcrete() || decl.codegenData() )
            return;

        decl.setCodegenData(std::make_unique<LLVMCustomData<ast::ProcedureDeclaration>>());

        auto last = procContext;
        procContext = &decl;

        declProcedureParameter(*decl.result());
        for ( auto const& p : decl.parameters() )
            declProcedureParameter(*p);

        if ( auto defn = decl.definition() )
            traceProc(*defn);

        procContext = last;
    }

    result_t declProcedureParameter(ast::ProcedureParameter const& decl)
    {
        if ( !decl.codegenData() )
            decl.setCodegenData(std::make_unique<LLVMCustomData<ast::ProcedureParameter>>());
    }

    result_t declVariable(ast::VariableDeclaration const& decl)
    {
        if ( !decl.codegenData() )
            decl.setCodegenData(std::make_unique<LLVMCustomData<ast::VariableDeclaration>>());
    }

    result_t declImport(ast::ImportDeclaration const&)
    {
        // nop
    }

    result_t declSymbolVariable(ast::SymbolVariable const& sv)
    {
        if ( sv.boundExpression() )
            if ( auto decl = getDeclaration(sv.boundExpression()) )
                dispatch(*decl);
    }

    result_t declTemplate(ast::TemplateDeclaration const& decl)
    {
        if ( !decl.symbol().prototype().isConcrete() || decl.codegenData() )
            return;

        if ( auto defn = decl.definition() )
            for ( auto& e : defn->childDeclarations() )
                dispatch(*e);
    }
};

//
// CodeGenPass

template <typename Dispatcher>
struct CodeGenPass
{
    using result_t = void;
    Dispatcher& dispatch;

    Diagnostics& dgn;
    llvm::Module* module;
    ast::Module const& sourceModule;

    CodeGenPass(Dispatcher& dispatch,
                Diagnostics& dgn,
                llvm::Module* module,
                ast::Module const& sourceModule)
        : dispatch(dispatch)
        , dgn(dgn)
        , module(module)
        , sourceModule(sourceModule)
    {
    }

    result_t declDataSum(ast::DataSumDeclaration const&)
    {
        // todo
    }

    result_t declDataSumCtor(ast::DataSumDeclaration::Constructor const&)
    {
        // nop
    }

    result_t declDataProduct(ast::DataProductDeclaration const& decl)
    {
        if ( !decl.symbol().prototype().isConcrete() )
            return;

        // todo
        if ( auto defn = decl.definition() ) {
            for ( auto c : defn->childDeclarations() )
                dispatch(*c);

            for ( auto l : defn->childLambdas() )
                dispatch(*l);
        }
    }

    result_t declField(ast::DataProductDeclaration::Field const&)
    {
        // todo
    }

    result_t declSymbol(ast::SymbolDeclaration const&)
    {
        // nop
    }

    result_t declProcedure(ast::ProcedureDeclaration const& decl)
    {
        if ( !decl.symbol().prototype().isConcrete() )
            return;

        if ( isIntrinsic(decl) )
            return;

        auto fun = customData(decl);
        if ( fun->defn ) {
            error(decl.symbol().token()) << "defined more than once";
            die();
        }

        if ( fun->type )
            return;

        auto returnType = toType(*decl.returnType());
        if ( !returnType ) {
            error(decl.symbol().token()) << "cannot resolve return type";
            die();
        }

        std::vector<llvm::Type*> params;
        params.reserve(decl.parameters().size());
        for ( auto const& p : decl.parameters() ) {
            llvm::Type* paramType = toType(*p->type());
            if ( !paramType ) {
                error(p->symbol().token()) << "cannot resolve parameter type";
                die();
            }

            params.push_back(paramType);
        }

        fun->type = llvm::FunctionType::get(returnType, params, /*isVarArg*/false);

        if ( decl.scope().declaration() )
            fun->defn = llvm::Function::Create(fun->type,
                                               llvm::Function::ExternalLinkage,
                                               decl.scope().declaration()->symbol().token().lexeme(),
                                               module);
        else
            fun->defn = llvm::Function::Create(fun->type,
                                               llvm::Function::InternalLinkage,
                                               "",
                                               module);

        auto defn = decl.definition();
        if ( !defn )
            return;

        for ( auto d : defn->childDeclarations() )
            dispatch(*d);

        for ( auto l : defn->childLambdas() )
            dispatch(*l);

        {
            auto arg = fun->defn->arg_begin();
            for ( auto const& p : decl.parameters() )
                customData(*p)->arg = &*(arg++);
        }

        auto entryBlock = llvm::BasicBlock::Create(module->getContext(), "", fun->defn);
        llvm::IRBuilder<> builder(entryBlock);

        // todo: comply with calling convention
        if ( !returnType->isVoidTy() )
            fun->returnInst = builder.CreateAlloca(returnType);

        std::function<void(ast::ProcedureScope const&)> gatherAllocas =
        [&](ast::ProcedureScope const& scope) {
            for ( auto const& d : scope.childDeclarations() ) {
                if ( auto var = d->as<ast::VariableDeclaration>() ) {
                    customData(*var)->inst = builder.CreateAlloca(toType(*var->type()));
                }
            }

            auto gatherStatement = [this, &builder](ast::Statement const& stmt) {
                for ( auto const& v : stmt.unnamedVariables() ) {
                    auto vdata = std::make_unique<LLVMCustomData<ast::VariableDeclaration>>();
                    vdata->inst = builder.CreateAlloca(toType(*v->type()));
                    v->setCodegenData(std::move(vdata));
                }
            };

            for ( auto const& bb : scope.basicBlocks() ) {
                for ( auto const& stmt : bb->statements() )
                    gatherStatement(*stmt);

                if ( auto brJunc = bb->junction()->as<ast::BranchJunction>() ) {
                    if ( brJunc->statement() )
                        gatherStatement(*brJunc->statement());
                }
                else if ( auto retJunc = bb->junction()->as<ast::ReturnJunction>() ) {
                    if ( retJunc->statement() )
                        gatherStatement(*retJunc->statement());
                }
            }

            for ( auto const& s : scope.childScopes() )
                gatherAllocas(*s);
        };
        gatherAllocas(*defn);

        toBlock(*defn, *defn->basicBlocks().front(), fun->defn, entryBlock, nullptr);
        if ( llvm::verifyFunction(*fun->defn, &llvm::errs()) ) {
#ifndef NDEBUG
            fun->defn->dump();
#endif
            die("verify failure");
        }
    }

    void toBlock(ast::ProcedureScope const& scope,
                 ast::BasicBlock const& block,
                 llvm::Function* func,
                 llvm::BasicBlock* bb,
                 llvm::BasicBlock* parentCleanup)
    {
        auto bdata = customData(block);
        if ( !bdata ) {
            block.setCodegenData(std::make_unique<LLVMCustomData<ast::BasicBlock>>());
            bdata = customData(block);
        }
        bdata->bb = bb;
        bdata->parentCleanup = parentCleanup;

        auto fdata = customData(*scope.declaration());

        llvm::IRBuilder<> builder(bb);

        //std::size_t cleanupDeclLast = 0;
        //std::vector<llvm::BasicBlock*> cleanupBlocks;

        //auto createCleanupBlock = [&](lexer::Token const& token, bool exit) {
        //    auto const decls = scope.childDeclarations();
        //    std::vector<ast::VariableDeclaration*> priorVars;
        //    for ( ; cleanupDeclLast < decls.size(); ++cleanupDeclLast ) {
        //        if ( token.kind() != lexer::TokenKind::Undefined && !isBefore(decls[cleanupDeclLast]->symbol().token(), token) )
        //            break;

        //        if ( auto var = decls[cleanupDeclLast]->as<ast::VariableDeclaration>() )
        //            priorVars.push_back(var);
        //    }

        //    cleanupBlocks.push_back(llvm::BasicBlock::Create(module->getContext(), "", func));
        //    llvm::IRBuilder<> cleanupBuilder(cleanupBlocks.back());
        //    for ( auto v = priorVars.rbegin(); v != priorVars.rend(); ++v ) {
        //        if ( auto dp = (*v)->dataType()->as<ast::DataProductDeclaration>() ) {
        //            auto expr = createMemberCall(*dp->definition()->destructor(), **v);
        //            toValue(cleanupBuilder, *expr);
        //        }
        //    }

        //    if ( cleanupBlocks.size() == 1 ) {
        //        if ( exit && parentCleanup )
        //            cleanupBuilder.CreateBr(parentCleanup);
        //        else if ( !exit && merge )
        //            cleanupBuilder.CreateBr(merge);
        //        else if ( fdata->returnInst )
        //            cleanupBuilder.CreateRet(cleanupBuilder.CreateLoad(fdata->returnInst));
        //        else
        //            cleanupBuilder.CreateRetVoid();
        //    }
        //    else {
        //        cleanupBuilder.CreateBr(cleanupBlocks[cleanupBlocks.size() - 2]);
        //    }
        //};

        for ( auto const& stmt : block.statements() ) {
            for ( auto ass : stmt->assignExpressions() ) {
                if ( !toValue(builder, builder.getVoidTy(), *ass) ) {
                    error(*ass) << "invalid instruction";
                    die();
                }
            }

            if ( !toValue(builder, builder.getVoidTy(), stmt->expression()) ) {
                error(stmt->expression()) << "invalid instruction";
                die();
            }

            auto u = stmt->unnamedVariables();
            if ( u.length() ) {
                for ( std::size_t i = u.length() - 1; ~i; --i ) {
                    if ( auto dp = getDeclaration(u[i]->type())->as<ast::DataProductDeclaration>() ) {
                        auto expr = createMemberCall(*dp->definition()->destructor(), *u[i]);
                        toValue(builder, builder.getVoidTy(), *expr);
                    }
                }
            }
        }

        if ( auto retJunc = block.junction()->as<ast::ReturnJunction>() ) {
            if ( retJunc->expression() ) {
                auto retVal = toValue(builder, fdata->type->getReturnType(), *retJunc->expression());
                if ( fdata->returnInst )
                    builder.CreateStore(retVal, fdata->returnInst);
            }

            /*createCleanupBlock(retJunc->token(), true);
            builder.CreateBr(cleanupBlocks.back());*/
            if ( fdata->returnInst )
                builder.CreateRet(builder.CreateLoad(fdata->returnInst));
            else
                builder.CreateRetVoid();

            return;
        }

        if ( auto brJunc = block.junction()->as<ast::BranchJunction>() ) {
            auto cond = toValue(builder, nullptr, *brJunc->condition());
            auto cmp = builder.CreateICmpNE(cond, llvm::ConstantInt::get(cond->getType(), 0));

            auto trueBlock = llvm::BasicBlock::Create(module->getContext(), "", func);
            auto falseBlock = llvm::BasicBlock::Create(module->getContext(), "", func);

            builder.CreateCondBr(cmp, trueBlock, falseBlock);

            toBlock(scope, *brJunc->branch(0), func, trueBlock, nullptr /*cleanupBlocks.back()*/);
            toBlock(scope, *brJunc->branch(1), func, falseBlock, nullptr /*cleanupBlocks.back()*/);

            return;
        }

        if ( auto jmpJunc = block.junction()->as<ast::JumpJunction>() ) {
            if ( auto tdata = customData(*jmpJunc->targetBlock()) ) {
                builder.CreateBr(tdata->bb);
            }
            else {
                auto target = llvm::BasicBlock::Create(module->getContext(), "", func);
                toBlock(scope, *jmpJunc->targetBlock(), func, target, nullptr /*cleanupBlocks.back()*/);
                builder.CreateBr(target);
            }

            return;
        }

        die("missing procedure terminator");
    }

    result_t declProcedureParameter(ast::ProcedureParameter const&)
    {
        // nop
    }

    result_t declVariable(ast::VariableDeclaration const&)
    {
        // nop
    }

    result_t declImport(ast::ImportDeclaration const&)
    {
        // nop
    }

    result_t declSymbolVariable(ast::SymbolVariable const&)
    {
        // nop
    }

    result_t declTemplate(ast::TemplateDeclaration const& decl)
    {
        if ( !decl.symbol().prototype().isConcrete() )
            return;

        if ( auto defn = decl.definition() ) {
            for ( auto c : defn->childDeclarations() )
                dispatch(*c);

            for ( auto c : defn->childLambdas() )
                dispatch(*c);
        }
    }

private:
    Error& error()
    {
        return dgn.error(sourceModule) << "codegen: ";
    }

    Error& error(ast::Declaration const& decl)
    {
        return dgn.error(sourceModule, decl) << "codegen: ";
    }

    Error& error(ast::Expression const& expr)
    {
        return dgn.error(sourceModule, expr) << "codegen: ";
    }

    Error& error(lexer::Token const& token)
    {
        return dgn.error(sourceModule, token) << "codegen: ";
    }

    void die(const char* msg)
    {
        error() << msg;
        die();
    }

    void die()
    {
        dgn.die();
    }

    llvm::Type* toType(ast::Expression const& expr)
    {
        return codegen::toType(module->getContext(), expr);
    }

    llvm::Constant* toLiteral(llvm::IRBuilder<>& builder, llvm::Type* destType, lexer::Token const& token)
    {
        switch ( token.kind() ) {
        case lexer::TokenKind::Integer:
        {
            auto intType = llvm::dyn_cast_or_null<llvm::IntegerType>(destType);
            if ( !intType ) {
                auto bits = llvm::APInt::getBitsNeeded(token.lexeme(), 10);
                return builder.getInt(llvm::APInt(bits, token.lexeme(), 10));
            }

            return llvm::ConstantInt::get(intType, token.lexeme(), 10);
        }
        case lexer::TokenKind::Rational:
            return llvm::ConstantFP::get(destType, token.lexeme());
        case lexer::TokenKind::String:
        {
            auto const strType = customData(*sourceModule.axioms().intrinsic(ast::Sliceu8))->type;
            if ( destType != strType )
                return nullptr;

            auto const& str = sourceModule.interpretString(dgn, token);
            llvm::GlobalVariable* gv = builder.CreateGlobalString(str);
            auto zero = builder.getInt32(0);
            llvm::Constant* idx[] = { zero, zero };
            llvm::Constant* s[2] = {
                llvm::ConstantExpr::getInBoundsGetElementPtr(gv->getValueType(), gv, idx),
                builder.getInt64(str.length())
            };
            return llvm::ConstantStruct::get(strType, s);
        }
        }

        die("invalid literal");
        return nullptr;
    }

    llvm::Value* toRef(llvm::IRBuilder<>& builder, ast::Expression const& expr_)
    {
        auto expr = resolveIndirections(&expr_);
        if ( auto dot = expr->as<ast::DotExpression>() ) {
            llvm::Value* ret = nullptr;
            auto exprs = dot->expressions();
            for ( auto e : exprs ) {
                e = resolveIndirections(e);
                if ( auto decl = getDeclaration(e) ) {
                    if ( auto var = decl->as<ast::VariableDeclaration>() ) {
                        ret = customData(*var)->inst;
                        continue;
                    }

                    if ( auto param = decl->as<ast::ProcedureParameter>() ) {
                        ret = customData(*param)->arg;
                        continue;
                    }

                    if ( auto field = decl->as<ast::DataProductDeclaration::Field>() ) {
                        auto fdata = customData(*field);
                        ret = builder.CreateStructGEP(nullptr, ret, fdata->index);
                        continue;
                    }
                }
                else if ( auto lit = e->as<ast::LiteralExpression>() ) {
                    if ( lit->token().kind() != lexer::TokenKind::Integer )
                        die("bad index");

                    auto index = static_cast<unsigned>(std::atoi(lit->token().lexeme().c_str()));
                    ret = builder.CreateStructGEP(ret->getType()->getPointerElementType(),
                                                  ret,
                                                  index);
                    continue;
                }

                die("invalid dot expression");
            }

            return ret;
        }

        if ( auto decl = getDeclaration(expr) ) {
            if ( auto var = decl->as<ast::VariableDeclaration>() )
                return customData(*var)->inst;

            if ( auto param = decl->as<ast::ProcedureParameter>() )
                return customData(*param)->arg;
        }

        if ( auto app = expr->as<ast::ApplyExpression>() ) {
            if ( app->expressions().size() == 2
                && app->subject()->type()->kind() == ast::Expression::Kind::Tuple )
            {
                auto arr = toRef(builder, *app->subject());
                auto idx = toValue(builder, builder.getInt64Ty(), *app->expressions()[1]);
                llvm::Value* idxList[] = {
                    llvm::ConstantInt::get(idx->getType(), 0),
                    idx,
                };
                return builder.CreateGEP(arr, idxList);
            }
        }

        return nullptr;
    }

    llvm::Value* toValue(llvm::IRBuilder<>& builder, llvm::Type* destType, ast::Expression const& expression)
    {
        auto ret = toValue_impl(builder, destType, expression);
        if ( ret && ret->getType()->isPointerTy() && ret->getType()->getPointerElementType() == destType )
            return builder.CreateLoad(ret);

        return ret;
    }

    // todo: removeme
    llvm::Value* toValue_impl(llvm::IRBuilder<>& builder, llvm::Type* destType, ast::Expression const& expression)
    {
        auto const& axioms = sourceModule.axioms();
        auto const& expr = *resolveIndirections(&expression);
        if ( auto intrin = intrinsicInstruction(builder, destType, expr) )
            return intrin;

        if ( auto lit = expr.as<ast::LiteralExpression>() )
            return toLiteral(builder, destType, lit->token());

        if ( auto dot = expr.as<ast::DotExpression>() ) {
            llvm::Value* ret = nullptr;
            auto exprs = dot->expressions();
            for ( auto const& e : exprs ) {
                if ( auto decl = getDeclaration(e) ) {
                    if ( auto field = decl->as<ast::DataProductDeclaration::Field>() ) {
                        assert(ret && "expected field comes after another value");
                        auto fieldData = customData(*field);
                        if ( ret->getType()->isPointerTy() )
                            ret = builder.CreateLoad(builder.CreateStructGEP(nullptr, ret, fieldData->index));
                        else
                            ret = builder.CreateExtractValue(ret, fieldData->index);
                        continue;
                    }
                }
                else if ( auto lit = e->as<ast::LiteralExpression>() ) {
                    if ( lit->token().kind() != lexer::TokenKind::Integer )
                        die("bad index");

                    auto index = static_cast<unsigned>(std::atoi(lit->token().lexeme().c_str()));
                    if ( ret->getType()->isPointerTy() )
                        ret = builder.CreateLoad(builder.CreateStructGEP(nullptr, ret, index));
                    else
                        ret = builder.CreateExtractValue(ret, index);
                    continue;
                }

                ret = toValue(builder, destType, *e);
            }

            return ret;
        }

        if ( auto id = identify(expr) ) {
            auto decl = id->declaration();
            if ( !decl ) {
                auto d = resolveIndirections(getDeclaration(*id->type()));
                if ( d == axioms.intrinsic(ast::PointerNullLiteralType) ) {
                    auto ptrType = llvm::dyn_cast_or_null<llvm::PointerType>(destType);
                    return llvm::ConstantPointerNull::get(ptrType);
                }

                die("unresolved identifier");
            }

            if ( auto dsCtor = decl->as<ast::DataSumDeclaration::Constructor>() )
                die("dsctor not implemented");

            if ( auto param = decl->as<ast::ProcedureParameter>() ) {
                auto pdata = customData(*param);
                return pdata->arg;
            }

            if ( auto var = decl->as<ast::VariableDeclaration>() ) {
                auto vdata = customData(*var);
                // todo: removeme
                if ( destType && destType->isPointerTy() )
                    return vdata->inst;

                return builder.CreateLoad(vdata->inst);
            }

            if ( auto proc = decl->as<ast::ProcedureDeclaration>() ) {
                auto pdata = customData(*proc);
                return builder.CreateCall(pdata->defn);
            }

            die("unhandled identifier");
        }

        if ( auto a = expr.as<ast::ApplyExpression>() ) {
            auto const& exprs = a->expressions();
            auto proc = a->procedure();
            llvm::Value* fptr = nullptr;
            if ( !proc ) {
                proc = getProcedure(*a->subject());
                if ( !proc )
                    fptr = toValue(builder, nullptr, *exprs[0]);
            }

            ast::ArrowExpression const* arrow = nullptr;
            if ( proc ) {
                arrow = proc->type();
            }
            else {
                if ( auto decl = getDeclaration(resolveIndirections(*exprs[0])) ) {
                    if ( auto binder = getBinder(*decl) )
                        arrow = binder->type()->as<ast::ArrowExpression>();
                }
            }

            assert(arrow);

            auto formalParams = slice(&arrow->from());
            if ( auto tup = arrow->from().as<ast::TupleExpression>() )
                if ( tup->kind() == ast::TupleKind::Open )
                    formalParams = tup->expressions();

            std::vector<llvm::Value*> args;
            args.reserve(exprs.size());

            for ( std::size_t i = 1; i < exprs.size(); ++i ) {
                auto paramType = toType(*formalParams[i - 1]);
                auto val = toValue(builder, paramType, *exprs[i]);
                if ( !val )
                    die("cannot determine arg");
                args.push_back(val);
            }

            if ( proc )
                return builder.CreateCall(customData(*proc)->defn, args);

            return builder.CreateCall(fptr, args);
        }

        if ( auto assign = expr.as<ast::AssignExpression>() ) {
            auto ref = toRef(builder, assign->left());
            if ( !ref ) {
                ref = toValue(builder, nullptr, assign->left());
                if ( !ref )
                    die("cannot determine reference");
            }

            builder.CreateStore(toValue(builder, ref->getType()->getPointerElementType(), assign->right()),
                                ref);
            return builder.CreateLoad(ref);
        }

        if ( auto l = expr.as<ast::LambdaExpression>() ) {
            auto pdata = customData(l->procedure());
            return pdata->defn;
        }

        return nullptr;
    }

    bool isIntrinsic(ast::ProcedureDeclaration const& proc)
    {
        if ( sourceModule.axioms().isIntrinsic(proc) )
            return true;

        if ( auto pdecl = proc.scope().declaration() )
            return pdecl == sourceModule.axioms().intrinsic(ast::Sliceu8);

        return false;
    }

    llvm::Value* intrinsicInstruction(llvm::IRBuilder<>& builder,
                                      llvm::Type* destType,
                                      ast::Expression const& expr)
    {
        auto a = expr.as<ast::ApplyExpression>();
        if ( !a )
            return nullptr;

        auto subj = resolveIndirections(a->subject());
        if ( auto tup = subj->type()->as<ast::TupleExpression>() ) {
            if ( a->arguments().size() != 1 )
                die("expected one argument");

            auto proc = getProcedure(*a->arguments()[0]);
            if ( !proc )
                die("expected procedure argument");

            if ( tup->elementsCount() <= 1 )
                die("apply is not implemented for tuples");

            auto arr = toRef(builder, *subj);
            auto idxPtr = llvm::IRBuilder<>(&builder.GetInsertBlock()->getParent()->getEntryBlock()).CreateAlloca(builder.getInt64Ty());
            auto const zero = llvm::ConstantInt::get(builder.getInt64Ty(), 0);
            auto const one = llvm::ConstantInt::get(builder.getInt64Ty(), 1);
            auto const size = llvm::ConstantInt::get(builder.getInt64Ty(), tup->elementsCount());

            builder.CreateStore(zero, idxPtr);
            auto condBlock = llvm::BasicBlock::Create(builder.getContext(), "", builder.GetInsertBlock()->getParent());
            builder.CreateBr(condBlock);

            builder.SetInsertPoint(condBlock);
            auto idx = builder.CreateLoad(idxPtr);
            auto cond = builder.CreateICmpNE(idx, size);
            auto loopBlock = llvm::BasicBlock::Create(builder.getContext(), "", builder.GetInsertBlock()->getParent());
            auto mergeBlock = llvm::BasicBlock::Create(builder.getContext(), "", builder.GetInsertBlock()->getParent());
            builder.CreateCondBr(cond, loopBlock, mergeBlock);

            builder.SetInsertPoint(loopBlock);
            llvm::Value* idxList[] = {
                llvm::ConstantInt::get(idxPtr->getAllocatedType(), 0),
                idx,
            };
            auto elemPtr = builder.CreateGEP(arr, idxList);
            llvm::Value* args[] = { elemPtr };
            builder.CreateCall(customData(*proc)->defn, args);

            auto next = builder.CreateAdd(idx, one);
            builder.CreateStore(next, idxPtr);

            builder.CreateBr(condBlock);

            builder.SetInsertPoint(mergeBlock);

            return toRef(builder, *subj);
        }

        auto const proc = a->procedure();
        if ( !proc )
            return nullptr;

        const auto& axioms = sourceModule.axioms();
        auto const& exprs = a->expressions();

        if ( proc == axioms.intrinsic(ast::Array_idx)
          || proc == axioms.intrinsic(ast::Sliceu8_idx)
          || descendsFromTemplate(axioms.intrinsic(ast::Slice_idx)->symbol(), proc->symbol()) )
        {
            auto arr = toRef(builder, *a->expressions()[1]);
            auto basePtr = builder.CreateStructGEP(nullptr, arr, 0);
            auto baseVal = builder.CreateLoad(basePtr);
            auto idx = toValue(builder, toType(*proc->parameters()[1]->type()), *exprs[2]);
            return builder.CreateGEP(baseVal, idx);
        }
        else if ( proc == axioms.intrinsic(ast::Sliceu8_dtor) )
        {
            return toValue(builder, destType, *exprs[1]);
        }

        // todo: hash lookup
#define US(SYM)    \
    X(SYM##u, SYM) \
    X(SYM##s, SYM)

#define X_INSTR(X)   \
    US(Add)          \
    US(Sub)          \
    US(Mul)          \
    X(Divu, UDiv)    \
    X(Divs, SDiv)    \
    X(Remu, URem)    \
    X(Rems, SRem)    \
                     \
    US(Shl)          \
    X(Shru   , LShr) \
    X(Shrs   , AShr) \
    X(Bitandu, And ) \
    X(Bitands, And ) \
    X(Bitoru , Or  ) \
    X(Bitors , Or  ) \
    X(Bitxoru, Xor ) \
    X(Bitxoru, Xor ) \
                     \
    X(Equ , ICmpEQ ) \
    X(Eqs , ICmpEQ ) \
    X(Nequ, ICmpNE ) \
    X(Neqs, ICmpNE ) \
    X(Gtu , ICmpUGT) \
    X(Gts , ICmpSGT) \
    X(Geu , ICmpUGE) \
    X(Ges , ICmpSGE) \
    X(Ltu , ICmpULT) \
    X(Lts , ICmpSLT) \
    X(Leu , ICmpULE) \
    X(Les , ICmpSLE) \

#define X(ID,LLVMSUFFIX)                                                                             \
        else if ( ast::descendsFromTemplate(axioms.intrinsic(ast::ID)->symbol(), proc->symbol()) ) { \
            auto type = toType(*exprs[1]->type());                                                   \
            auto p1 = toValue(builder, type, *exprs[1]);                                             \
            auto p2 = toValue(builder, type, *exprs[2]);                                             \
            return builder.Create##LLVMSUFFIX(p1, p2);                                               \
        }

        X_INSTR(X)

#undef X
#undef X_INSTR
#undef US

        else if ( proc == axioms.intrinsic(ast::Truncu1u8)
                || proc == axioms.intrinsic(ast::Truncu1u16)
                || proc == axioms.intrinsic(ast::Truncu1u32)
                || proc == axioms.intrinsic(ast::Truncu1u64)
                || proc == axioms.intrinsic(ast::Truncu1u128) )
        {
            return builder.CreateTrunc(toValue(builder, destType, *exprs[1]), llvm::Type::getInt1Ty(builder.getContext()));
        }
        else if ( proc == axioms.intrinsic(ast::Truncu8u16)
                || proc == axioms.intrinsic(ast::Truncu8u32)
                || proc == axioms.intrinsic(ast::Truncu8u64)
                || proc == axioms.intrinsic(ast::Truncu8u128)
                || proc == axioms.intrinsic(ast::Trunci8i16)
                || proc == axioms.intrinsic(ast::Trunci8i32)
                || proc == axioms.intrinsic(ast::Trunci8i64)
                || proc == axioms.intrinsic(ast::Trunci8i128) )
        {
            return builder.CreateTrunc(toValue(builder, destType, *exprs[1]), llvm::Type::getInt8Ty(builder.getContext()));
        }
        else if ( proc == axioms.intrinsic(ast::Truncu16u32)
                || proc == axioms.intrinsic(ast::Truncu16u64)
                || proc == axioms.intrinsic(ast::Truncu16u128)
                || proc == axioms.intrinsic(ast::Trunci16i32)
                || proc == axioms.intrinsic(ast::Trunci16i64)
                || proc == axioms.intrinsic(ast::Trunci16i128) )
        {
            return builder.CreateTrunc(toValue(builder, destType, *exprs[1]), llvm::Type::getInt16Ty(builder.getContext()));
        }
        else if ( proc == axioms.intrinsic(ast::Truncu32u64)
                || proc == axioms.intrinsic(ast::Truncu32u128)
                || proc == axioms.intrinsic(ast::Trunci32i64)
                || proc == axioms.intrinsic(ast::Trunci32i128) )
        {
            return builder.CreateTrunc(toValue(builder, destType, *exprs[1]), llvm::Type::getInt32Ty(builder.getContext()));
        }
        else if ( proc == axioms.intrinsic(ast::Truncu64u128)
                || proc == axioms.intrinsic(ast::Trunci64i128) )
        {
            return builder.CreateTrunc(toValue(builder, destType, *exprs[1]), llvm::Type::getInt128Ty(builder.getContext()));
        }

        if ( rootTemplate(proc->symbol()) == &axioms.intrinsic(ast::Addr)->symbol() ) {
            auto ret = toRef(builder, *resolveIndirections(exprs[1]));
            if ( !ret )
                die("missing identity for addr");

            return ret;
        }

        return nullptr;
    }
};

//
// LLVMGenerator::LLVMState

struct LLVMGenerator::LLVMState
{
    Diagnostics& dgn;
    ast::ModuleSet& moduleSet;

    std::unique_ptr<llvm::LLVMContext> context;

    LLVMState(Diagnostics& dgn,
              ast::ModuleSet& moduleSet)
        : dgn(dgn)
        , moduleSet(moduleSet)
        , context(std::make_unique<llvm::LLVMContext>())
    {
    }

    ~LLVMState()
    {
        for ( auto& m : moduleSet.modules() )
            m->setCodegenData(nullptr);
    }

    ast::AxiomsModule const& axioms() const
    {
        return moduleSet.axioms();
    }

    void write(ast::Module const& module, std::experimental::filesystem::path const& path)
    {
        /*
        InitializeAllTargetInfos();
        InitializeAllTargets();
        InitializeAllTargetMCs();
        */
        llvm::InitializeNativeTarget();

        /*
        InitializeAllAsmParsers();
        */
        llvm::InitializeNativeTargetAsmParser();

        /*
        InitializeAllAsmPrinters();
        */
        llvm::InitializeNativeTargetAsmPrinter();

        auto targetTriple = llvm::sys::getDefaultTargetTriple();
        auto m = customData(module)->module.get();
        m->setTargetTriple(targetTriple);

        std::string err;
        auto target = llvm::TargetRegistry::lookupTarget(targetTriple, err);

        if ( !target ) {
            dgn.error(module) << err;
            return;
        }

        auto cpu = "generic";
        auto features = "";

        llvm::TargetOptions opt;
        auto rm = llvm::Optional<llvm::Reloc::Model>();
        auto targetMachine = target->createTargetMachine(targetTriple, cpu, features, opt, rm);

        m->setDataLayout(targetMachine->createDataLayout());

        auto sourcePath = module.path();
        if ( sourcePath.empty() ) {
            dgn.error(module) << "cannot determine output name for module";
            return;
        }

        std::error_code ec;
        llvm::raw_fd_ostream outFile(path.string(), ec, llvm::sys::fs::F_None);

        if ( ec ) {
            dgn.error(module) << "failed to write object file: " << ec.message();
            return;
        }

        llvm::legacy::PassManager pass;
        if ( targetMachine->addPassesToEmitFile(pass, outFile, llvm::TargetMachine::CGFT_ObjectFile) ) {
            dgn.error(module) << "cannot emit a file of this type for target machine " << targetTriple;
            return;
        }

        pass.run(*m);
        outFile.flush();
    }

    void writeIR(ast::Module const& module, std::experimental::filesystem::path const& path)
    {
        auto sourcePath = module.path();
        if ( sourcePath.empty() ) {
            dgn.error(module) << "cannot determine output name for module";
            return;
        }

        std::error_code ec;
        llvm::raw_fd_ostream outFile(path.string(), ec, llvm::sys::fs::F_None);

        if ( ec ) {
            dgn.error(module) << "failed to write IR file: " << ec.message();
            return;
        }

        customData(module)->module->print(outFile, nullptr);
    }

    void generate(ast::Module const& module)
    {
        module.setCodegenData(std::make_unique<LLVMCustomData<ast::Module>>());
        auto mdata = customData(module);
        mdata->module = std::make_unique<llvm::Module>(module.name(), *context);

        ast::ShallowApply<InitCodeGenPass> init(dgn, module);
        for ( auto d : module.scope()->childDeclarations() )
            init(*d);

        for ( auto d : module.scope()->childLambdas() )
            init(*d);

        for ( auto d : module.templateInstantiations() )
            init(*d);

        registerTypes(module, *module.scope());
        // todo: HACK needs dependency analysis
        for ( auto i = module.templateInstantiations().size() - 1; ~i; --i )
            registerTypes(module, *module.templateInstantiations()[i]);

        ast::ShallowApply<CodeGenPass> gen(dgn, mdata->module.get(), module);
        for ( auto d : module.templateInstantiations() )
            gen(*d);
        for ( auto d : module.scope()->childDeclarations() )
            gen(*d);
        for ( auto d : module.scope()->childLambdas() )
            gen(*d);
    }

    llvm::Type* toType(ast::Expression const& expr)
    {
        return codegen::toType(*context, expr);
    }

    llvm::Type* intrinsicType(ast::Declaration const& decl)
    {
        if ( auto ds = decl.as<ast::DataSumDeclaration>() ) {
            auto dsData = customData(*ds);
            if ( dsData->type )
                return dsData->type;

            if ( ds == axioms().intrinsic(ast::IntegerLiteralType )
              || ds == axioms().intrinsic(ast::RationalLiteralType) )
            {
                dsData->type = (llvm::Type*)0x1; // todo: choose width based on expression
                return dsData->type;
            }
            else if ( ds == axioms().intrinsic(ast::u1  ) ) return dsData->type = llvm::Type::getInt1Ty  (*context);
            else if ( ds == axioms().intrinsic(ast::u8  ) ) return dsData->type = llvm::Type::getInt8Ty  (*context);
            else if ( ds == axioms().intrinsic(ast::u16 ) ) return dsData->type = llvm::Type::getInt16Ty (*context);
            else if ( ds == axioms().intrinsic(ast::u32 ) ) return dsData->type = llvm::Type::getInt32Ty (*context);
            else if ( ds == axioms().intrinsic(ast::u64 ) ) return dsData->type = llvm::Type::getInt64Ty (*context);
            else if ( ds == axioms().intrinsic(ast::u128) ) return dsData->type = llvm::Type::getInt128Ty(*context);
            else if ( ds == axioms().intrinsic(ast::i8  ) ) return dsData->type = llvm::Type::getInt8Ty  (*context);
            else if ( ds == axioms().intrinsic(ast::i16 ) ) return dsData->type = llvm::Type::getInt16Ty (*context);
            else if ( ds == axioms().intrinsic(ast::i32 ) ) return dsData->type = llvm::Type::getInt32Ty (*context);
            else if ( ds == axioms().intrinsic(ast::i64 ) ) return dsData->type = llvm::Type::getInt64Ty (*context);
            else if ( ds == axioms().intrinsic(ast::i128) ) return dsData->type = llvm::Type::getInt128Ty(*context);

            auto const& sym = ds->symbol();
            if ( rootTemplate(sym) == &axioms().intrinsic(ast::PointerTemplate)->symbol()
              || rootTemplate(sym) == &axioms().intrinsic(ast::ReferenceTemplate)->symbol() )
            {
                auto t = toType(*sym.prototype().pattern()[0]);
                if ( t->isVoidTy() )
                    dsData->type = llvm::Type::getInt8PtrTy(*context);
                else
                    dsData->type = llvm::PointerType::getUnqual(t);
                return dsData->type;
            }
        }

        if ( auto dp = decl.as<ast::DataProductDeclaration>() ) {
            // todo
        }

        return nullptr;
    }

    llvm::Type* registerType(ast::Module const& mod, ast::Declaration const& decl)
    {
        if ( auto t = intrinsicType(decl) )
            return t;

        if ( auto ds = decl.as<ast::DataSumDeclaration>() ) {
            auto defn = ds->definition();
            if ( !defn )
                return nullptr;

            auto dsData = customData(*ds);
            if ( dsData->type )
                return dsData->type;

            dgn.error(mod, *ds) << "not implemented";
            dgn.die();

            return nullptr;
        }

        if ( auto dp = decl.as<ast::DataProductDeclaration>() ) {
            auto defn = dp->definition();
            if ( !defn )
                return nullptr;

            auto dpData = customData(*dp);
            if ( dpData->type )
                return dpData->type;

            std::vector<llvm::Type*> fieldTypes;
            fieldTypes.reserve(defn->fields().size());
            for ( auto& f : defn->fields() ) {
                auto d = getDeclaration(f->type());
                auto type = registerType(mod, *resolveIndirections(d));
                if ( !type ) {
                    dgn.error(mod, *d) << "type is not registered";
                    dgn.die();
                }

                fieldTypes.push_back(type);
            }

            dpData->type = llvm::StructType::create(*context,
                                                    fieldTypes,
                                                    dp->symbol().token().lexeme(),
                                                    /*isPacked*/false);
            return dpData->type;
        }

        return nullptr;
    }

    void registerTypes(ast::Module const& m, ast::DeclarationScope const& scope)
    {
        for ( auto d : scope.childDeclarations() )
            registerTypes(m, *d);

        for ( auto d : scope.childLambdas() )
            registerTypes(m, *d);
    }

    void registerTypes(ast::Module const& m, ast::Declaration const& decl)
    {
        auto d = resolveIndirections(&decl);
        if ( !d->symbol().prototype().isConcrete() )
            return;

        registerType(m, *d);

        if ( auto ds = d->as<ast::DataSumDeclaration>() ) {
            if ( auto defn = ds->definition() )
                registerTypes(m, *defn);
        }
        else if ( auto dp = d->as<ast::DataProductDeclaration>() ) {
            if ( auto defn = dp->definition() )
                registerTypes(m, *defn);
        }
        else if ( auto proc = d->as<ast::ProcedureDeclaration>() ) {
            if ( auto defn = proc->definition() )
                registerTypes(m, *defn);
        }
    }
};

//
// LLVMGenerator

LLVMGenerator::LLVMGenerator(Diagnostics& dgn, ast::ModuleSet& moduleSet)
    : myImpl(std::make_unique<LLVMState>(dgn, moduleSet))
{
}

LLVMGenerator::~LLVMGenerator() = default;

void LLVMGenerator::generate(ast::Module const& module)
{
    myImpl->generate(module);
}

void LLVMGenerator::write(ast::Module const& module, std::experimental::filesystem::path const& path)
{
    myImpl->write(module, path);
}

void LLVMGenerator::writeIR(ast::Module const& module, std::experimental::filesystem::path const& path)
{
    myImpl->writeIR(module, path);
}

    } // namespace codegen
} // namespace kyfoo
