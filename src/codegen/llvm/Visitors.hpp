#pragma once

#include <kyfoo/codegen/llvm/Generator.hpp>

#include <filesystem>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-parameter"
#pragma warning(push, 0)
#include <llvm/ADT/APFloat.h>
#include <llvm/ADT/STLExtras.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DataLayout.h>
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
#pragma GCC diagnostic pop

#include <kyfoo/Algorithm.hpp>
#include <kyfoo/Diagnostics.hpp>
#include <kyfoo/Math.hpp>

#include <kyfoo/lexer/Token.hpp>
#include <kyfoo/lexer/TokenKind.hpp>

#include <kyfoo/ast/Axioms.hpp>
#include <kyfoo/ast/Declarations.hpp>
#include <kyfoo/ast/Expressions.hpp>
#include <kyfoo/ast/Fabrication.hpp>
#include <kyfoo/ast/Module.hpp>
#include <kyfoo/ast/Scopes.hpp>
#include <kyfoo/ast/Semantics.hpp>
#include <kyfoo/ast/Visitors.hpp>

#include <kyfoo/codegen/Codegen.hpp>

namespace {
    inline ::llvm::StringRef strRef(kyfoo::stringv s)
    {
        return ::llvm::StringRef(s.data(), s.card());
    }
}

namespace kyfoo::codegen::llvm {

template <typename T>
struct LLVMCustomData : public CustomData
{
};

template<>
struct LLVMCustomData<ast::Module> : public CustomData
{
    Box<::llvm::Module> module;
    std::map<stringv, ::llvm::GlobalVariable*> strings;

    ::llvm::GlobalVariable* interpretString(Diagnostics& dgn,
                                            ::llvm::IRBuilder<>& builder,
                                            ast::Module const& mod,
                                            lexer::Token const& token)
    {
        return getString(builder, mod.interpretString(dgn, token));
    }

    ::llvm::GlobalVariable* getString(::llvm::IRBuilder<>& builder, stringv str)
    {
        auto e = strings.lower_bound(str);
        if ( e != end(strings) && e->first == str )
            return e->second;

        return strings.emplace_hint(e, std::make_pair(str, builder.CreateGlobalString(strRef(str))))->second;
    }
};

template<>
struct LLVMCustomData<ast::VariableDeclaration> : public CustomData
{
    ::llvm::AllocaInst* inst = nullptr;
};

inline ::llvm::Function* cloneFunction(::llvm::Module* mod, ::llvm::Function const* func)
{
    return ::llvm::Function::Create(func->getFunctionType(),
                                    func->getLinkage(),
                                    func->getName(),
                                    mod);
}

template<>
struct LLVMCustomData<ast::ProcedureDeclaration> : public CustomData
{
    void define(::llvm::Module* mod, ::llvm::Function* func)
    {
        defn = func;
        functions.insert({mod, func});
    }

    bool isDefined() const
    {
        return defn;
    }

    ::llvm::Function* getFunction(::llvm::Module* mod)
    {
        auto e = functions.lower_bound(mod);
        if ( e != end(functions) && e->first == mod )
            return e->second;

        return functions.emplace_hint(e, std::make_pair(mod, cloneFunction(mod, defn)))->second;
    }

    ::llvm::FunctionType* getType()
    {
        return defn->getFunctionType();
    }

private:
    ::llvm::Function* defn = nullptr;
    std::map<::llvm::Module*, ::llvm::Function*> functions;
};

template<>
struct LLVMCustomData<ast::ProcedureParameter> : public CustomData
{
    ::llvm::Argument* arg = nullptr;
};

template<>
struct LLVMCustomData<ast::DataSumDeclaration> : public CustomData
{
    ::llvm::IntegerType* tagType = nullptr;
    ::llvm::Type* allocaType = nullptr;
};

template<>
struct LLVMCustomData<ast::DataProductDeclaration> : public CustomData
{
    ::llvm::Type* type = nullptr;
};

template<>
struct LLVMCustomData<ast::Constructor> : public CustomData
{
    ::llvm::StructType* type = nullptr;
};

template<>
struct LLVMCustomData<ast::Field> : public CustomData
{
    u32 index = 0;
};

template<>
struct LLVMCustomData<ast::Expression> : public CustomData
{
    ast::VariableDeclaration* tmp = nullptr;
};

template<>
struct LLVMCustomData<ast::BasicBlock> : public CustomData
{
    ::llvm::BasicBlock* bb = nullptr;
    ::llvm::BasicBlock* parentCleanup = nullptr;
};

template <typename T>
LLVMCustomData<T>* customData(T const& ast)
{
    return static_cast<LLVMCustomData<T>*>(ast.codegenData());
}

//
// CodeGenPass

template <typename Dispatcher>
struct CodeGenPass
{
    using result_t = void;
    Dispatcher& dispatch;

    Diagnostics& dgn;
    Context& ctx;
    ::llvm::Module* module;
    ast::Module const& sourceModule;
    std::vector<ast::ProcedureDeclaration const*> procBodiesToCodegen;

    CodeGenPass(Dispatcher& dispatch,
                Diagnostics& dgn,
                Context& ctx,
                ::llvm::Module* module,
                ast::Module const& sourceModule)
        : dispatch(dispatch)
        , dgn(dgn)
        , ctx(ctx)
        , module(module)
        , sourceModule(sourceModule)
    {
    }

    result_t declDataSum(ast::DataSumDeclaration const& ds)
    {
        ctx.generate(ds);
    }

    result_t declDataProduct(ast::DataProductDeclaration const& dp)
    {
        ctx.generate(dp);
    }

    result_t declSymbol(ast::SymbolDeclaration const& s)
    {
        if ( s.expression() ) {
            auto decl = getDeclaration(s.expression());
            if ( decl )
                dispatch(*decl);
        }
    }

    result_t declConstructor(ast::Constructor const& ctor)
    {
        ENFORCEU("constructors are only to be generated by their parents");
    }

    result_t declField(ast::Field const& field)
    {
        ENFORCEU("fields are only to be generated by their parents");
    }

    result_t declProcedure(ast::ProcedureDeclaration const& decl)
    {
        if ( !decl.symbol().prototype().isConcrete() || decl.codegenData() )
            return;

        decl.setCodegenData(mk<LLVMCustomData<ast::ProcedureDeclaration>>());

        if ( isIntrinsic(decl) )
            return;

        auto fdata = customData(decl);
        if ( fdata->isDefined() ) {
            error(decl.symbol().token()) << "defined more than once";
            die();
        }

        auto returnType = ctx.toType(*decl.returnType());
        if ( !returnType ) {
            error(decl.symbol().token()) << "cannot resolve return type";
            die();
        }

        std::vector<::llvm::Type*> params;
        params.reserve(decl.parameters().card());
        for ( auto const& p : decl.parameters() ) {
            declProcedureParameter(*p);
            ::llvm::Type* paramType = ctx.toType(*p->type());
            if ( !paramType ) {
                error(p->symbol().token()) << "cannot resolve parameter type";
                die();
            }

            if ( auto ds = ast::as<ast::DataSumDeclaration>(*p->type()) ) {
                if ( !paramType->isPointerTy() )
                    paramType = ::llvm::PointerType::get(paramType, 0);
            }

            params.push_back(paramType);
        }

        auto type = ::llvm::FunctionType::get(returnType, params, /*isVarArg*/false);

        ::llvm::StringRef name;
        if ( decl.scope().declaration() )
            name = strRef(decl.scope().declaration()->symbol().token().lexeme());

        auto defn = decl.definition();
        if ( !defn ) {
            fdata->define(module, ::llvm::Function::Create(type,
                                                           ::llvm::Function::ExternalLinkage,
                                                           name,
                                                           module));
            return;
        }

        // todo: function export
        /*auto linkage = ::llvm::Function::InternalLinkage;
        if ( !name.empty() )
            linkage = ::llvm::Function::ExternalLinkage;*/
        auto linkage = ::llvm::Function::ExternalLinkage;

        fdata->define(module, ::llvm::Function::Create(type, linkage, name, module));
        procBodiesToCodegen.push_back(&decl);
    }

    void generateProcBodies()
    {
        for ( auto procs = std::move(procBodiesToCodegen); !procs.empty(); procs = std::move(procBodiesToCodegen) ) {
            for ( auto p : procs )
                generateProcBody(*p);
        }
    }

    ::llvm::AllocaInst* createAlloca(::llvm::Type* type, ::llvm::Value* arraySize = nullptr)
    {
        return allocaBuilder.CreateAlloca(type, arraySize);
    }

    void generateProcBody(ast::ProcedureDeclaration const& decl)
    {
        auto defn = decl.definition();

        for ( auto d : defn->childDeclarations() )
            dispatch(*d);

        for ( auto l : defn->childLambdas() )
            dispatch(*l);

        auto fdata = customData(decl);
        auto fun = fdata->getFunction(module);
        {
            auto arg = fun->arg_begin();
            for ( auto const& p : decl.parameters() )
                customData(*p)->arg = &*(arg++);
        }

        ::llvm::IRBuilder<> allocaBuilder(::llvm::BasicBlock::Create(module->getContext(), "", fun));

        std::function<void(ast::ProcedureScope const&)> gatherAllocas =
        [&](ast::ProcedureScope const& scope) {
            for ( auto const& d : scope.childDeclarations() ) {
                if ( auto var = d->as<ast::VariableDeclaration>() ) {
                    declVariable(*var);
                    customData(*var)->inst = allocaBuilder.CreateAlloca(ctx.toType(*var->type()));
                }
            }

            auto gatherStatement = [this, &allocaBuilder](ast::Statement const& stmt) {
                for ( auto const& v : stmt.unnamedVariables() ) {
                    declVariable(*v);
                    auto type = ctx.toType(*v->type());
                    if ( !type->isVoidTy() )
                        customData(*v)->inst = allocaBuilder.CreateAlloca(type);
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
                    gatherStatement(retJunc->statement());
                }
            }

            for ( auto const& s : scope.childScopes() )
                gatherAllocas(*s);
        };
        gatherAllocas(*defn);

        auto entryBlock = ::llvm::BasicBlock::Create(module->getContext(), "", fun);
        toBlock(allocaBuilder, *defn, *defn->basicBlocks().front(), fun, entryBlock, nullptr);
        allocaBuilder.CreateBr(entryBlock);
        if ( ::llvm::verifyFunction(*fun, &::llvm::errs()) ) {
#ifndef NDEBUG
            fun->dump();
#endif
            die("verify failure");
        }
    }

    void toBlock(::llvm::IRBuilder<>& allocaBuilder,
                 ast::ProcedureScope const& scope,
                 ast::BasicBlock const& block,
                 ::llvm::Function* func,
                 ::llvm::BasicBlock* bb,
                 ::llvm::BasicBlock* parentCleanup)
    {
        auto bdata = customData(block);
        if ( !bdata ) {
            block.setCodegenData(mk<LLVMCustomData<ast::BasicBlock>>());
            bdata = customData(block);
        }
        bdata->bb = bb;
        bdata->parentCleanup = parentCleanup;

        auto fdata = customData(*scope.declaration());

        ::llvm::IRBuilder<> builder(bb);

        //uz cleanupDeclLast = 0;
        //std::vector<::llvm::BasicBlock*> cleanupBlocks;

        //auto createCleanupBlock = [&](lexer::Token const& token, bool exit) {
        //    auto const decls = scope.childDeclarations();
        //    std::vector<ast::VariableDeclaration*> priorVars;
        //    for ( ; cleanupDeclLast < decls.size(); ++cleanupDeclLast ) {
        //        if ( token.kind() != lexer::TokenKind::Undefined && !isBefore(decls[cleanupDeclLast]->symbol().token(), token) )
        //            break;

        //        if ( auto var = decls[cleanupDeclLast]->as<ast::VariableDeclaration>() )
        //            priorVars.push_back(var);
        //    }

        //    cleanupBlocks.push_back(::llvm::BasicBlock::Create(module->getContext(), "", func));
        //    ::llvm::IRBuilder<> cleanupBuilder(cleanupBlocks.back());
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
            if ( auto estmt = stmt->as<ast::ExpressionStatement>() ) {
                toValue(allocaBuilder, builder, builder.getVoidTy(), estmt->expression());
            }
            else if ( auto vstmt = stmt->as<ast::VariableStatement>() ) {
                if ( auto expr = vstmt->initializer() ) {
                    auto varType = ctx.toType(*vstmt->variable().type());
                    auto val = toValue(allocaBuilder, builder, varType, *expr);
                    if ( val->getType()->isVoidTy() )
                        continue;

                    builder.CreateStore(val, customData(vstmt->variable())->inst);
                }
            }

            //auto u = stmt->unnamedVariables();
            //if ( u.length() ) {
            //    for ( uz i = u.length() - 1; ~i; --i ) {
            //        if ( auto dp = getDeclaration(u[i]->type())->as<ast::DataProductDeclaration>() ) {
            //            if ( auto defn = dp->definition() ) {
            //                auto expr = createMemberCall(*defn->destructor(), *u[i]);
            //                toValue(builder, builder.getVoidTy(), *expr);
            //            }
            //        }
            //    }
            //}
        }

        if ( auto retJunc = block.junction()->as<ast::ReturnJunction>() ) {
            /*createCleanupBlock(retJunc->token(), true);
            builder.CreateBr(cleanupBlocks.back());*/

            auto retType = fdata->getType()->getReturnType();
            auto retVal = toValue(allocaBuilder, builder, retType, retJunc->expression());
            if ( !retVal || retType->isVoidTy() )
                builder.CreateRetVoid();
            else
                builder.CreateRet(retVal);

            return;
        }

        if ( auto brJunc = block.junction()->as<ast::BranchJunction>() ) {
            if ( brJunc->branch(0) ) {
                // todo: conditional trait
                auto cond = toValue(allocaBuilder, builder, builder.getInt1Ty(), *brJunc->condition());
                auto cmp = cond->getType() == builder.getInt1Ty() ?
                    cond :
                    builder.CreateICmpNE(cond, ::llvm::ConstantInt::get(cond->getType(), 0));

                auto trueBlock = ::llvm::BasicBlock::Create(module->getContext(), "", func);
                auto falseBlock = ::llvm::BasicBlock::Create(module->getContext(), "", func);

                builder.CreateCondBr(cmp, trueBlock, falseBlock);

                toBlock(allocaBuilder, scope, *brJunc->branch(0), func, trueBlock, nullptr /*cleanupBlocks.back()*/);
                toBlock(allocaBuilder, scope, *brJunc->branch(1), func, falseBlock, nullptr /*cleanupBlocks.back()*/);
            }
            else {
                auto ds = getDeclaration(brJunc->condition()->type())->as<ast::DataSumDeclaration>();
                auto dsData = customData(*ds);
                auto subjRef = toRef(allocaBuilder, builder, *brJunc->condition());
                ::llvm::Value* subjTagVal = nullptr;
                if ( subjRef->getType()->isPointerTy() ) {
                    subjTagVal = builder.CreateLoad(builder.CreateStructGEP(subjRef, 0));
                }
                else {
                    unsigned idxs[] = { 0 };
                    subjTagVal = builder.CreateExtractValue(subjRef, idxs);
                }
                std::vector<ast::BranchJunction const*> branches;
                ast::BasicBlock const* merge = brJunc->branch(1);
                for ( auto br = merge->junction()->as<ast::BranchJunction>();
                      br && br->isElse();
                      br = merge->junction()->as<ast::BranchJunction>() )
                {
                    ENFORCE(merge->statements().card() == 0, "branch blocks should be empty");
                    branches.emplace_back(br);
                    merge = br->branch(1);
                }
                auto mergeBlock = ::llvm::BasicBlock::Create(module->getContext(), "", func);
                toBlock(allocaBuilder, scope, *merge, func, mergeBlock, nullptr);
                auto sw = builder.CreateSwitch(subjTagVal, mergeBlock, branches.size());
                ::llvm::BasicBlock* lastBlock = nullptr;
                for ( auto br : branches ) {
                    auto const ordinal = indexOf(ds->definition()->constructors(), getDeclaration(br->condition())->as<ast::Constructor>());
                    auto brTagVal = ::llvm::ConstantInt::get(dsData->tagType, ordinal);
                    lastBlock = ::llvm::BasicBlock::Create(module->getContext(), "", func);
                    toBlock(allocaBuilder, scope, *br->branch(0), func, lastBlock, nullptr);
                    sw->addCase(brTagVal, lastBlock);
                }
                mergeBlock->moveAfter(lastBlock);
            }

            return;
        }

        if ( auto jmpJunc = block.junction()->as<ast::JumpJunction>() ) {
            if ( auto tdata = customData(*jmpJunc->targetBlock()) ) {
                builder.CreateBr(tdata->bb);
            }
            else {
                auto target = ::llvm::BasicBlock::Create(module->getContext(), "", func);
                toBlock(allocaBuilder, scope, *jmpJunc->targetBlock(), func, target, nullptr /*cleanupBlocks.back()*/);
                builder.CreateBr(target);
            }

            return;
        }

        die("missing procedure terminator");
    }

    result_t declProcedureParameter(ast::ProcedureParameter const& param)
    {
        if ( !param.codegenData() )
            param.setCodegenData(mk<LLVMCustomData<ast::ProcedureParameter>>());
    }

    result_t declVariable(ast::VariableDeclaration const& var)
    {
        if ( !var.codegenData() )
            var.setCodegenData(mk<LLVMCustomData<ast::VariableDeclaration>>());
    }

    result_t declImport(ast::ImportDeclaration const&)
    {
        // nop
    }

    result_t declSymbolVariable(ast::SymbolVariable const& symVar)
    {
        if ( symVar.boundExpression() )
            if ( auto decl = getDeclaration(symVar.boundExpression()) )
                dispatch(*decl);
    }

    result_t declTemplate(ast::TemplateDeclaration const& templ)
    {
        if ( !templ.symbol().prototype().isConcrete() || templ.codegenData() )
            return;

        if ( auto defn = templ.definition() ) {
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

private:
    template <typename T>
    LLVMCustomData<T>* cgd(T const& ast)
    {
        if ( !ast.codegenData() )
            dispatch(ast);

        return customData(ast);
    }

    template<>
    LLVMCustomData<ast::Module>* cgd(ast::Module const& mod)
    {
        // todo
        return customData(mod);
    }

    ::llvm::Constant* toLiteral(::llvm::IRBuilder<>& builder, ::llvm::Type* destType, lexer::Token const& token)
    {
        switch ( token.kind() ) {
        case lexer::TokenKind::Integer:
        {
            auto intType = ::llvm::dyn_cast_or_null<::llvm::IntegerType>(destType);
            if ( !intType ) {
                auto bits = ::llvm::APInt::getBitsNeeded(strRef(token.lexeme()), 10);
                return builder.getInt(::llvm::APInt(bits, strRef(token.lexeme()), 10));
            }

            return ::llvm::ConstantInt::get(intType, strRef(token.lexeme()), 10);
        }
        case lexer::TokenKind::Rational:
            return ::llvm::ConstantFP::get(destType, strRef(token.lexeme()));
        case lexer::TokenKind::String:
        {
            auto const strType = ::llvm::dyn_cast<::llvm::StructType>(cgd(*sourceModule.axioms().intrinsic(ast::ascii))->type);
            if ( destType != strType )
                return nullptr;

            return cgd(sourceModule)->interpretString(dgn, builder, sourceModule, token);
        }

        default:
            die("invalid literal");
            return nullptr;
        }
    }

    ::llvm::Value* toRef(::llvm::IRBuilder<>& allocaBuilder, ::llvm::IRBuilder<>& builder, ast::Expression const& expr_)
    {
        auto expr = resolveIndirections(&expr_);
        if ( auto dot = expr->as<ast::DotExpression>() ) {
            ::llvm::Value* ret = nullptr;
            auto exprs = dot->expressions();
            for ( auto e : exprs ) {
                e = resolveIndirections(e);
                if ( auto decl = getDeclaration(e) ) {
                    if ( auto var = decl->as<ast::VariableDeclaration>() ) {
                        ret = cgd(*var)->inst;
                        continue;
                    }

                    if ( auto param = decl->as<ast::ProcedureParameter>() ) {
                        ret = cgd(*param)->arg;
                        continue;
                    }

                    if ( auto field = decl->as<ast::Field>() ) {
                        auto fdata = cgd(*field);
                        ret = builder.CreateStructGEP(nullptr, ret, fdata->index);
                        continue;
                    }
                }
                else if ( auto lit = e->as<ast::LiteralExpression>() ) {
                    if ( lit->token().kind() != lexer::TokenKind::Integer )
                        die("bad index");

                    auto index = static_cast<unsigned>(stoi(lit->token().lexeme()));
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
                return cgd(*var)->inst;

            if ( auto param = decl->as<ast::ProcedureParameter>() )
                return cgd(*param)->arg;
        }

        if ( auto app = expr->as<ast::ApplyExpression>() ) {
            if ( app->expressions().card() == 2
                && app->subject()->type()->kind() == ast::Expression::Kind::Tuple )
            {
                auto arr = toRef(allocaBuilder, builder, *app->subject());
                auto idx = toValue(allocaBuilder, builder, builder.getInt64Ty(), *app->expressions()[1]);
                ::llvm::Value* idxList[] = {
                    ::llvm::ConstantInt::get(idx->getType(), 0),
                    idx,
                };
                return builder.CreateGEP(arr, idxList);
            }
        }

        return nullptr;
    }

    ::llvm::Value* toValue(::llvm::IRBuilder<>& allocaBuilder,
                           ::llvm::IRBuilder<>& builder,
                           ::llvm::Type* destType,
                           ast::Expression const& expression)
    {
        auto ret = toValue_impl(allocaBuilder, builder, destType, expression);
        if ( ret && ret->getType()->isPointerTy() && ret->getType()->getPointerElementType() == destType )
            return builder.CreateLoad(ret);

        return ret;
    }

    // todo: removeme
    ::llvm::Value* toValue_impl(::llvm::IRBuilder<>& allocaBuilder,
                                ::llvm::IRBuilder<>& builder,
                                ::llvm::Type* destType,
                                ast::Expression const& expression)
    {
        auto const& axioms = sourceModule.axioms();
        auto const& expr = *resolveIndirections(&expression);
        if ( auto intrin = intrinsicInstruction(allocaBuilder, builder, destType, expr) )
            return intrin;

        if ( auto lit = expr.as<ast::LiteralExpression>() )
            return toLiteral(builder, destType, lit->token());

        if ( auto dot = expr.as<ast::DotExpression>() ) {
            ::llvm::Value* ret = nullptr;
            auto exprs = dot->expressions();
            for ( auto const& e : exprs ) {
                if ( auto decl = getDeclaration(e) ) {
                    if ( auto field = decl->as<ast::Field>() ) {
                        assert(ret && "expected field comes after another value");
                        auto fieldData = cgd(*field);
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

                    auto index = static_cast<unsigned>(stoi(lit->token().lexeme()));
                    if ( ret->getType()->isPointerTy() )
                        ret = builder.CreateLoad(builder.CreateStructGEP(nullptr, ret, index));
                    else
                        ret = builder.CreateExtractValue(ret, index);
                    continue;
                }

                ret = toValue(allocaBuilder, builder, destType, *e);
            }

            return ret;
        }

        if ( auto id = identify(expr) ) {
            auto decl = id->declaration();
            if ( !decl ) {
                auto d = resolveIndirections(getDeclaration(*id->type()));
                if ( d == axioms.intrinsic(ast::PointerNullLiteralType) ) {
                    auto ptrType = ::llvm::dyn_cast_or_null<::llvm::PointerType>(destType);
                    return ::llvm::ConstantPointerNull::get(ptrType);
                }

                die("unresolved identifier");
            }

            if ( auto param = decl->as<ast::ProcedureParameter>() ) {
                auto pdata = cgd(*param);
                return pdata->arg;
            }

            if ( auto var = decl->as<ast::VariableDeclaration>() ) {
                auto vdata = cgd(*var);
                // todo: removeme
                if ( destType && destType->isPointerTy() )
                    return vdata->inst;

                if ( !vdata->inst )
                    return nullptr;

                return builder.CreateLoad(vdata->inst);
            }

            if ( auto proc = decl->as<ast::ProcedureDeclaration>() ) {
                auto pdata = cgd(*proc);
                return builder.CreateCall(pdata->getFunction(module));
            }

            if ( auto ctor = decl->as<ast::Constructor>() ) {
                auto cdata = cgd(*ctor);
                auto a = allocaBuilder.CreateAlloca(cdata->type);
                auto const ordinal = indexOf(ctor->parent().definition()->constructors(), ctor);
                builder.CreateStore(::llvm::ConstantInt::get(cdata->type->getElementType(0), ordinal),
                                    builder.CreateStructGEP(a, 0));
                if ( auto dptr = ::llvm::dyn_cast_or_null<::llvm::PointerType>(destType) ) {
                    auto dsType = ctx.toType(ctor->parent());
                    if ( dptr->getPointerElementType() == dsType )
                        return builder.CreatePointerCast(a, destType);
                }

                return a;
            }

            die("unhandled identifier");
        }

        if ( auto a = expr.as<ast::ApplyExpression>() ) {
            auto const& exprs = a->expressions();
            auto proc = a->procedure();
            ::llvm::Value* fptr = nullptr;
            if ( !proc ) {
                proc = getProcedure(*a->subject());
                if ( !proc )
                    fptr = toValue(allocaBuilder, builder, nullptr, *exprs[0]);
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

            if ( !arrow )
                die("cannot determine function type");

            auto formalParams = arrow->sliceFrom();
            if ( auto tup = arrow->from().as<ast::TupleExpression>() )
                if ( tup->kind() == ast::TupleKind::Open )
                    formalParams = tup->expressions();

            std::vector<::llvm::Value*> args;
            args.reserve(exprs.card());

            for ( uz i = 1; i < exprs.card(); ++i ) {
                auto paramType = ctx.toType(*formalParams[i - 1]);

                // todo: encapsulate
                if ( auto ds = ast::as<ast::DataSumDeclaration>(formalParams[i - 1]) ) {
                    if ( !isIntrinsic(*ds) )
                        paramType = ::llvm::PointerType::get(paramType, 0);
                }

                auto val = toValue(allocaBuilder, builder, paramType, *exprs[i]);
                if ( !val )
                    die("cannot determine arg");
                args.push_back(val);
            }

            if ( proc )
                return builder.CreateCall(cgd(*proc)->getFunction(module), args);

            return builder.CreateCall(fptr, args);
        }

        if ( auto assign = expr.as<ast::AssignExpression>() ) {
            auto ref = toRef(allocaBuilder, builder, assign->left());
            if ( !ref )
                ref = toValue(allocaBuilder, builder, nullptr, assign->left());

            auto leftType = ref ? ref->getType()->getPointerElementType() : builder.getVoidTy();
            auto val = toValue(allocaBuilder, builder, leftType, assign->right());
            if ( val->getType()->isVoidTy() )
                return nullptr;

            if ( !ref || !val )
                die("cannot determine reference");

            builder.CreateStore(val, ref);
            return builder.CreateLoad(ref);
        }

        if ( auto l = expr.as<ast::LambdaExpression>() ) {
            auto pdata = cgd(l->procedure());
            return pdata->getFunction(module);
        }

        return nullptr;
    }

    bool isIntrinsic(ast::ProcedureDeclaration const& proc)
    {
        if ( sourceModule.axioms().isIntrinsic(proc) )
            return true;

        if ( auto pdecl = proc.scope().declaration() )
            return pdecl == sourceModule.axioms().intrinsic(ast::ascii);

        return false;
    }

    bool isIntrinsic(ast::DataSumDeclaration const& ds)
    {
        return sourceModule.axioms().isIntrinsic(ds);
    }

    ::llvm::Value* intrinsicInstruction(::llvm::IRBuilder<>& allocaBuilder,
                                        ::llvm::IRBuilder<>& builder,
                                        ::llvm::Type* /*destType*/,
                                        ast::Expression const& expr)
    {
        auto a = expr.as<ast::ApplyExpression>();
        if ( !a )
            return nullptr;

        auto subj = resolveIndirections(a->subject());
        if ( auto tup = subj->type()->as<ast::TupleExpression>() ) {
            if ( a->arguments().card() != 1 )
                die("expected one argument");

            auto proc = getProcedure(*a->arguments()[0]);
            if ( !proc ) {
                if ( a->expressions().card() == 2 ) {
                    // assume it's indexing into the tuple
                    auto arr = toRef(allocaBuilder, builder, *subj);
                    auto idx = toValue(allocaBuilder, builder, builder.getInt64Ty(), *a->expressions()[1]);
                    ::llvm::Value* idxList[] = {
                        ::llvm::ConstantInt::get(idx->getType(), 0),
                        idx,
                    };
                    return builder.CreateGEP(arr, idxList);
                }

                die("expected procedure argument");
            }

            if ( tup->elementsCount() <= 1 )
                die("apply is not implemented for tuples");

            auto arr = toRef(allocaBuilder, builder, *subj);
            auto idxPtr = ::llvm::IRBuilder<>(&builder.GetInsertBlock()->getParent()->getEntryBlock()).CreateAlloca(builder.getInt64Ty());
            auto const zero = ::llvm::ConstantInt::get(builder.getInt64Ty(), 0);
            auto const one = ::llvm::ConstantInt::get(builder.getInt64Ty(), 1);
            auto const card = ::llvm::ConstantInt::get(builder.getInt64Ty(), tup->elementsCount());

            builder.CreateStore(zero, idxPtr);
            auto condBlock = ::llvm::BasicBlock::Create(builder.getContext(), "", builder.GetInsertBlock()->getParent());
            builder.CreateBr(condBlock);

            builder.SetInsertPoint(condBlock);
            auto idx = builder.CreateLoad(idxPtr);
            auto cond = builder.CreateICmpNE(idx, card);
            auto loopBlock = ::llvm::BasicBlock::Create(builder.getContext(), "", builder.GetInsertBlock()->getParent());
            auto mergeBlock = ::llvm::BasicBlock::Create(builder.getContext(), "", builder.GetInsertBlock()->getParent());
            builder.CreateCondBr(cond, loopBlock, mergeBlock);

            builder.SetInsertPoint(loopBlock);
            ::llvm::Value* idxList[] = {
                ::llvm::ConstantInt::get(idxPtr->getAllocatedType(), 0),
                idx,
            };
            auto elemPtr = builder.CreateGEP(arr, idxList);
            ::llvm::Value* args[] = { elemPtr };
            builder.CreateCall(cgd(*proc)->getFunction(module), args);

            auto next = builder.CreateAdd(idx, one);
            builder.CreateStore(next, idxPtr);

            builder.CreateBr(condBlock);

            builder.SetInsertPoint(mergeBlock);

            return toRef(allocaBuilder, builder, *subj);
        }

        auto const proc = a->procedure();
        if ( !proc )
            return nullptr;

        const auto& axioms = sourceModule.axioms();
        auto const& exprs = a->expressions();

        auto const rootTempl = rootTemplate(proc->symbol());
        if ( rootTempl == &axioms.intrinsic(ast::UnsignedFromInteger      )->symbol()
          || rootTempl == &axioms.intrinsic(ast::SignedFromInteger        )->symbol()
          || rootTempl == &axioms.intrinsic(ast::implicitIntegerToUnsigned)->symbol()
          || rootTempl == &axioms.intrinsic(ast::implicitIntegerToSigned  )->symbol() )
        {
            return toLiteral(builder,
                             ctx.toType(*proc->result()->type()),
                             exprs[1]->as<ast::LiteralExpression>()->token());
        }
        else if ( rootTempl == &axioms.intrinsic(ast::UnsignedFromUnsigned)->symbol() )
        {
            return builder.CreateZExt(toValue(allocaBuilder, builder, ctx.toType(*proc->result()->type()), *exprs[1]),
                                      ctx.toType(*proc->parameters()[0]->type()));
        }
        else if ( rootTempl == &axioms.intrinsic(ast::SignedFromSigned)->symbol() )
        {
            return builder.CreateSExt(toValue(allocaBuilder, builder, ctx.toType(*proc->result()->type()), *exprs[1]),
                                      ctx.toType(*proc->parameters()[0]->type()));
        }
        else if ( rootTempl == &axioms.intrinsic(ast::UnsignedSucc)->symbol()
               || rootTempl == &axioms.intrinsic(ast::UnsignedPred)->symbol()
               || rootTempl == &axioms.intrinsic(ast::SignedSucc  )->symbol()
               || rootTempl == &axioms.intrinsic(ast::SignedPred  )->symbol() )
        {
            auto selfType = ctx.toType(*getDeclaration(*proc->parameters()[0]->type()));
            auto val = toValue(allocaBuilder, builder, selfType, *exprs[1]);
            auto const isAdd = rootTempl == &axioms.intrinsic(ast::UnsignedSucc)->symbol()
                            || rootTempl == &axioms.intrinsic(ast::UnsignedPred)->symbol()
                            || rootTempl == &axioms.intrinsic(ast::SignedSucc  )->symbol()
                            || rootTempl == &axioms.intrinsic(ast::SignedPred  )->symbol();
            auto nextVal = isAdd
                ? builder.CreateAdd(val, ::llvm::ConstantInt::get(selfType, 1))
                : builder.CreateSub(val, ::llvm::ConstantInt::get(selfType, 1));
            return nextVal;
        }
        else if ( rootTempl == &axioms.intrinsic(ast::UnsignedInc)->symbol()
               || rootTempl == &axioms.intrinsic(ast::SignedInc  )->symbol()
               || rootTempl == &axioms.intrinsic(ast::UnsignedDec)->symbol()
               || rootTempl == &axioms.intrinsic(ast::SignedDec  )->symbol() )
        {
            auto selfType = ctx.toType(removeReference(*getDeclaration(*proc->parameters()[0]->type())));
            auto selfRef = toRef(allocaBuilder, builder, *exprs[1]);
            auto val = builder.CreateLoad(selfRef);

            auto const isAdd = rootTempl == &axioms.intrinsic(ast::UnsignedInc )->symbol()
                            || rootTempl == &axioms.intrinsic(ast::SignedInc   )->symbol();
            auto nextVal = isAdd
                ? builder.CreateAdd(val, ::llvm::ConstantInt::get(selfType, 1))
                : builder.CreateSub(val, ::llvm::ConstantInt::get(selfType, 1));

            builder.CreateStore(nextVal, selfRef);
            return selfRef;
        }
        else if ( rootTempl == &axioms.intrinsic(ast::Array_idx)->symbol()
               || rootTempl == &axioms.intrinsic(ast::Slice_idx)->symbol() )
        {
            auto arr = toRef(allocaBuilder, builder, *exprs[1]);
            auto basePtr = builder.CreateStructGEP(nullptr, arr, 0);
            auto baseVal = builder.CreateLoad(basePtr);
            auto idx = toValue(allocaBuilder, builder, ctx.toType(*proc->parameters()[1]->type()), *exprs[2]);
            return builder.CreateGEP(baseVal, idx);
        }
        else if ( rootTempl == &axioms.intrinsic(ast::implicitStringToAscii)->symbol() )
        {
            auto const strType = ::llvm::dyn_cast<::llvm::StructType>(cgd(*sourceModule.axioms().intrinsic(ast::ascii))->type);
            auto const& str = sourceModule.interpretString(dgn, exprs[1]->as<ast::LiteralExpression>()->token());
            auto gv = cgd(sourceModule)->getString(builder, str);
            auto zero = builder.getInt32(0);
            ::llvm::Constant* idx[] = { zero, zero };
            ::llvm::Constant* s[2] = {
                ::llvm::ConstantExpr::getInBoundsGetElementPtr(gv->getValueType(), gv, idx),
                builder.getInt64(str.card())
            };
            return ::llvm::ConstantStruct::get(strType, s);
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
    X(Equ, ICmpEQ ) \
    X(Eqs, ICmpEQ ) \
    X(Neu, ICmpNE ) \
    X(Nes, ICmpNE ) \
    X(Gtu, ICmpUGT) \
    X(Gts, ICmpSGT) \
    X(Geu, ICmpUGE) \
    X(Ges, ICmpSGE) \
    X(Ltu, ICmpULT) \
    X(Lts, ICmpSLT) \
    X(Leu, ICmpULE) \
    X(Les, ICmpSLE)

#define X(ID,LLVMSUFFIX)                                                                             \
        else if ( rootTempl == &axioms.intrinsic(ast::ID)->symbol() ) { \
            auto t1 = ctx.toType(*proc->parameters()[0]->type());                                        \
            auto t2 = ctx.toType(*proc->parameters()[1]->type());                                        \
            auto p1 = toValue(allocaBuilder, builder, t1, *exprs[1]);                                               \
            auto p2 = toValue(allocaBuilder, builder, t2, *exprs[2]);                                               \
            return builder.Create##LLVMSUFFIX(p1, p2);                                               \
        }

        X_INSTR(X)

#undef X
#undef X_INSTR
#undef US

        else if ( proc == axioms.intrinsic(ast::Not) )
        {
            return builder.CreateNot(toValue(allocaBuilder, builder, ctx.toType(*proc->parameters()[0]->type()), *exprs[1]));
        }
        else if ( rootTempl == &axioms.intrinsic(ast::Truncu)->symbol()
               || rootTempl == &axioms.intrinsic(ast::Truncs)->symbol() )
        {
            auto srcType = ctx.toType(*proc->parameters()[0]->type());
            auto dstType = ctx.toType(*proc->returnType());

            {
                auto srcInt = ::llvm::dyn_cast_or_null<::llvm::IntegerType>(srcType);
                auto dstInt = ::llvm::dyn_cast_or_null<::llvm::IntegerType>(dstType);
                if ( !srcInt || !dstInt )
                    die("trunc instruction parameter is not an integer");

                if ( dstInt->getBitWidth() > srcInt->getBitWidth() )
                    die("trunc instruction destination width is greater than source width (extension)");
            }

            auto p = toValue(allocaBuilder, builder, srcType, *exprs[1]);
            return builder.CreateTrunc(p, dstType);
        }
        else if ( rootTempl == &axioms.intrinsic(ast::Addr)->symbol() ) {
            auto ret = toRef(allocaBuilder, builder, *resolveIndirections(exprs[1]));
            if ( !ret )
                die("missing identity for addr");

            return ret;
        }
        else if ( rootTempl == &axioms.intrinsic(ast::Cast)->symbol() ) {
            auto templ = procTemplate(*proc);
            return builder.CreatePointerCast(toValue(allocaBuilder, builder, nullptr, *resolveIndirections(exprs[1])), ctx.toType(*templ->symbol().prototype().pattern()[0]));
        }

        return nullptr;
    }
};

} // namespace kyfoo::codegen::llvm
