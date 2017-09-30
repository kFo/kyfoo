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
    llvm::FunctionType* proto = nullptr;
    llvm::Function* body = nullptr;
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
    llvm::Type* type = nullptr;
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

LLVMCustomData<ast::Module>* customData(ast::Module const& mod)
{
    // todo
    return static_cast<LLVMCustomData<ast::Module>*>(mod.codegenData());
}

template <typename T>
LLVMCustomData<T>* customData(T const& decl)
{
    return static_cast<LLVMCustomData<T>*>(decl.codegenData());
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

llvm::Type* toType(ast::Declaration const& decl)
{
    auto d = resolveIndirections(&decl);
    if ( !d )
        return nullptr;

    if ( auto ds = d->as<ast::DataSumDeclaration>() )
        return customData(*ds)->type;

    if ( auto dp = d->as<ast::DataProductDeclaration>() )
        return customData(*dp)->type;

    return nullptr;
}

llvm::Type* toType(ast::Expression const& expr)
{
    if ( !expr.declaration() )
        return nullptr;

    return toType(*expr.declaration());
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

        if ( auto defn = decl.definition() )
            for ( auto& e : defn->childDeclarations() )
                dispatch(*e);
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
        }
    }

    result_t declField(ast::DataProductDeclaration::Field const&)
    {
        // nop
    }

    result_t declSymbol(ast::SymbolDeclaration const& s)
    {
        if ( s.expression() && s.expression()->declaration() )
            dispatch(*s.expression()->declaration());
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

        if ( auto defn = decl.definition() ) {
            for ( auto& e : defn->childDeclarations() )
                dispatch(*e);
        }

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
        if ( sv.boundExpression() && sv.boundExpression()->declaration() )
            dispatch(*sv.boundExpression()->declaration());
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
        // todo
        if ( auto defn = decl.definition() ) {
            for ( auto c : defn->childDeclarations() )
                dispatch(*c);
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
        if ( fun->body ) {
            error(decl.symbol().identifier()) << "defined more than once";
            die();
        }

        if ( fun->proto )
            return;

        auto returnType = toType(*decl.returnType());
        if ( !returnType ) {
            error(decl.symbol().identifier()) << "cannot resolve return type";
            die();
        }

        if ( decl.result()->passSemantics() == ast::ProcedureParameter::ByReference )
            returnType = llvm::PointerType::getUnqual(returnType);

        std::vector<llvm::Type*> params;
        params.reserve(decl.parameters().size());
        for ( auto const& p : decl.parameters() ) {
            llvm::Type* paramType = toType(*p->dataType());
            if ( !paramType ) {
                error(p->symbol().identifier()) << "cannot resolve parameter type";
                die();
            }

            if ( p->passSemantics() == ast::ProcedureParameter::ByReference )
                paramType = llvm::PointerType::getUnqual(paramType);

            params.push_back(paramType);
        }

        fun->proto = llvm::FunctionType::get(returnType, params, /*isVarArg*/false);

        fun->body = llvm::Function::Create(fun->proto,
                                           llvm::Function::ExternalLinkage, // todo
                                           decl.scope().declaration()->symbol().identifier().lexeme(),
                                           module);

        auto defn = decl.definition();
        if ( !defn )
            return;

        {
            auto arg = fun->body->arg_begin();
            for ( auto const& p : decl.parameters() )
                customData(*p)->arg = &*(arg++);
        }

        auto bb = llvm::BasicBlock::Create(module->getContext(), "entry", fun->body);
        llvm::IRBuilder<> builder(bb);

        for ( auto const& d : defn->childDeclarations() ) {
            if ( auto var = d->as<ast::VariableDeclaration>() ) {
                customData(*var)->inst = builder.CreateAlloca(toType(*var->dataType()));
            }
        }

        for ( auto const& stmt : defn->statements() ) {
            for ( auto const& v : stmt.unnamedVariables() ) {
                auto vdata = std::make_unique<LLVMCustomData<ast::VariableDeclaration>>();
                vdata->inst = builder.CreateAlloca(toType(*v->dataType()));
                v->setCodegenData(std::move(vdata));
            }
        }

        llvm::Value* lastInst = nullptr;
        for ( auto const& stmt : defn->statements() ) {
            lastInst = toValue(builder, stmt.expression());
            if ( !lastInst ) {
                error(stmt.expression()) << "invalid instruction";
                die();
            }

            auto u = stmt.unnamedVariables();
            if ( !u.length() )
                continue;

            for ( std::size_t i = u.length() - 1; ~i; --i ) {
                if ( auto dp = u[i]->dataType()->as<ast::DataProductDeclaration>() ) {
                    auto expr = createMemberCallExpression(*dp->definition()->destructor(), *u[i]);
                    toValue(builder, *expr);
                }
            }
        }

        // todo
        builder.CreateRet(lastInst);
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
        if ( auto defn = decl.definition() ) {
            for ( auto c : defn->childDeclarations() )
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

    llvm::AllocaInst* getThis(llvm::IRBuilder<>& builder,
                              ast::ProcedureDeclaration const& proc,
                              ast::ApplyExpression const& a)
    {
        if ( !proc.thisType() ) {
            error(proc) << "is not a method";
            die();
        }

        auto const arity = proc.symbol().prototype().pattern().size();
        auto const argCount = a.expressions().size() - 1;

        llvm::Value* val = nullptr;
        if ( arity == argCount ) {
            // this param is inferred
            auto subject = a.expressions()[0];
            if ( auto d = subject->as<ast::DotExpression>() ) {
                // member access via dot-expression
                auto instance = d->expressions()[d->expressions().size() - 2];
                val = toValue(builder, *instance);
            }
            else {
                error(a) << "cannot determine this param";
                die();
            }
        }
        else if ( arity == argCount + 1 ) {
            val = toValue(builder, *a.expressions()[1]);
        }
        else {
            error(a) << "cannot determine this param";
            die();
        }

        auto ret = llvm::dyn_cast_or_null<llvm::AllocaInst>(val);
        if ( !ret ) {
            error(a) << "this parameter is not an AllocaInst";
            die();
        }

        return ret;
    }

    llvm::Value* toValue(llvm::IRBuilder<>& builder, ast::Expression const& expression)
    {
        auto const& axioms = sourceModule.axioms();
        auto const& expr = *resolveIndirections(&expression);
        if ( auto intrin = intrinsicInstruction(builder, expr) )
            return intrin;

        if ( auto p = expr.as<ast::PrimaryExpression>() ) {
            auto decl = p->declaration();
            if ( !decl )
                die("unresolved identifier");

            if ( auto ds = decl->as<ast::DataSumDeclaration>() ) {
                if ( ds == axioms.intrinsic(ast::IntegerLiteralType) ) {
                    return llvm::ConstantInt::get(llvm::Type::getInt32Ty(builder.getContext()),
                                                  p->token().lexeme(), 10);
                }
                else if ( ds == axioms.intrinsic(ast::RationalLiteralType) ) {
                    return llvm::ConstantFP::get(llvm::Type::getDoubleTy(builder.getContext()),
                                                 p->token().lexeme());
                }
                else if ( ds == axioms.intrinsic(ast::StringLiteralType) ) {
                    return builder.CreateGlobalStringPtr(sourceModule.interpretString(dgn, p->token()));
                }
            }

            if ( auto dsCtor = decl->as<ast::DataSumDeclaration::Constructor>() )
                die("dsctor not implemented");

            if ( auto param = decl->as<ast::ProcedureParameter>() ) {
                auto pdata = customData(*param);
                return pdata->arg;
            }

            if ( auto var = decl->as<ast::VariableDeclaration>() ) {
                auto vdata = customData(*var);
                return vdata->inst;
            }

            die("unhandled identifier");
        }
        else if ( auto a = expr.as<ast::ApplyExpression>() ) {
            auto proc = a->declaration()->as<ast::ProcedureDeclaration>();
            if ( !proc ) {
                error(*a) << "expected apply-expression to refer to procedure-declaration";
                die();
            }

            std::vector<llvm::Value*> params;
            params.reserve(a->expressions().size());
            
            auto const isMethod = proc->thisType() ? 1 : 0;
            if ( isMethod )
                params.push_back(getThis(builder, *proc, *a));

            if ( a->expressions().size() > 1 ) {
                for ( std::size_t i = 1; i < a->expressions().size(); ++i ) {
                    auto const paramOrdinal = proc->ordinal(i - 1);
                    if ( paramOrdinal < isMethod )
                        continue;

                    auto const& e = a->expressions()[i];
                    if ( e->declaration() == axioms.intrinsic(ast::PointerNullLiteralType) ) {
                        auto paramType = toType(*proc->parameters()[paramOrdinal]->dataType());
                        params.push_back(llvm::ConstantPointerNull::get(static_cast<llvm::PointerType*>(paramType)));
                    }
                    else {
                        params.push_back(toValue(builder, *e));
                    }
                }
            }

            return builder.CreateCall(customData(*proc)->body, params);
        }
        else if ( auto dot = expr.as<ast::DotExpression>() ) {
            llvm::Value* ret = nullptr;
            auto exprs = dot->expressions();
            for ( auto const& e : exprs ) {
                if ( auto field = e->declaration()->as<ast::DataProductDeclaration::Field>() ) {
                    assert(ret && "expected field comes after another value");
                    auto fieldData = customData(*field);
                    ret = builder.CreateExtractValue(ret, fieldData->index);
                }
                else {
                    ret = toValue(builder, *e);
                }
            }

            return ret;
        }
        else if ( auto varExpr = expr.as<ast::VarExpression>() ) {
            auto var = varExpr->identity().declaration()->as<ast::VariableDeclaration>();
            auto vdata = customData(*var);
            return builder.CreateStore(toValue(builder, varExpr->expression()), vdata->inst);
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
                                      ast::Expression const& expr)
    {
        const auto& axioms = sourceModule.axioms();

        if ( auto a = expr.as<ast::ApplyExpression>() ) {
            auto const& exprs = a->expressions();
            auto const decl = a->declaration();

            if ( decl == axioms.intrinsic(ast::Sliceu8_ctor) ) {
                auto proc = decl->as<ast::ProcedureDeclaration>();
                auto v = getThis(builder, *proc, *a);

                // base
                builder.CreateStore(toValue(builder, *exprs[1]), builder.CreateStructGEP(nullptr, v, 0));

                // card
                auto const len = sourceModule.interpretString(dgn, exprs[1]->as<ast::PrimaryExpression>()->token()).size() + 1;
                auto lenVal = llvm::ConstantInt::get(toType(*axioms.intrinsic(ast::Sliceu8))->getContainedType(1), len);
                builder.CreateStore(lenVal, builder.CreateStructGEP(nullptr, v, 1));

                // todo: removeme
                return builder.CreateLoad(v);
            }
            else if ( decl == axioms.intrinsic(ast::Sliceu8_dtor) ) {
                auto proc = decl->as<ast::ProcedureDeclaration>();
                auto v = getThis(builder, *proc, *a);
                return v;
            }
            else if ( decl == axioms.intrinsic(ast::Addu1)
                   || decl == axioms.intrinsic(ast::Addu8)
                   || decl == axioms.intrinsic(ast::Addu16)
                   || decl == axioms.intrinsic(ast::Addu32)
                   || decl == axioms.intrinsic(ast::Addu64)
                   || decl == axioms.intrinsic(ast::Addi8)
                   || decl == axioms.intrinsic(ast::Addi16)
                   || decl == axioms.intrinsic(ast::Addi32)
                   || decl == axioms.intrinsic(ast::Addi64) )
            {
                auto p1 = toValue(builder, *exprs[1]);
                auto p2 = toValue(builder, *exprs[2]);
                return builder.CreateAdd(p1, p2);
            }
            else if ( decl == axioms.intrinsic(ast::Truncu1u8)
                   || decl == axioms.intrinsic(ast::Truncu1u16)
                   || decl == axioms.intrinsic(ast::Truncu1u32)
                   || decl == axioms.intrinsic(ast::Truncu1u64)
                   || decl == axioms.intrinsic(ast::Truncu1u128) )
            {
                return builder.CreateTrunc(toValue(builder, *exprs[1]), llvm::Type::getInt1Ty(builder.getContext()));
            }
            else if ( decl == axioms.intrinsic(ast::Truncu8u16)
                   || decl == axioms.intrinsic(ast::Truncu8u32)
                   || decl == axioms.intrinsic(ast::Truncu8u64)
                   || decl == axioms.intrinsic(ast::Truncu8u128)
                   || decl == axioms.intrinsic(ast::Trunci8i16)
                   || decl == axioms.intrinsic(ast::Trunci8i32)
                   || decl == axioms.intrinsic(ast::Trunci8i64)
                   || decl == axioms.intrinsic(ast::Trunci8i128) )
            {
                return builder.CreateTrunc(toValue(builder, *exprs[1]), llvm::Type::getInt8Ty(builder.getContext()));
            }
            else if ( decl == axioms.intrinsic(ast::Truncu16u32)
                   || decl == axioms.intrinsic(ast::Truncu16u64)
                   || decl == axioms.intrinsic(ast::Truncu16u128)
                   || decl == axioms.intrinsic(ast::Trunci16i32)
                   || decl == axioms.intrinsic(ast::Trunci16i64)
                   || decl == axioms.intrinsic(ast::Trunci16i128) )
            {
                return builder.CreateTrunc(toValue(builder, *exprs[1]), llvm::Type::getInt16Ty(builder.getContext()));
            }
            else if ( decl == axioms.intrinsic(ast::Truncu32u64)
                   || decl == axioms.intrinsic(ast::Truncu32u128)
                   || decl == axioms.intrinsic(ast::Trunci32i64)
                   || decl == axioms.intrinsic(ast::Trunci32i128) )
            {
                return builder.CreateTrunc(toValue(builder, *exprs[1]), llvm::Type::getInt32Ty(builder.getContext()));
            }
            else if ( decl == axioms.intrinsic(ast::Truncu64u128)
                   || decl == axioms.intrinsic(ast::Trunci64i128) )
            {
                return builder.CreateTrunc(toValue(builder, *exprs[1]), llvm::Type::getInt128Ty(builder.getContext()));
            }

            if ( rootTemplate(decl->symbol()) == &axioms.intrinsic(ast::Addr)->symbol() ) {
                return toValue(builder, *exprs[1]);
            }
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

    void generate(ast::Module const& module)
    {
        module.setCodegenData(std::make_unique<LLVMCustomData<ast::Module>>());
        auto mdata = customData(module);
        mdata->module = std::make_unique<llvm::Module>(module.name(), *context);

        ast::ShallowApply<InitCodeGenPass> init(dgn, module);
        for ( auto d : module.scope()->childDeclarations() )
            init(*d);

        for ( auto d : module.templateInstantiations() )
            init(*d);

        registerTypes(module, *module.scope());
        for ( auto d : module.templateInstantiations() )
            registerTypes(module, *d);

        ast::ShallowApply<CodeGenPass> gen(dgn, mdata->module.get(), module);
        for ( auto d : module.templateInstantiations() )
            gen(*d);
        for ( auto d : module.scope()->childDeclarations() )
            gen(*d);
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

    llvm::Type* intrinsicType(ast::Declaration const& decl)
    {
        if ( auto ds = decl.as<ast::DataSumDeclaration>() ) {
            auto dsData = customData(*ds);
            if ( dsData->type )
                return dsData->type;

            if ( ds == axioms().intrinsic(ast::IntegerLiteralType )
              || ds == axioms().intrinsic(ast::RationalLiteralType)
              || ds == axioms().intrinsic(ast::StringLiteralType  ) )
            {
                dsData->type = (llvm::Type*)0x1; // todo: choose width based on expression
                return dsData->type;
            }
            else if ( ds == axioms().intrinsic(ast::EmptyLiteralType) ) return dsData->type = llvm::Type::getVoidTy(*context);
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
            if ( rootTemplate(sym) == &axioms().intrinsic(ast::PointerTemplate)->symbol() ) {
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
                auto type = registerType(mod, *resolveIndirections(f->constraint().declaration()));
                if ( !type ) {
                    dgn.error(mod, *f->constraint().declaration()) << "type is not registered";
                    dgn.die();
                }

                fieldTypes.push_back(type);
            }

            dpData->type = llvm::StructType::create(*context,
                                                    fieldTypes,
                                                    dp->symbol().identifier().lexeme(),
                                                    /*isPacked*/false);
            return dpData->type;
        }

        return nullptr;
    }

    void registerTypes(ast::Module const& m, ast::DeclarationScope const& scope)
    {
        for ( auto d : scope.childDeclarations() )
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
