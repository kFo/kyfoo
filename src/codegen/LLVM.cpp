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
struct LLVMCustomData<ast::VariableDeclaration> : public CustomData
{
    llvm::Value* value = nullptr;
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
    llvm::Value* value = nullptr;
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
    llvm::Type* type = nullptr;
    std::uint32_t index = 0;
};

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

llvm::Type* toType(ast::Expression const& expr)
{
    auto decl = resolveIndirections(expr.declaration());
    if ( !decl )
        return nullptr;

    if ( auto ds = decl->as<ast::DataSumDeclaration>() )
        return customData(*ds)->type;

    if ( auto dp = decl->as<ast::DataProductDeclaration>() )
        return customData(*dp)->type;

    return nullptr;
}

//
// InitCodeGenPass

template <typename Dispatcher>
struct InitCodeGenPass
{
    using result_t = void;
    Dispatcher& dispatch;

    InitCodeGenPass(Dispatcher& dispatch)
        : dispatch(dispatch)
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

        declProcedureParameter(*decl.result());
        for ( auto const& p : decl.parameters() )
            declProcedureParameter(*p);

        if ( auto defn = decl.definition() )
            for ( auto& e : defn->childDeclarations() )
                dispatch(*e);
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
    ast::Module& sourceModule;

    CodeGenPass(Dispatcher& dispatch,
                Diagnostics& dgn,
                llvm::Module* module,
                ast::Module& sourceModule)
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

    result_t declDataProduct(ast::DataProductDeclaration const&)
    {
        // todo
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

        if ( sourceModule.axioms().isIntrinsic(decl) )
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

        std::vector<llvm::Type*> params;
        params.reserve(decl.parameters().size());
        for ( auto const& p : decl.parameters() ) {
            auto paramType = toType(p->expression());
            if ( !paramType ) {
                error(p->symbol().identifier()) << "cannot resolve parameter type";
                die();
            }

            params.push_back(paramType);
        }

        fun->proto = llvm::FunctionType::get(returnType, params, /*isVarArg*/false);

        if ( !decl.definition() )
            return;

        fun->body = llvm::Function::Create(fun->proto,
                                           llvm::Function::ExternalLinkage, // todo
                                           decl.symbol().identifier().lexeme(),
                                           module);

        {
            auto arg = fun->body->arg_begin();
            for ( auto const& p : decl.parameters() )
                customData(*p)->value = &*(arg++);
        }

        auto bb = llvm::BasicBlock::Create(module->getContext(), "entry", fun->body);
        llvm::IRBuilder<> builder(bb);

        llvm::Value* lastInst = nullptr;
        for ( auto const& e : decl.definition()->expressions() ) {
            lastInst = toValue(builder, *e);
            if ( !lastInst ) {
                error(*e) << "invalid instruction";
                die();
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

    result_t declTemplate(ast::TemplateDeclaration const&)
    {
        // todo
    }

private:
    Error& error()
    {
        return dgn.error(sourceModule) << "codegen: ";
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

    llvm::Value* toValue(llvm::IRBuilder<>& builder, ast::Expression const& expr)
    {
        auto const& axioms = sourceModule.axioms();
        if ( auto inst = intrinsicInstruction(builder, expr) )
            return inst;

        if ( auto p = expr.as<ast::PrimaryExpression>() ) {
            switch (p->token().kind()) {
            case lexer::TokenKind::Identifier:
            {
                auto decl = p->declaration();
                if ( !decl )
                    die("unresolved identifier");

                if ( auto dsCtor = decl->as<ast::DataSumDeclaration::Constructor>() )
                    die("dsctor not implemented");

                if ( auto proc = decl->as<ast::ProcedureDeclaration>() ) {
                    auto pdata = customData(*proc);
                    return builder.CreateCall(pdata->body, llvm::None);
                }

                if ( auto var = decl->as<ast::VariableDeclaration>() ) {
                    auto vdata = customData(*var);
                    return vdata->value;
                }

                die("unhandled identifier");
            }

            case lexer::TokenKind::Integer:
                // todo
                return llvm::ConstantInt::get(llvm::Type::getInt32Ty(builder.getContext()),
                                              p->token().lexeme(), 10);

            case lexer::TokenKind::Rational:
                // todo
                return llvm::ConstantFP::get(llvm::Type::getDoubleTy(builder.getContext()),
                                             p->token().lexeme());

            case lexer::TokenKind::String:
                // todo
                return llvm::ConstantDataArray::get(builder.getContext(),
                                                    llvm::ArrayRef<std::uint8_t>(reinterpret_cast<std::uint8_t const*>(p->token().lexeme().c_str()), p->token().lexeme().size() + 1));
            }

            return nullptr;
        }
        else if ( auto a = expr.as<ast::ApplyExpression>() ) {
            std::vector<llvm::Value*> params;
            params.reserve(a->expressions().size() - 1);
            if ( a->expressions().size() > 1 ) {
                for ( auto const& e : a->expressions()(1, a->expressions().size()) ) {
                    if ( e->declaration() == axioms.intrinsic(ast::PointerNullLiteralType) )
                        params.push_back(llvm::ConstantPointerNull::get(static_cast<llvm::PointerType*>(toType(*e))));
                    else
                        params.push_back(toValue(builder, *e));
                }
            }

            auto fun = customData(*a->expressions()[0]->declaration()->as<ast::ProcedureDeclaration>());
            return builder.CreateCall(fun->body, params);
        }
        else if ( auto dot = expr.as<ast::DotExpression>() ) {
            llvm::Value* ret = nullptr;
            auto exprs = dot->expressions();
            for ( auto const& e : exprs ) {
                if ( auto field = e->declaration()->as<ast::DataProductDeclaration::Field>() ) {
                    auto fieldData = customData(*field);
                    ret = builder.CreateStructGEP(ret->getType(), ret, fieldData->index);
                }
                else {
                    ret = toValue(builder, *e);
                }
            }

            return ret;
        }

        return nullptr;
    }

    llvm::Value* intrinsicInstruction(llvm::IRBuilder<>& builder,
                                      ast::Expression const& expr)
    {
        const auto& axioms = sourceModule.axioms();

        if ( auto a = expr.as<ast::ApplyExpression>() ) {
            auto const& exprs = a->expressions();
            auto const decl = a->declaration();
            if ( decl == axioms.intrinsic(ast::Addu1)
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
            else if ( decl == axioms.intrinsic(ast::Addr) ) {
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
    ast::Module& sourceModule;

    std::unique_ptr<llvm::LLVMContext> context;
    std::unique_ptr<llvm::Module> module;

    LLVMState(Diagnostics& dgn,
              ast::Module& sourceModule)
        : dgn(dgn)
        , sourceModule(sourceModule)
        , context(std::make_unique<llvm::LLVMContext>())
        , module(std::make_unique<llvm::Module>(sourceModule.name(), *context))
    {
    }

    Error& error(ast::Declaration const& decl)
    {
        return dgn.error(sourceModule, decl.symbol().identifier()) << "codegen: ";
    }

    Error& error(ast::Expression const& expr)
    {
        return dgn.error(sourceModule, expr) << "codegen: ";
    }

    Error& error(lexer::Token const& token)
    {
        return dgn.error(sourceModule, token) << "codegen: ";
    }

    Error& error()
    {
        return dgn.error(sourceModule) << "codegen: ";
    }

    void die(std::string const& msg)
    {
        error() << msg;
        dgn.die();
    }

    void die()
    {
        dgn.die();
    }

    void generate()
    {
        ast::ShallowApply<InitCodeGenPass> init;
        for ( auto d : sourceModule.scope()->childDeclarations() )
            init(*d);

        for ( auto d : sourceModule.templateInstantiations() )
            init(*d);

        registerTypes(*sourceModule.scope());
        for ( auto d : sourceModule.templateInstantiations() ) {
            if ( d->symbol().prototype().isConcrete() ) {
                auto decl = resolveIndirections(d);
                registerType(*decl);
            }
        }

        ast::ShallowApply<CodeGenPass> gen(dgn, module.get(), sourceModule);
        for ( auto d : sourceModule.scope()->childDeclarations() )
            gen(*d);
    }

    llvm::Type* intrinsicType(ast::Declaration const& decl)
    {
        auto const& axioms = sourceModule.axioms();

        if ( auto ds = decl.as<ast::DataSumDeclaration>() ) {
            auto dsData = customData(*ds);
            if ( dsData->type )
                return dsData->type;

            if ( ds == axioms.intrinsic(ast::IntegerLiteralType )
              || ds == axioms.intrinsic(ast::RationalLiteralType)
              || ds == axioms.intrinsic(ast::StringLiteralType  ) )
            {
                dsData->type = (llvm::Type*)0x1; // todo: choose width based on expression
                return dsData->type;
            }
            else if ( ds == axioms.intrinsic(ast::u1  ) ) return dsData->type = llvm::Type::getInt1Ty  (*context);
            else if ( ds == axioms.intrinsic(ast::u8  ) ) return dsData->type = llvm::Type::getInt8Ty  (*context);
            else if ( ds == axioms.intrinsic(ast::u16 ) ) return dsData->type = llvm::Type::getInt16Ty (*context);
            else if ( ds == axioms.intrinsic(ast::u32 ) ) return dsData->type = llvm::Type::getInt32Ty (*context);
            else if ( ds == axioms.intrinsic(ast::u64 ) ) return dsData->type = llvm::Type::getInt64Ty (*context);
            else if ( ds == axioms.intrinsic(ast::u128) ) return dsData->type = llvm::Type::getInt128Ty(*context);
            else if ( ds == axioms.intrinsic(ast::i8  ) ) return dsData->type = llvm::Type::getInt8Ty  (*context);
            else if ( ds == axioms.intrinsic(ast::i16 ) ) return dsData->type = llvm::Type::getInt16Ty (*context);
            else if ( ds == axioms.intrinsic(ast::i32 ) ) return dsData->type = llvm::Type::getInt32Ty (*context);
            else if ( ds == axioms.intrinsic(ast::i64 ) ) return dsData->type = llvm::Type::getInt64Ty (*context);
            else if ( ds == axioms.intrinsic(ast::i128) ) return dsData->type = llvm::Type::getInt128Ty(*context);

            auto const& sym = ds->symbol();
            if ( rootTemplate(sym) == &axioms.intrinsic(ast::PointerTemplate)->symbol() ) {
                auto t = toType(*sym.prototype().pattern()[0]);
                dsData->type = llvm::PointerType::get(t, 0);
                return dsData->type;
            }
        }

        if ( auto dp = decl.as<ast::DataProductDeclaration>() ) {
            // todo
        }

        return nullptr;
    }

    llvm::Type* registerType(ast::Declaration const& decl)
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

            error(*ds) << "not implemented";
            die();

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
                auto type = registerType(*f->constraint().declaration());
                if ( !type ) {
                    error(*f->constraint().declaration()) << "type is not registered";
                    die();
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

    void registerTypes(ast::DeclarationScope const& scope)
    {
        for ( auto const& decl : scope.childDeclarations() ) {
            auto d = resolveIndirections(decl);
            if ( !d->symbol().prototype().isConcrete() )
                continue;

            registerType(*d);

            if ( auto ds = d->as<ast::DataSumDeclaration>() ) {
                if ( auto defn = ds->definition() )
                    registerTypes(*defn);
            }
            else if ( auto dp = d->as<ast::DataProductDeclaration>() ) {
                if ( auto defn = dp->definition() )
                    registerTypes(*defn);
            }
            else if ( auto proc = d->as<ast::ProcedureDeclaration>() ) {
                if ( auto defn = proc->definition() )
                    registerTypes(*defn);
            }
        }
    }
};

//
// LLVMGenerator

LLVMGenerator::LLVMGenerator(Diagnostics& dgn, ast::Module& sourceModule)
    : myImpl(std::make_unique<LLVMState>(dgn, sourceModule))
{
}

LLVMGenerator::~LLVMGenerator() = default;

void LLVMGenerator::generate()
{
    myImpl->generate();
}

void LLVMGenerator::write(std::experimental::filesystem::path const& path)
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
    auto m = myImpl->module.get();
    m->setTargetTriple(targetTriple);

    std::string err;
    auto target = llvm::TargetRegistry::lookupTarget(targetTriple, err);

    if ( !target ) {
        myImpl->error() << err;
        return;
    }

    auto cpu = "generic";
    auto features = "";

    llvm::TargetOptions opt;
    auto rm = llvm::Optional<llvm::Reloc::Model>();
    auto targetMachine = target->createTargetMachine(targetTriple, cpu, features, opt, rm);

    m->setDataLayout(targetMachine->createDataLayout());

    auto sourcePath = myImpl->sourceModule.path();
    if ( sourcePath.empty() ) {
        myImpl->error() << "cannot determine output name for module";
        return;
    }

    std::error_code ec;
    llvm::raw_fd_ostream outFile(path.string(), ec, llvm::sys::fs::F_None);

    if ( ec ) {
        myImpl->error() << "failed to write object file: " << ec.message();
        return;
    }

    llvm::legacy::PassManager pass;
    if ( targetMachine->addPassesToEmitFile(pass, outFile, llvm::TargetMachine::CGFT_ObjectFile) ) {
        myImpl->error() << "cannot emit a file of this type for target machine " << targetTriple;
        return;
    }

    pass.run(*m);
    outFile.flush();
}

    } // namespace codegen
} // namespace kyfoo
