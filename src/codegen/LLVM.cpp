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
struct LLVMCustomData<ast::DataSumDeclaration> : public CustomData
{
    llvm::Type* type = nullptr;
};

template<>
struct LLVMCustomData<ast::DataProductDeclaration> : public CustomData
{
    llvm::Type* type = nullptr;
};

template <typename T>
LLVMCustomData<T>* customData(T const& decl)
{
    return static_cast<LLVMCustomData<T>*>(decl.codegenData());
}

llvm::Type* registerType(llvm::LLVMContext& context, ast::Declaration const& decl)
{
    if ( auto ds = decl.as<ast::DataSumDeclaration>() ) {
        // todo
        if ( ds->symbol().name() == "i8" )
            return llvm::Type::getInt8Ty(context);

        if ( ds->symbol().name() == "i32" )
            return llvm::Type::getInt32Ty(context);

        if ( ds->symbol().name() == "Pi32" )
            return llvm::Type::getInt32PtrTy(context);

        return nullptr;
    }

    if ( auto dp = decl.as<ast::DataProductDeclaration>() ) {
        auto dpData = customData(*dp);
        if ( dpData->type )
            return dpData->type;

        auto defn = dp->definition();
        if ( !defn )
            throw std::runtime_error("codegen: typereg error");

        std::vector<llvm::Type*> fieldTypes;
        fieldTypes.reserve(defn->fields().size());
        for ( auto& f : defn->fields() ) {
            auto type = registerType(context, *f->constraint()->declaration());
            if ( !type )
                throw std::runtime_error("codegen: type registration error");

            fieldTypes.push_back(type);
        }

        dpData->type = llvm::StructType::create(context, fieldTypes, dp->symbol().name(), /*isPacked*/false);
        return dpData->type;
    }

    return nullptr;
}

llvm::Type* toType(ast::Expression const& expr)
{
    auto decl = expr.declaration();
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
        decl.setCodegenData(std::make_unique<LLVMCustomData<ast::DataSumDeclaration>>());
    }

    result_t declDataSumCtor(ast::DataSumDeclaration::Constructor const& decl)
    {
        decl.setCodegenData(std::make_unique<LLVMCustomData<ast::DataSumDeclaration::Constructor>>());
    }

    result_t declDataProduct(ast::DataProductDeclaration const& decl)
    {
        decl.setCodegenData(std::make_unique<LLVMCustomData<ast::DataProductDeclaration>>());
    }

    result_t declSymbol(ast::SymbolDeclaration const&)
    {
        // nop
    }

    result_t declProcedure(ast::ProcedureDeclaration const& decl)
    {
        decl.setCodegenData(std::make_unique<LLVMCustomData<ast::ProcedureDeclaration>>());

        declVariable(*decl.result());
        for ( auto const& p : decl.parameters() )
            declVariable(*p);

        if ( auto defn = decl.definition() )
            for ( auto& e : defn->childDeclarations() )
                dispatch(*e);
    }

    result_t declVariable(ast::VariableDeclaration const& decl)
    {
        decl.setCodegenData(std::make_unique<LLVMCustomData<ast::VariableDeclaration>>());
    }

    result_t declImport(ast::ImportDeclaration const&)
    {
        // nop
    }

    result_t declSymbolVariable(ast::SymbolVariable const&)
    {
        // nop
    }
};

//
// RegisterTypesPass

template <typename Dispatcher>
struct RegisterTypesPass
{
    using result_t = void;
    Dispatcher& dispatch;
    Diagnostics& dgn;
    llvm::Module* module;

    RegisterTypesPass(Dispatcher& dispatch,
                      Diagnostics& dgn,
                      llvm::Module* module)
        : dispatch(dispatch)
        , dgn(dgn)
        , module(module)
    {
    }

    result_t declDataSum(ast::DataSumDeclaration const& ds)
    {
        auto data = customData(ds);
        if ( !data->type )
            data->type = registerType(module->getContext(), ds);
    }

    result_t declDataSumCtor(ast::DataSumDeclaration::Constructor const&)
    {
        // nop
    }

    result_t declDataProduct(ast::DataProductDeclaration const& dp)
    {
        auto data = customData(dp);
        if ( !data->type )
            data->type = registerType(module->getContext(), dp);
    }

    result_t declSymbol(ast::SymbolDeclaration const&)
    {
        // nop
    }

    result_t declProcedure(ast::ProcedureDeclaration const& decl)
    {
        if ( auto defn = decl.definition() )
            for ( auto& e : defn->childDeclarations() )
                dispatch(*e);
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
    ast::Module* sourceModule;

    CodeGenPass(Dispatcher& dispatch,
                Diagnostics& dgn,
                llvm::Module* module,
                ast::Module* sourceModule)
        : dispatch(dispatch)
        , dgn(dgn)
        , module(module)
        , sourceModule(sourceModule)
    {
    }

    result_t declDataSum(ast::DataSumDeclaration const& )
    {
        // todo
    }

    result_t declDataSumCtor(ast::DataSumDeclaration::Constructor const& )
    {
        // nop
    }

    result_t declDataProduct(ast::DataProductDeclaration const& )
    {
        // todo
    }

    result_t declSymbol(ast::SymbolDeclaration const& )
    {
        // nop
    }

    result_t declProcedure(ast::ProcedureDeclaration const& decl)
    {
        auto fun = customData(decl);

        auto returnType = toType(*decl.returnType());
        std::vector<llvm::Type*> params;
        params.reserve(decl.parameters().size());
        for ( auto const& p : decl.parameters() )
            params.push_back(toType(*p->constraint()));

        fun->proto = llvm::FunctionType::get(returnType, params, /*isVarArg*/false);

        if ( !decl.definition() ) {
            // hack: intrinsics
            fun->body = llvm::Function::Create(fun->proto,
                                               llvm::Function::ExternalLinkage, // todo
                                               decl.symbol().name(),
                                               module);

            auto bb = llvm::BasicBlock::Create(module->getContext(), "entry", fun->body);
            llvm::IRBuilder<> builder(bb);

            if ( decl.symbol().name() == "add" ) {
                auto args = fun->body->arg_begin();
                auto p1 = &*args;
                ++args;
                auto p2 = &*args;
                auto add = builder.CreateAdd(p1, p2);
                builder.CreateRet(add);
            }

            return;
        }

        fun->body = llvm::Function::Create(fun->proto,
                                           llvm::Function::ExternalLinkage, // todo
                                           decl.symbol().name(),
                                           module);

        {
            auto arg = fun->body->arg_begin();
            for ( auto const& p : decl.parameters() )
                customData(*static_cast<ast::VariableDeclaration const*>(p))->value = &*(arg++);
        }

        auto bb = llvm::BasicBlock::Create(module->getContext(), "entry", fun->body);
        llvm::IRBuilder<> builder(bb);

        llvm::Instruction* inst = nullptr;
        for ( auto const& e : decl.definition()->expressions() ) {
            inst = addInstruction(builder, *e);
            if ( !inst )
                die("invalid instruction");
        }

        // todo
        builder.CreateRet(inst);
    }

    result_t declVariable(ast::VariableDeclaration const& )
    {
        // nop
    }

    result_t declImport(ast::ImportDeclaration const& )
    {
        // nop
    }

    result_t declSymbolVariable(ast::SymbolVariable const& )
    {
        // nop
    }

private:
    void die(const char* msg)
    {
        dgn.error(sourceModule) << "codegen: " << msg;
        dgn.die();
    }

    llvm::Value* toValue(llvm::IRBuilder<>& builder, ast::Expression const& expr)
    {
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

            case lexer::TokenKind::Decimal:
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

        return addInstruction(builder, expr);
    }

    llvm::Instruction* addInstruction(llvm::IRBuilder<>& builder,
                                      ast::Expression const& expr)
    {
        if ( auto p = expr.as<ast::PrimaryExpression>() ) {
            auto decl = p->declaration();
            if ( !decl )
                return nullptr;

            auto procDecl = decl->as<ast::ProcedureDeclaration>();
            if ( !procDecl )
                return nullptr;

            return builder.CreateCall(customData(*procDecl)->body, llvm::None);
        }

        if ( auto a = expr.as<ast::ApplyExpression>() ) {
            std::vector<llvm::Value*> params;
            params.reserve(a->expressions().size() - 1);
            if ( a->expressions().size() > 1 )
                for ( auto const& e : a->expressions()(1, a->expressions().size()) )
                    params.push_back(toValue(builder, *e));

            auto fun = customData(*a->expressions()[0]->declaration()->as<ast::ProcedureDeclaration>());
            return builder.CreateCall(fun->body, params);
        }

        return nullptr;
    }
};

//
// LLVMGenerator

struct LLVMGenerator::LLVMState
{
    LLVMState(std::string const& moduleName)
        : context(std::make_unique<llvm::LLVMContext>())
        , module(std::make_unique<llvm::Module>(moduleName, *context))
    {
    }

    std::unique_ptr<llvm::LLVMContext> context;
    std::unique_ptr<llvm::Module> module;
};

LLVMGenerator::LLVMGenerator(Diagnostics& dgn, ast::Module* sourceModule)
    : myDiagnostics(dgn)
    , mySourceModule(sourceModule)
    , myImpl(std::make_unique<LLVMState>(mySourceModule->name()))
{
}

LLVMGenerator::~LLVMGenerator() = default;

void LLVMGenerator::generate()
{
    ast::ShallowApply<InitCodeGenPass> init;
    for ( auto d : mySourceModule->scope()->childDeclarations() )
        init(*d);

    ast::ShallowApply<RegisterTypesPass> regTypes(myDiagnostics,
                                                  myImpl->module.get());
    for ( auto d : mySourceModule->scope()->childDeclarations() )
        regTypes(*d);

    ast::ShallowApply<CodeGenPass> gen(myDiagnostics,
                                       myImpl->module.get(),
                                       mySourceModule);
    for ( auto d : mySourceModule->scope()->childDeclarations() )
        gen(*d);
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
        error() << err;
        return;
    }

    auto cpu = "generic";
    auto features = "";

    llvm::TargetOptions opt;
    auto rm = llvm::Optional<llvm::Reloc::Model>();
    auto targetMachine = target->createTargetMachine(targetTriple, cpu, features, opt, rm);

    m->setDataLayout(targetMachine->createDataLayout());

    auto sourcePath = mySourceModule->path();
    if ( sourcePath.empty() ) {
        error() << "cannot determine output name for module";
        return;
    }

    std::error_code ec;
    llvm::raw_fd_ostream outFile(path.string(), ec, llvm::sys::fs::F_None);

    if ( ec ) {
        error() << "failed to write object file: " << ec.message();
        return;
    }

    llvm::legacy::PassManager pass;
    if ( targetMachine->addPassesToEmitFile(pass, outFile, llvm::TargetMachine::CGFT_ObjectFile) ) {
        error() << "cannot emit a file of this type for target machine " << targetTriple;
        return;
    }

    pass.run(*m);
    outFile.flush();
}

Error& LLVMGenerator::error()
{
    return myDiagnostics.error(mySourceModule) << "codegen: ";
}

    } // namespace codegen
} // namespace kyfoo
