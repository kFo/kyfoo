#include "Context.hpp"
#include "Visitors.hpp"

namespace kyfoo::codegen::llvm {

Context::Context(Diagnostics& dgn,
                 ast::ModuleSet& moduleSet,
                 stringv targetTriple)
    : myDgn(dgn)
    , myModuleSet(moduleSet)
    , myContext(mk<::llvm::LLVMContext>())
{
    init(mkString(targetTriple));
}

Context::~Context()
{
    for ( auto& m : myModuleSet.modules() )
        m->setCodegenData(nullptr);
}

void Context::write(ast::Module const& module, std::filesystem::path const& path)
{
    auto m = customData(module)->module.get();

    auto sourcePath = module.path();
    if ( sourcePath.empty() ) {
        myDgn.error(module) << "cannot determine output name for module";
        return;
    }

    std::error_code ec;
    ::llvm::raw_fd_ostream outFile(path.string(), ec, ::llvm::sys::fs::F_None);

    if ( ec ) {
        myDgn.error(module) << "failed to write object file: " << ec.message();
        return;
    }

    ::llvm::legacy::PassManager pass;
    if ( myTargetMachine->addPassesToEmitFile(pass, outFile, nullptr, ::llvm::TargetMachine::CGFT_ObjectFile) ) {
        myDgn.error(module) << "cannot emit a file of this type for target machine " << myTargetTriple;
        return;
    }

    pass.run(*m);
    outFile.flush();
}

void Context::writeIR(ast::Module const& module, std::filesystem::path const& path)
{
    auto sourcePath = module.path();
    if ( sourcePath.empty() ) {
        myDgn.error(module) << "cannot determine output name for module";
        return;
    }

    std::error_code ec;
    ::llvm::raw_fd_ostream outFile(path.string(), ec, ::llvm::sys::fs::F_None);

    if ( ec ) {
        myDgn.error(module) << "failed to write IR file: " << ec.message();
        return;
    }

    customData(module)->module->print(outFile, nullptr);
}

void Context::generate(ast::Module const& module)
{
    module.setCodegenData(mk<LLVMCustomData<ast::Module>>());
    auto mdata = customData(module);
    mdata->module = mk<::llvm::Module>(mkString(module.name()), *myContext);
    mdata->module->setTargetTriple(myTargetTriple);
    mdata->module->setDataLayout(*myDefaultDataLayout);

    ast::ShallowApply<CodeGenPass> gen(myDgn, *this, mdata->module.get(), module);
    for ( auto d : module.templateInstantiations() )
        gen(*d);
    for ( auto d : module.scope()->childDeclarations() )
        gen(*d);
    for ( auto d : module.scope()->childLambdas() )
        gen(*d);

    gen.getOperator().generateProcBodies();

    if ( verifyModule(*mdata->module, &::llvm::errs()) ) {
#ifndef NDEBUG
        mdata->module->dump();
#endif
        myDgn.die();
    }
}

void Context::generate(ast::Declaration const& decl)
{
    if ( decl.codegenData() )
        return;

    auto& mod = decl.scope().module();
    auto mdata = customData(mod);
    ast::ShallowApply<CodeGenPass> gen(myDgn, *this, mdata->module.get(), mod);
    gen(decl);
}

void Context::generate(ast::DataSumDeclaration const& ds)
{
    if ( !ds.symbol().prototype().isConcrete() || ds.codegenData() )
        return;

    ds.setCodegenData(mk<LLVMCustomData<ast::DataSumDeclaration>>());
    auto dsData = customData(ds);

    if ( auto t = intrinsicType(ds) ) {
        dsData->type = t;
        return;
    }

    auto defn = ds.definition();
    if ( !defn )
        return;

    auto ctors = defn->constructors();
    if ( !ctors )
        return;

    dsData->biggestCtor = toType(*ctors.front());
    uz size = *sizeOf(*ctors.front());
    for ( ctors.popFront(); ctors; ctors.popFront() ) {
        auto s = *sizeOf(*ctors.front());
        if ( s > size ) {
            dsData->biggestCtor = toType(*ctors.front());
            size = s;
        }
    }

    auto const bits = roundUpToPow2(defn->constructors().card());
    dsData->tagType = myDefaultDataLayout->getSmallestLegalIntType(*myContext, bits);

    ::llvm::Type* fieldTypes[] = { dsData->tagType, dsData->biggestCtor };
    constexpr auto isPacked = false;
    dsData->type = ::llvm::StructType::create(*myContext,
                                              fieldTypes,
                                              strRef(ds.symbol().token().lexeme()),
                                              isPacked);
}

void Context::generate(ast::DataProductDeclaration const& dp)
{
    if ( !dp.symbol().prototype().isConcrete() || dp.codegenData() )
        return;

    dp.setCodegenData(mk<LLVMCustomData<ast::DataProductDeclaration>>());
    auto dpData = customData(dp);
    if ( auto t = intrinsicType(dp) ) {
        dpData->type = t;
        return;
    }

    auto defn = dp.definition();
    if ( !defn )
        return;

    std::vector<::llvm::Type*> fieldTypes;
    auto const& fields = defn->fields();
    fieldTypes.reserve(fields.card());
    for ( uz i = 0; i < fields.card(); ++i ) {
        generate(*fields[i]);
        customData(*fields[i])->index = static_cast<u32>(i);

        auto d = getDeclaration(fields[i]->type());
        auto type = toType(*resolveIndirections(d));
        if ( !type ) {
            myDgn.error(d->scope().module(), *d) << "type is not registered";
            myDgn.die();
        }

        fieldTypes.push_back(type);
    }

    constexpr auto isPacked = false;
    dpData->type = ::llvm::StructType::create(*myContext,
                                              fieldTypes,
                                              strRef(dp.symbol().token().lexeme()),
                                              isPacked);
}

std::optional<uz> Context::sizeOf(ast::Declaration const& decl)
{
    auto type = toType(decl);
    if ( !type )
        return {};

    return myDefaultDataLayout->getTypeAllocSize(type);
}

std::optional<uz> Context::sizeOf(ast::Expression const& expr)
{
    auto type = toType(expr);
    if ( !type )
        return {};

    return myDefaultDataLayout->getTypeAllocSize(type);
}

::llvm::Type* Context::toType(ast::Declaration const& decl)
{
    auto d = resolveIndirections(&decl);
    if ( !d->codegenData() )
        generate(*d);

    if ( auto ds = d->as<ast::DataSumDeclaration>() )
        return customData(*ds)->type;

    if ( auto dp = d->as<ast::DataProductDeclaration>() )
        return customData(*dp)->type;

    return nullptr;
}

::llvm::Type* Context::toType(ast::Expression const& expr)
{
    auto e = resolveIndirections(&expr);
    if ( auto decl = getDeclaration(*e) )
        return toType(*decl);

    if ( auto t = e->as<ast::TupleExpression>() ) {
        if ( !t->expressions() && t->kind() == ast::TupleKind::Open )
            return ::llvm::Type::getVoidTy(*myContext);

        ::llvm::Type* elementType = nullptr;
        if ( t->expressions().card() > 1 ) {
            std::vector<::llvm::Type*> types;
            types.reserve(t->expressions().card());
            for ( auto const& te : t->expressions() )
                types.push_back(toType(*te));

            elementType = ::llvm::StructType::get(*myContext, types);
        }
        else {
            elementType = toType(*t->expressions()[0]);
        }

        if ( t->elementsCount() > 1 )
            return ::llvm::ArrayType::get(elementType, t->elementsCount());

        return elementType;
    }

    if ( auto a = e->as<ast::ArrowExpression>() ) {
        enum { NotVarArg = false };
        std::vector<::llvm::Type*> params;
        if ( auto tup = a->from().as<ast::TupleExpression>() ) {
            if ( !tup->expressions() )
                return ::llvm::PointerType::getUnqual(::llvm::FunctionType::get(toType(a->to()), NotVarArg));

            if ( tup->kind() == ast::TupleKind::Open ) {
                params.reserve(tup->expressions().card());
                for ( auto texpr : tup->expressions() )
                    params.push_back(toType(*texpr));
            }
        }

        if ( params.empty() )
            params.push_back(toType(a->from()));

        return ::llvm::PointerType::getUnqual(::llvm::FunctionType::get(toType(a->to()), params, NotVarArg));
    }

    return nullptr;
}

::llvm::Type* Context::intrinsicType(ast::Declaration const& decl)
{
    if ( auto ds = decl.as<ast::DataSumDeclaration>() ) {
        auto dsData = customData(*ds);
        if ( dsData->type )
            return dsData->type;

        if ( ds == axioms().intrinsic(ast::IntegerLiteralType )
            || ds == axioms().intrinsic(ast::RationalLiteralType) )
        {
            dsData->type = (::llvm::Type*)0x1; // todo: choose width based on expression
            return dsData->type;
        }

        auto const& sym = ds->symbol();
        if ( rootTemplate(sym) == &axioms().intrinsic(ast::PointerTemplate)->symbol()
            || rootTemplate(sym) == &axioms().intrinsic(ast::ReferenceTemplate)->symbol() )
        {
            auto t = toType(*sym.prototype().pattern()[0]);
            if ( t->isVoidTy() )
                dsData->type = ::llvm::Type::getInt8PtrTy(*myContext);
            else
                dsData->type = ::llvm::PointerType::getUnqual(t);
            return dsData->type;
        }
    }

    if ( auto dp = decl.as<ast::DataProductDeclaration>() ) {
        auto dpData = customData(*dp);
        if ( dpData->type )
            return dpData->type;

             if ( dp == axioms().intrinsic(ast::u1  ) ) return dpData->type = ::llvm::Type::getInt1Ty  (*myContext);
        else if ( dp == axioms().intrinsic(ast::u8  ) ) return dpData->type = ::llvm::Type::getInt8Ty  (*myContext);
        else if ( dp == axioms().intrinsic(ast::u16 ) ) return dpData->type = ::llvm::Type::getInt16Ty (*myContext);
        else if ( dp == axioms().intrinsic(ast::u32 ) ) return dpData->type = ::llvm::Type::getInt32Ty (*myContext);
        else if ( dp == axioms().intrinsic(ast::u64 ) ) return dpData->type = ::llvm::Type::getInt64Ty (*myContext);
        else if ( dp == axioms().intrinsic(ast::u128) ) return dpData->type = ::llvm::Type::getInt128Ty(*myContext);
        else if ( dp == axioms().intrinsic(ast::s8  ) ) return dpData->type = ::llvm::Type::getInt8Ty  (*myContext);
        else if ( dp == axioms().intrinsic(ast::s16 ) ) return dpData->type = ::llvm::Type::getInt16Ty (*myContext);
        else if ( dp == axioms().intrinsic(ast::s32 ) ) return dpData->type = ::llvm::Type::getInt32Ty (*myContext);
        else if ( dp == axioms().intrinsic(ast::s64 ) ) return dpData->type = ::llvm::Type::getInt64Ty (*myContext);
        else if ( dp == axioms().intrinsic(ast::s128) ) return dpData->type = ::llvm::Type::getInt128Ty(*myContext);
    }

    return nullptr;
}

void Context::init(std::string targetTriple)
{
    /*
    InitializeAllTargetInfos();
    InitializeAllTargets();
    InitializeAllTargetMCs();
    */
    ::llvm::InitializeNativeTarget();

    /*
    InitializeAllAsmParsers();
    */
    ::llvm::InitializeNativeTargetAsmParser();

    /*
    InitializeAllAsmPrinters();
    */
    ::llvm::InitializeNativeTargetAsmPrinter();

    if ( targetTriple.empty() )
        targetTriple = ::llvm::sys::getDefaultTargetTriple();

    myTargetTriple = std::move(targetTriple);

    std::string err;
    auto target = ::llvm::TargetRegistry::lookupTarget(myTargetTriple, err);
    ENFORCE(target, err);

    auto cpu = "generic";
    auto features = "";

    ::llvm::TargetOptions opt;
    auto rm = ::llvm::Optional<::llvm::Reloc::Model>();
    myTargetMachine = target->createTargetMachine(myTargetTriple, cpu, features, opt, rm);
    myDefaultDataLayout = mk<::llvm::DataLayout>(myTargetMachine->createDataLayout());
}

ast::AxiomsModule const& Context::axioms() const
{
    return myModuleSet.axioms();
}

} // kyfoo::codegen::llvm
