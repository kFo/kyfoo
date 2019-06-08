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
        myDgn.error(module, diag::module_no_name);
        return;
    }

    std::error_code ec;
    ::llvm::raw_fd_ostream outFile(path.string(), ec, ::llvm::sys::fs::F_None);

    if ( ec ) {
        myDgn.error(module, diag::module_cannot_write_object_file);
            // .reason(ec.message()); // todo
        return;
    }

    ::llvm::legacy::PassManager pass;
    if ( myTargetMachine->addPassesToEmitFile(pass, outFile, nullptr, ::llvm::TargetMachine::CGFT_ObjectFile) ) {
        myDgn.error(module, diag::module_unsupported_target);
            // .subject(myTargetTriple);
        return;
    }

    pass.run(*m);
    outFile.flush();
}

void Context::writeIR(ast::Module const& module, std::filesystem::path const& path)
{
    auto sourcePath = module.path();
    if ( sourcePath.empty() ) {
        myDgn.error(module, diag::module_no_name);
        return;
    }

    std::error_code ec;
    ::llvm::raw_fd_ostream outFile(path.string(), ec, ::llvm::sys::fs::F_None);

    if ( ec ) {
        myDgn.error(module, diag::module_cannot_write_object_file);
            // .reason(ec.message()); // todo
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
        myDgn.die("LLVM module errors");
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

void Context::generate(ast::DataTypeDeclaration const& dt)
{
    if ( !dt.symbol().prototype().isConcrete() || dt.codegenData() )
        return;

    dt.setCodegenData(mk<LLVMCustomData<ast::DataTypeDeclaration>>());
    auto dtData = customData(dt);
    if ( auto t = intrinsicType(dt) ) {
        dtData->intrinsic = t;
        return;
    }

    auto defn = dt.definition();
    if ( !defn ) {
        if ( auto s = dt.super() ) {
            ::llvm::Type* fieldTypes[] = { toType(*s) };
            constexpr auto isPacked = false;
            dtData->type = ::llvm::StructType::create(
                *myContext,
                fieldTypes,
                strRef(dt.symbol().token().lexeme()),
                isPacked);
        }

        return;
    }

    std::vector<::llvm::Type*> fieldTypes;
    auto fields = defn->fields();
    auto variations = defn->variations();
    {
        auto n = fields.card();
        if ( variations )
            ++n;

        if ( dt.super() )
            ++n;

        fieldTypes.reserve(n);
    }

    if ( dt.super() )
        fieldTypes.push_back(toType(*dt.super()));

    if ( variations ) {
        auto const bits = trunc<unsigned>(log2(roundUpToPow2(variations.card())));
        dtData->tagType = ::llvm::cast<::llvm::IntegerType>(myDefaultDataLayout->getSmallestLegalIntType(*myContext, bits));
        fieldTypes.push_back(dtData->tagType);
    }

    for ( uz i = 0; i < fields.card(); ++i ) {
        fields[i]->setCodegenData(mk<LLVMCustomData<ast::Field>>());
        customData(*fields[i])->index = static_cast<u32>(i);

        auto d = getDeclaration(fields[i]->type());
        auto type = toType(*resolveIndirections(d));
        if ( !type )
            myDgn.die("type is not registered");

        fieldTypes.push_back(type);
    }

    constexpr auto isPacked = false;
    dtData->type = ::llvm::StructType::create(
        *myContext,
        fieldTypes,
        strRef(dt.symbol().token().lexeme()),
        isPacked);

    if ( variations ) {
        generate(*variations.front());
        dtData->largestSubtype = ::llvm::cast<::llvm::StructType>(toType(*variations.front()));
        uz size = *sizeOf(*variations.front());
        for ( variations.popFront(); variations; variations.popFront() ) {
            generate(*variations.front());
            auto s = *sizeOf(*variations.front());
            if ( s > size ) {
                dtData->largestSubtype = ::llvm::cast<::llvm::StructType>(toType(*variations.front()));
                size = s;
            }
        }
    }
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

    if ( auto dt = d->as<ast::DataTypeDeclaration>() ) {
        auto dtData = customData(*dt);
        if ( dtData->intrinsic )
            return dtData->intrinsic;

        return dtData->type;
    }

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
    auto dt = decl.as<ast::DataTypeDeclaration>();
    if ( !dt )
        return nullptr;

    auto dtData = customData(*dt);
    if ( dtData->intrinsic )
        return dtData->intrinsic;

    if ( dt == axioms().intrinsic(ast::intrin::type::IntegerLiteralType )
      || dt == axioms().intrinsic(ast::intrin::type::RationalLiteralType) )
    {
        dtData->intrinsic = (::llvm::Type*)0x1; // todo: choose width based on expression
        return dtData->intrinsic;
    }

    auto const& sym = dt->symbol();
    if ( rootTemplate(sym) == &axioms().intrinsic(ast::intrin::type::PointerTemplate  )->symbol()
      || rootTemplate(sym) == &axioms().intrinsic(ast::intrin::type::ReferenceTemplate)->symbol() )
    {
        auto t = toType(*sym.prototype().pattern().front());
        if ( t->isVoidTy() )
            dtData->intrinsic = ::llvm::Type::getInt8PtrTy(*myContext);
        else
            dtData->intrinsic = ::llvm::PointerType::getUnqual(t);

        return dtData->intrinsic;
    }

         if ( dt == axioms().intrinsic(ast::intrin::type::u1  ) ) return dtData->intrinsic = ::llvm::Type::getInt1Ty  (*myContext);
    else if ( dt == axioms().intrinsic(ast::intrin::type::u8  ) ) return dtData->intrinsic = ::llvm::Type::getInt8Ty  (*myContext);
    else if ( dt == axioms().intrinsic(ast::intrin::type::u16 ) ) return dtData->intrinsic = ::llvm::Type::getInt16Ty (*myContext);
    else if ( dt == axioms().intrinsic(ast::intrin::type::u32 ) ) return dtData->intrinsic = ::llvm::Type::getInt32Ty (*myContext);
    else if ( dt == axioms().intrinsic(ast::intrin::type::u64 ) ) return dtData->intrinsic = ::llvm::Type::getInt64Ty (*myContext);
    else if ( dt == axioms().intrinsic(ast::intrin::type::u128) ) return dtData->intrinsic = ::llvm::Type::getInt128Ty(*myContext);
    else if ( dt == axioms().intrinsic(ast::intrin::type::s8  ) ) return dtData->intrinsic = ::llvm::Type::getInt8Ty  (*myContext);
    else if ( dt == axioms().intrinsic(ast::intrin::type::s16 ) ) return dtData->intrinsic = ::llvm::Type::getInt16Ty (*myContext);
    else if ( dt == axioms().intrinsic(ast::intrin::type::s32 ) ) return dtData->intrinsic = ::llvm::Type::getInt32Ty (*myContext);
    else if ( dt == axioms().intrinsic(ast::intrin::type::s64 ) ) return dtData->intrinsic = ::llvm::Type::getInt64Ty (*myContext);
    else if ( dt == axioms().intrinsic(ast::intrin::type::s128) ) return dtData->intrinsic = ::llvm::Type::getInt128Ty(*myContext);

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
