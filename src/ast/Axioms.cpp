auto source = R"axioms(
:| unsigned<\n : integer>
:| signed<unsigned<\n : integer>>

u1   = unsigned<1  >
u8   = unsigned<8  >
u16  = unsigned<16 >
u32  = unsigned<32 >
u64  = unsigned<64 >
u128 = unsigned<128>

i8   = signed<u8  >
i16  = signed<u16 >
i32  = signed<u32 >
i64  = signed<u64 >
i128 = signed<u128>

:| pointer<\T>

:& array<\T, \card : integer>

:& array<\T>
    base : pointer T
    card : size_t

wordSize = 64
size_t = unsigned<wordSize>

staticSize<\T>(p : pointer T) : size_t => wordSize

add(x : unsigned<1  >, y : unsigned<1  >) : unsigned<1  >
add(x : unsigned<8  >, y : unsigned<8  >) : unsigned<8  >
add(x : unsigned<16 >, y : unsigned<16 >) : unsigned<16 >
add(x : unsigned<32 >, y : unsigned<32 >) : unsigned<32 >
add(x : unsigned<64 >, y : unsigned<64 >) : unsigned<64 >
add(x : unsigned<128>, y : unsigned<128>) : unsigned<128>

add(x : signed<unsigned<8  >>, y : signed<unsigned<8  >>) : signed<unsigned<8  >>
add(x : signed<unsigned<16 >>, y : signed<unsigned<16 >>) : signed<unsigned<16 >>
add(x : signed<unsigned<32 >>, y : signed<unsigned<32 >>) : signed<unsigned<32 >>
add(x : signed<unsigned<64 >>, y : signed<unsigned<64 >>) : signed<unsigned<64 >>
add(x : signed<unsigned<128>>, y : signed<unsigned<128>>) : signed<unsigned<128>>

addr<\T>(=x : T) : pointer T

)axioms";

#include <kyfoo/ast/Axioms.hpp>

#include <kyfoo/ast/Expressions.hpp>
#include <kyfoo/ast/Declarations.hpp>
#include <kyfoo/ast/Module.hpp>
#include <kyfoo/ast/Scopes.hpp>
#include <kyfoo/ast/Semantics.hpp>

namespace kyfoo {
    namespace ast {

//
// AxiomsModule

AxiomsModule::AxiomsModule(ModuleSet* moduleSet,
                           std::string const& name)
    : Module(moduleSet, name)
    , myEmptyLiteralType      (std::make_unique<DataSumDeclaration>(Symbol(""        )))
    , myIntegerLiteralType    (std::make_unique<DataSumDeclaration>(Symbol("integer" )))
    , myRationalLiteralType   (std::make_unique<DataSumDeclaration>(Symbol("rational")))
    , myStringLiteralType     (std::make_unique<DataSumDeclaration>(Symbol("string"  )))
    , myPointerNullLiteralType(std::make_unique<DataSumDeclaration>(Symbol("nil_t"   )))
{
    myDataSumDecls[EmptyLiteralType      ] = myEmptyLiteralType.get();
    myDataSumDecls[IntegerLiteralType    ] = myIntegerLiteralType.get();
    myDataSumDecls[RationalLiteralType   ] = myRationalLiteralType.get();
    myDataSumDecls[StringLiteralType     ] = myStringLiteralType.get();
    myDataSumDecls[PointerNullLiteralType] = myPointerNullLiteralType.get();
}

AxiomsModule::~AxiomsModule() = default;

DataSumDeclaration const* AxiomsModule::emptyLiteralType() const
{
    return myEmptyLiteralType.get();
}

DataSumDeclaration const* AxiomsModule::integerLiteralType() const
{
    return myIntegerLiteralType.get();
}

DataSumDeclaration const* AxiomsModule::rationalLiteralType() const
{
    return myRationalLiteralType.get();
}

DataSumDeclaration const* AxiomsModule::stringLiteralType() const
{
    return myStringLiteralType.get();
}

DataSumDeclaration const* AxiomsModule::pointerNullLiteralType() const
{
    return myPointerNullLiteralType.get();
}

DataSumDeclaration const* AxiomsModule::intrinsic(DataSumIntrinsics i) const
{
    return myDataSumDecls[i];
}

ProcedureDeclaration const* AxiomsModule::intrinsic(InstructionIntrinsics i) const
{
    return myInstructionDecls[i];
}

bool AxiomsModule::isIntrinsic(DataSumDeclaration const& decl) const
{
    for ( std::size_t i = 0; i < DataSumInstrinsicsCount; ++i )
        if ( myDataSumDecls[i] == &decl )
            return true;

    return false;
}

bool AxiomsModule::isIntrinsic(ProcedureDeclaration const& decl) const
{
    for ( std::size_t i = 0; i < InstructionIntrinsicsCount; ++i )
        if ( myInstructionDecls[i] == &decl )
            return true;

    return false;
}

bool AxiomsModule::isIntrinsic(Declaration const& decl) const
{
    if ( auto ds = decl.as<DataSumDeclaration>() )
        return isIntrinsic(*ds);

    if ( auto proc = decl.as<ProcedureDeclaration>() )
        return isIntrinsic(*proc);

    return false;
}

bool AxiomsModule::isLiteral(DataSumDeclaration const& decl) const
{
    for ( std::size_t i = EmptyLiteralType; i <= PointerNullLiteralType; ++i )
        if ( myDataSumDecls[i] == &decl )
            return true;

    return false;
}

bool AxiomsModule::isLiteral(Declaration const& decl) const
{
    if ( auto ds = decl.as<DataSumDeclaration>() )
        return isLiteral(*ds);

    return false;
}

AxiomsModule::IntegerMetaData const* AxiomsModule::integerMetaData(Declaration const& decl) const
{
    for ( auto const& e : myIntegerMetaData )
        if ( e.decl == &decl )
            return &e;

    return nullptr;
}

void AxiomsModule::init(Diagnostics& dgn)
{
    std::stringstream s(source);
    try {
        parse(dgn, s);
        if ( dgn.errorCount() )
            return;

        resolveImports(dgn);
        if ( dgn.errorCount() )
            return;

        semantics(dgn);
        if ( dgn.errorCount() )
            return;

        myDataSumDecls[u1  ] = resolveIndirections(scope()->findEquivalent(dgn, Symbol("u1"  )).decl())->as<DataSumDeclaration>();
        myDataSumDecls[u8  ] = resolveIndirections(scope()->findEquivalent(dgn, Symbol("u8"  )).decl())->as<DataSumDeclaration>();
        myDataSumDecls[u16 ] = resolveIndirections(scope()->findEquivalent(dgn, Symbol("u16" )).decl())->as<DataSumDeclaration>();
        myDataSumDecls[u32 ] = resolveIndirections(scope()->findEquivalent(dgn, Symbol("u32" )).decl())->as<DataSumDeclaration>();
        myDataSumDecls[u64 ] = resolveIndirections(scope()->findEquivalent(dgn, Symbol("u64" )).decl())->as<DataSumDeclaration>();
        myDataSumDecls[u128] = resolveIndirections(scope()->findEquivalent(dgn, Symbol("u128")).decl())->as<DataSumDeclaration>();

        myDataSumDecls[i8  ] = resolveIndirections(scope()->findEquivalent(dgn, Symbol("i8"  )).decl())->as<DataSumDeclaration>();
        myDataSumDecls[i16 ] = resolveIndirections(scope()->findEquivalent(dgn, Symbol("i16" )).decl())->as<DataSumDeclaration>();
        myDataSumDecls[i32 ] = resolveIndirections(scope()->findEquivalent(dgn, Symbol("i32" )).decl())->as<DataSumDeclaration>();
        myDataSumDecls[i64 ] = resolveIndirections(scope()->findEquivalent(dgn, Symbol("i64" )).decl())->as<DataSumDeclaration>();
        myDataSumDecls[i128] = resolveIndirections(scope()->findEquivalent(dgn, Symbol("i128")).decl())->as<DataSumDeclaration>();

        auto childDecls = scope()->childDeclarations();
        for ( auto decl = begin(childDecls); decl != end(childDecls); ++decl )
        {
            auto const& sym = (*decl)->symbol();
            if ( sym.name() == "unsigned" ) {
                for ( int i = UnsignedTemplate; i <= SignedTemplate; ++decl, ++i )
                    myDataSumDecls[i] = (*decl)->as<DataSumDeclaration>();
            }
            else if ( sym.name() == "pointer" ) {
                for ( int i = PointerTemplate; i <= ArrayDynamicTemplate; ++decl, ++i )
                    myDataSumDecls[i] = (*decl)->as<DataSumDeclaration>();
            }
            else if ( sym.name() == "add" ) {
                for ( int i = Addu1; i < InstructionIntrinsicsCount; ++decl, ++i )
                    myInstructionDecls[i] = (*decl)->as<ProcedureDeclaration>();
            }
        }

        buildMetaData();
    }
    catch (Diagnostics*) {
        // fall through
    }
    catch (std::exception const&) {
        // fall through
    }
}

void AxiomsModule::buildMetaData()
{
    myIntegerMetaData[0 ] = IntegerMetaData{ intrinsic(u1  ), 1   };
    myIntegerMetaData[1 ] = IntegerMetaData{ intrinsic(u8  ), 8   };
    myIntegerMetaData[2 ] = IntegerMetaData{ intrinsic(u16 ), 16  };
    myIntegerMetaData[3 ] = IntegerMetaData{ intrinsic(u32 ), 32  };
    myIntegerMetaData[4 ] = IntegerMetaData{ intrinsic(u64 ), 64  };
    myIntegerMetaData[5 ] = IntegerMetaData{ intrinsic(u128), 128 };

    myIntegerMetaData[6 ] = IntegerMetaData{ intrinsic(i8  ), -8   };
    myIntegerMetaData[7 ] = IntegerMetaData{ intrinsic(i16 ), -16  };
    myIntegerMetaData[8 ] = IntegerMetaData{ intrinsic(i32 ), -32  };
    myIntegerMetaData[9 ] = IntegerMetaData{ intrinsic(i64 ), -64  };
    myIntegerMetaData[10] = IntegerMetaData{ intrinsic(i128), -128 };
}

    } // namespace ast
} // namespace kyfoo
