auto source = R"axioms(
:| pointer<\T>

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

wordSize = 64
size_t = unsigned<wordSize>

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

staticSize<\T>(p : pointer T) : size_t => wordSize

:& array<\T, \card : integer>

:& array<\T>
    base : pointer T
    card : size_t
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

AxiomsModule::AxiomsModule(ModuleSet* moduleSet, std::string const& name)
    : Module(moduleSet, name)
    , myEmptyType(std::make_unique<DataSumDeclaration>(Symbol("")))
    , myIntegerType(std::make_unique<DataSumDeclaration>(Symbol("integer")))
    , myRationalType(std::make_unique<DataSumDeclaration>(Symbol("rational")))
    , myStringType(std::make_unique<DataSumDeclaration>(Symbol("string")))
{
    myDataSumDecls[Empty] = myEmptyType.get();
    myDataSumDecls[Integer] = myIntegerType.get();
    myDataSumDecls[Rational] = myRationalType.get();
    myDataSumDecls[String] = myStringType.get();
}

AxiomsModule::~AxiomsModule() = default;

DataSumDeclaration const* AxiomsModule::emptyType() const
{
    return myEmptyType.get();
}

DataSumDeclaration const* AxiomsModule::integerType() const
{
    return myIntegerType.get();
}

DataSumDeclaration const* AxiomsModule::rationalType() const
{
    return myRationalType.get();
}

DataSumDeclaration const* AxiomsModule::stringType() const
{
    return myStringType.get();
}

DataSumDeclaration const* AxiomsModule::intrinsic(DataSumIntrinsics i) const
{
    return myDataSumDecls[i];
}

ProcedureDeclaration const* AxiomsModule::intrinsic(InstructionIntrinsics i) const
{
    return myInstructionDecls[i];
}

bool AxiomsModule::init()
{
    std::stringstream s(source);
    Diagnostics dgn;
    try {
        parse(dgn, s);
        if ( dgn.errorCount() )
            return false;

        resolveImports(dgn);
        if ( dgn.errorCount() )
            return false;

        semantics(dgn);
        if ( dgn.errorCount() )
            return false;

        myDataSumDecls[u1  ] = resolveIndirections(scope()->findEquivalent(Symbol("u1"  )).decl())->as<DataSumDeclaration>();
        myDataSumDecls[u8  ] = resolveIndirections(scope()->findEquivalent(Symbol("u8"  )).decl())->as<DataSumDeclaration>();
        myDataSumDecls[u16 ] = resolveIndirections(scope()->findEquivalent(Symbol("u16" )).decl())->as<DataSumDeclaration>();
        myDataSumDecls[u32 ] = resolveIndirections(scope()->findEquivalent(Symbol("u32" )).decl())->as<DataSumDeclaration>();
        myDataSumDecls[u64 ] = resolveIndirections(scope()->findEquivalent(Symbol("u64" )).decl())->as<DataSumDeclaration>();
        myDataSumDecls[u128] = resolveIndirections(scope()->findEquivalent(Symbol("u128")).decl())->as<DataSumDeclaration>();

        myDataSumDecls[i8  ] = resolveIndirections(scope()->findEquivalent(Symbol("i8"  )).decl())->as<DataSumDeclaration>();
        myDataSumDecls[i16 ] = resolveIndirections(scope()->findEquivalent(Symbol("i16" )).decl())->as<DataSumDeclaration>();
        myDataSumDecls[i32 ] = resolveIndirections(scope()->findEquivalent(Symbol("i32" )).decl())->as<DataSumDeclaration>();
        myDataSumDecls[i64 ] = resolveIndirections(scope()->findEquivalent(Symbol("i64" )).decl())->as<DataSumDeclaration>();
        myDataSumDecls[i128] = resolveIndirections(scope()->findEquivalent(Symbol("i128")).decl())->as<DataSumDeclaration>();

        auto childDecls = scope()->childDeclarations();
        for ( auto decl = begin(childDecls); decl != end(childDecls); ++decl )
        {
            auto const& sym = (*decl)->symbol();
            if ( sym.name() == "pointer" ) {
                for ( int i = PointerTemplate; i <= SignedTemplate; ++decl, ++i )
                    myDataSumDecls[i] = (*decl)->as<DataSumDeclaration>();
            }
            else if ( sym.name() == "add" ) {
                for ( int i = Addu1; i < InstructionIntrinsicsCount; ++decl, ++i )
                    myInstructionDecls[i] = (*decl)->as<ProcedureDeclaration>();
            }
        }

        return true;
    }
    catch (Diagnostics*) {
        // fall through
    }
    catch (std::exception const&) {
        // fall through
    }

    return false;
}

    } // namespace ast
} // namespace kyfoo
