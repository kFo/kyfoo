auto source = R"axioms(
:| unsigned<\n : integer>
:| signed<unsigned<\n : integer>>
:| pointer<\T>

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

ascii = slice<u8>

:& array<\T, \card : integer>

:& array<\T>
    base : pointer T
    card : size_t

:& slice<\T>
    base : pointer T
    card : size_t

:& slice<u8>
    base : pointer u8
    card : size_t

    ctor(s : string)

wordSize = 64
size_t = unsigned<wordSize>

staticSize(p : pointer \T) : size_t => wordSize

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

trunc<unsigned<1 >>(x : unsigned<8  >) : unsigned<1 >
trunc<unsigned<1 >>(x : unsigned<16 >) : unsigned<1 >
trunc<unsigned<1 >>(x : unsigned<32 >) : unsigned<1 >
trunc<unsigned<1 >>(x : unsigned<64 >) : unsigned<1 >
trunc<unsigned<1 >>(x : unsigned<128>) : unsigned<1 >
trunc<unsigned<8 >>(x : unsigned<16 >) : unsigned<8 >
trunc<unsigned<8 >>(x : unsigned<32 >) : unsigned<8 >
trunc<unsigned<8 >>(x : unsigned<64 >) : unsigned<8 >
trunc<unsigned<8 >>(x : unsigned<128>) : unsigned<8 >
trunc<unsigned<16>>(x : unsigned<32 >) : unsigned<16>
trunc<unsigned<16>>(x : unsigned<64 >) : unsigned<16>
trunc<unsigned<16>>(x : unsigned<128>) : unsigned<16>
trunc<unsigned<32>>(x : unsigned<64 >) : unsigned<32>
trunc<unsigned<32>>(x : unsigned<128>) : unsigned<32>
trunc<unsigned<64>>(x : unsigned<128>) : unsigned<64>
trunc<signed<unsigned<8 >>>(x : signed<unsigned<16 >>) : signed<unsigned<8 >>
trunc<signed<unsigned<8 >>>(x : signed<unsigned<32 >>) : signed<unsigned<8 >>
trunc<signed<unsigned<8 >>>(x : signed<unsigned<64 >>) : signed<unsigned<8 >>
trunc<signed<unsigned<8 >>>(x : signed<unsigned<128>>) : signed<unsigned<8 >>
trunc<signed<unsigned<16>>>(x : signed<unsigned<32 >>) : signed<unsigned<16>>
trunc<signed<unsigned<16>>>(x : signed<unsigned<64 >>) : signed<unsigned<16>>
trunc<signed<unsigned<16>>>(x : signed<unsigned<128>>) : signed<unsigned<16>>
trunc<signed<unsigned<32>>>(x : signed<unsigned<64 >>) : signed<unsigned<32>>
trunc<signed<unsigned<32>>>(x : signed<unsigned<128>>) : signed<unsigned<32>>
trunc<signed<unsigned<64>>>(x : signed<unsigned<128>>) : signed<unsigned<64>>

addr(=p : \T) : pointer T

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
{
    myScope = std::make_unique<ast::DeclarationScope>(*this);

    myScope->append(std::make_unique<DataSumDeclaration>(Symbol(lexer::Token(lexer::TokenKind::Identifier, 0, 0, ""        ))));
    myScope->append(std::make_unique<DataSumDeclaration>(Symbol(lexer::Token(lexer::TokenKind::Identifier, 0, 0, "integer" ))));
    myScope->append(std::make_unique<DataSumDeclaration>(Symbol(lexer::Token(lexer::TokenKind::Identifier, 0, 0, "rational"))));
    myScope->append(std::make_unique<DataSumDeclaration>(Symbol(lexer::Token(lexer::TokenKind::Identifier, 0, 0, "string"  ))));
    myScope->append(std::make_unique<DataSumDeclaration>(Symbol(lexer::Token(lexer::TokenKind::Identifier, 0, 0, "null_t"  ))));

    myDataSumDecls[EmptyLiteralType      ] = static_cast<DataSumDeclaration*>(scope()->childDeclarations()[0]);
    myDataSumDecls[IntegerLiteralType    ] = static_cast<DataSumDeclaration*>(scope()->childDeclarations()[1]);
    myDataSumDecls[RationalLiteralType   ] = static_cast<DataSumDeclaration*>(scope()->childDeclarations()[2]);
    myDataSumDecls[StringLiteralType     ] = static_cast<DataSumDeclaration*>(scope()->childDeclarations()[3]);
    myDataSumDecls[PointerNullLiteralType] = static_cast<DataSumDeclaration*>(scope()->childDeclarations()[4]);
}

AxiomsModule::~AxiomsModule() = default;

DataSumDeclaration const* AxiomsModule::intrinsic(DataSumIntrinsics i) const
{
    return myDataSumDecls[i];
}

DataProductDeclaration const* AxiomsModule::intrinsic(DataProductIntrinsics i) const
{
    return myDataProductDecls[i];
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

bool AxiomsModule::isIntrinsic(DataProductDeclaration const& decl) const
{
    for ( std::size_t i = 0; i < DataProductIntrinsicsCount; ++i )
        if ( myDataProductDecls[i] == &decl )
            return true;

    return false;
}

bool AxiomsModule::isIntrinsic(ProcedureDeclaration const& decl) const
{
    for ( std::size_t i = 0; i < InstructionIntrinsicsCount; ++i )
        if ( descendsFromTemplate(myInstructionDecls[i]->symbol(), decl.symbol()) )
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
    for ( std::size_t i = 0; i < DataSumInstrinsicsCount; ++i )
        if ( myDataSumDecls[i] == &decl )
            return true;

    return false;
}

bool AxiomsModule::isLiteral(DataProductDeclaration const& decl) const
{
    for ( std::size_t i = 0; i < DataProductIntrinsicsCount; ++i )
        if ( myDataProductDecls[i] == &decl )
            return true;

    return false;
}

bool AxiomsModule::isLiteral(Declaration const& decl) const
{
    if ( auto ds = decl.as<DataSumDeclaration>() )
        return isLiteral(*ds);
    else if ( auto dp = decl.as<DataProductDeclaration>() )
        return isLiteral(*dp);

    return false;
}

AxiomsModule::IntegerMetaData const* AxiomsModule::integerMetaData(Declaration const& decl) const
{
    for ( auto const& e : myIntegerMetaData )
        if ( e.decl == &decl )
            return &e;

    return nullptr;
}

bool AxiomsModule::init(Diagnostics& dgn)
{
    std::stringstream s(source);
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

        myDataSumDecls[u1  ] = resolveIndirections(scope()->findEquivalent(dgn, "u1"  ).decl())->as<DataSumDeclaration>();
        myDataSumDecls[u8  ] = resolveIndirections(scope()->findEquivalent(dgn, "u8"  ).decl())->as<DataSumDeclaration>();
        myDataSumDecls[u16 ] = resolveIndirections(scope()->findEquivalent(dgn, "u16" ).decl())->as<DataSumDeclaration>();
        myDataSumDecls[u32 ] = resolveIndirections(scope()->findEquivalent(dgn, "u32" ).decl())->as<DataSumDeclaration>();
        myDataSumDecls[u64 ] = resolveIndirections(scope()->findEquivalent(dgn, "u64" ).decl())->as<DataSumDeclaration>();
        myDataSumDecls[u128] = resolveIndirections(scope()->findEquivalent(dgn, "u128").decl())->as<DataSumDeclaration>();

        myDataSumDecls[i8  ] = resolveIndirections(scope()->findEquivalent(dgn, "i8"  ).decl())->as<DataSumDeclaration>();
        myDataSumDecls[i16 ] = resolveIndirections(scope()->findEquivalent(dgn, "i16" ).decl())->as<DataSumDeclaration>();
        myDataSumDecls[i32 ] = resolveIndirections(scope()->findEquivalent(dgn, "i32" ).decl())->as<DataSumDeclaration>();
        myDataSumDecls[i64 ] = resolveIndirections(scope()->findEquivalent(dgn, "i64" ).decl())->as<DataSumDeclaration>();
        myDataSumDecls[i128] = resolveIndirections(scope()->findEquivalent(dgn, "i128").decl())->as<DataSumDeclaration>();

        myDataProductDecls[Sliceu8] = resolveIndirections(scope()->findEquivalent(dgn, "ascii").decl())->as<DataProductDeclaration>();

        auto childDecls = scope()->childDeclarations();
        auto decl = begin(childDecls) + (UnsignedTemplate - EmptyLiteralType);
        for ( int i = UnsignedTemplate; i <= PointerTemplate; ++i )
            myDataSumDecls[i] = (*decl++)->as<DataSumDeclaration>();

        while ( (*decl)->symbol().identifier().lexeme() != "array" )
            ++decl;

        for ( int i = ArrayStaticTemplate; i < DataProductIntrinsicsCount; ++i )
            myDataProductDecls[i] = (*decl++)->as<DataProductDeclaration>();

        while ( (*decl)->symbol().identifier().lexeme() != "add" )
            ++decl;

        int i = Addu1;
        while ( (*decl)->symbol().identifier().lexeme() == "add" ) {
            auto defn = (*decl)->as<TemplateDeclaration>()->definition();
            for ( auto& d : defn->childDeclarations() ) {
                myInstructionDecls[i] = d->as<ProcedureDeclaration>();
                ++i;
                ++decl;
            }
        }

        i = Truncu1u8;
        while ( (*decl)->symbol().identifier().lexeme() == "trunc" ) {
            auto defn = (*decl)->as<TemplateDeclaration>()->definition();
            for ( auto& d : defn->childDeclarations() ) {
                myInstructionDecls[i] = d->as<ProcedureDeclaration>();
                ++i;
                ++decl;
            }
        }

        if ( (*decl)->symbol().identifier().lexeme() == "addr" ) {
            auto defn = (*decl)->as<TemplateDeclaration>()->definition();
            myInstructionDecls[Addr] = defn->childDeclarations()[0]->as<ProcedureDeclaration>();
        }

        myInstructionDecls[Sliceu8_ctor] = myDataProductDecls[Sliceu8]
                                               ->definition()
                                               ->childDeclarations()[2]
                                               ->as<TemplateDeclaration>()
                                               ->definition()
                                               ->childDeclarations()[0]
                                               ->as<ProcedureDeclaration>();

        buildMetaData();

        return true;
    }
    catch (Diagnostics*) {
        // fall through
    }
    catch (std::exception const&) {
        throw;
    }

    return false;
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
