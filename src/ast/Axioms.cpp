auto source = R"axioms(
@"intrininst" "UnsignedTemplate"
:& unsigned<\n : integer>
    @"intrininst" "UnsignedFromInteger"
    (i : integer) -> unsigned<n>

    @"intrininst" "UnsignedFromUnsigned"
    (i : unsigned<\m>) -> unsigned<n>

@"intrininst" "implicitIntegerToUnsigned"
implicitTo<unsigned<\D>>(i : integer) -> unsigned<D>

@"intrininst" "implicitUnsignedToUnsigned"
implicitTo<unsigned<\D>>(s : unsigned<\S>) -> unsigned<D> =>
    :. unsigned<D> s

u1   := unsigned<1  >
u8   := unsigned<8  >
u16  := unsigned<16 >
u32  := unsigned<32 >
u64  := unsigned<64 >
u128 := unsigned<128>

@"intrininst" "SignedTemplate"
:& signed<\n : integer>
    @"intrininst" "SignedFromInteger"
    (i : integer) -> signed<n>

    @"intrininst" "SignedFromSigned"
    (i : signed<\m>) -> signed<n>

@"intrininst" "implicitIntegerToSigned"
implicitTo<signed<\D>>(i : integer) -> signed<D>

@"intrininst" "implicitSignedToSigned"
implicitTo<signed<\D>>(s : signed<\S>) -> signed<D> =>
    :. signed<D> s

s8   := signed<8  >
s16  := signed<16 >
s32  := signed<32 >
s64  := signed<64 >
s128 := signed<128>

@"intrininst" "ReferenceTemplate"
:| ref<\T>

@"intrininst" "PointerTemplate"
:| ptr<\T>

@"intrininst" "ArrayStaticTemplate"
:& array<\T, \card : integer>

@"intrininst" "ArrayDynamicTemplate"
:& array<\T>
    base : ptr T
    card : uz

    @"intrininst" "Array_idx"
    (this : ref array<T>, i : uz) -> ref T

@"intrininst" "SliceTemplate"
:& slice<\T>
    base : ptr T
    card : uz

    // todo: removeme
    ctor(this : ref slice<T>, p : ptr T, c : uz) -> () =>
        this.base = p
        this.card = c
        :.

    @"intrininst" "Slice_idx"
    (this : ref slice<T>, i : uz) -> ref T

    (this : ref slice<T>, f : (ref T) -> ()) -> () =>
        := i : uz = 0
        :* lt i this.card
            f (this i)
            i = add i 1
        :.

ascii := slice<u8>

@"intrininst" "implicitStringToAscii"
implicitTo<ascii>(s : string) -> ascii

wordSize := 64
uz := unsigned<wordSize>

staticSize(p : ptr \T) -> uz => wordSize

@"intrininst" "Addu"
add(x y : unsigned<\n>) -> unsigned<n>

@"intrininst" "Adds"
add(x y : signed<\n>) -> signed<n>

@"intrininst" "Subu"
sub(x y : unsigned<\n>) -> unsigned<n>

@"intrininst" "Subs"
sub(x y : signed<\n>) -> signed<n>

@"intrininst" "Mulu"
mul(x y : unsigned<\n>) -> unsigned<n>

@"intrininst" "Muls"
mul(x y : signed<\n>) -> signed<n>

@"intrininst" "Divu"
div(x y : unsigned<\n>) -> unsigned<n>

@"intrininst" "Divs"
div(x y : signed<\n>) -> signed<n>

@"intrininst" "Remu"
rem(x y : unsigned<\n>) -> unsigned<n>

@"intrininst" "Rems"
rem(x y : signed<\n>) -> signed<n>

@"intrininst" "Shlu"
shl(x : unsigned<\n>, y : unsigned<n>) -> unsigned<n>

@"intrininst" "Shls"
shl(x : signed<\n>, y : unsigned<n>) -> signed<n>

@"intrininst" "Shru"
shr(x : unsigned<\n>, y : unsigned<n>) -> unsigned<n>

@"intrininst" "Shrs"
shr(x : signed<\n>, y : unsigned<n>) -> signed<n>

@"intrininst" "Bitandu"
bitand(x y : unsigned<\n>) -> unsigned<n>

@"intrininst" "Bitands"
bitand(x y : signed<\n>) -> signed<n>

@"intrininst" "Bitoru"
bitor(x y : unsigned<\n>) -> unsigned<n>

@"intrininst" "Bitors"
bitor(x y : signed<\n>) -> signed<n>

@"intrininst" "Bitxoru"
bitxor(x y : unsigned<\n>) -> unsigned<n>

@"intrininst" "Bitxors"
bitxor(x y : signed<\n>) -> signed<n>

@"intrininst" "Equ"
eq(x y : unsigned<\n>) -> u1

@"intrininst" "Eqs"
eq(x y : signed<\n>) -> u1

@"intrininst" "Neu"
ne(x y : unsigned<\n>) -> u1

@"intrininst" "Nes"
ne(x y : signed<\n>) -> u1

@"intrininst" "Gtu"
gt(x y : unsigned<\n>) -> u1

@"intrininst" "Gts"
gt(x y : signed<\n>) -> u1

@"intrininst" "Geu"
ge(x y : unsigned<\n>) -> u1

@"intrininst" "Ges"
ge(x y : signed<\n>) -> u1

@"intrininst" "Ltu"
lt(x y : unsigned<\n>) -> u1

@"intrininst" "Lts"
lt(x y : signed<\n>) -> u1

@"intrininst" "Leu"
le(x y : unsigned<\n>) -> u1

@"intrininst" "Les"
le(x y : signed<\n>) -> u1

@"intrininst" "Not"
not(x : u1) -> u1

@"intrininst" "Truncu"
trunc<unsigned<\n>>(x : unsigned<\m>) -> unsigned<n>

@"intrininst" "Truncs"
trunc<signed<\n>>(x : signed<\m>) -> signed<n>

@"intrininst" "Addr"
addr(p : ref \T) -> ptr T

@"intrininst" "Cast"
cast<ptr \T>(p : ptr \U) -> ptr T

)axioms";

#include <kyfoo/ast/Axioms.hpp>

#include <algorithm>

#include <kyfoo/ast/Expressions.hpp>
#include <kyfoo/ast/Declarations.hpp>
#include <kyfoo/ast/Module.hpp>
#include <kyfoo/ast/Overloading.hpp>
#include <kyfoo/ast/Scopes.hpp>
#include <kyfoo/ast/Semantics.hpp>

namespace kyfoo::ast {

//
// AxiomsModule

AxiomsModule::AxiomsModule(ModuleSet* moduleSet,
                           std::string name)
    : Module(moduleSet, std::move(name))
{
    myScope = mk<ast::Scope>(*this);

    myScope->append(mk<DataSumDeclaration>(Symbol(lexer::Token(lexer::TokenKind::Identifier, "integer" , lexer::SourceLocation()))));
    myScope->append(mk<DataSumDeclaration>(Symbol(lexer::Token(lexer::TokenKind::Identifier, "rational", lexer::SourceLocation()))));
    myScope->append(mk<DataSumDeclaration>(Symbol(lexer::Token(lexer::TokenKind::Identifier, "string"  , lexer::SourceLocation()))));
    myScope->append(mk<DataSumDeclaration>(Symbol(lexer::Token(lexer::TokenKind::Identifier, "null_t"  , lexer::SourceLocation()))));

    for ( uz i = IntegerLiteralType; i <= PointerNullLiteralType; ++i )
        myDataSumDecls[i] = scope()->childDeclarations()[i]->as<DataSumDeclaration>();
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
    for ( uz i = 0; i < DataSumIntrinsicsCount; ++i )
        if ( myDataSumDecls[i] == &decl )
            return true;

    return false;
}

bool AxiomsModule::isIntrinsic(DataProductDeclaration const& decl) const
{
    for ( uz i = 0; i < DataProductIntrinsicsCount; ++i )
        if ( myDataProductDecls[i] == &decl )
            return true;

    return false;
}

bool AxiomsModule::isIntrinsic(ProcedureDeclaration const& decl) const
{
    for ( uz i = 0; i < InstructionIntrinsicsCount; ++i )
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
    for ( uz i = 0; i < DataSumIntrinsicsCount; ++i )
        if ( myDataSumDecls[i] == &decl )
            return true;

    return false;
}

bool AxiomsModule::isLiteral(DataProductDeclaration const& decl) const
{
    for ( uz i = 0; i < DataProductIntrinsicsCount; ++i )
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

void AxiomsModule::setIntrinsic(std::string_view nameLiteral, Declaration const* decl)
{
    static const char* sums[] = {
#define X(a) #a,
        INTRINSIC_DATASUMS(X)
#undef X
    };
    static const char* prods[] = {
#define X(a) #a,
        INTRINSIC_DATAPRODUCTS(X)
#undef X
    };
    static const char* instrs[] = {
#define X(a) #a,
        INTRINSIC_INSTRUCTIONS(X)
#undef X
    };

    auto name = nameLiteral.substr(1, nameLiteral.length() - 2);
    if ( auto ds = decl->as<DataSumDeclaration>() ) {
        myDataSumDecls[std::find(sums, sums + DataSumIntrinsicsCount, name) - sums] = ds;
        return;
    }

    if ( auto dp = decl->as<DataProductDeclaration>() ) {
        myDataProductDecls[std::find(prods, prods + DataProductIntrinsicsCount, name) - prods] = dp;
        return;
    }

    if ( auto proc = decl->as<ProcedureDeclaration>() ) {
        myInstructionDecls[std::find(instrs, instrs + InstructionIntrinsicsCount, name) - instrs] = proc;
        return;
    }

    throw std::runtime_error("unknown intrinsic");
}

void AxiomsModule::findIntrinsics(Scope const* s)
{
    for ( auto d : s->childDeclarations() ) {
        for ( auto const& attr : d->attributes() ) {
            // parsed as apply-expressions, lowered to tuple-expression during elaboration
            if ( auto t = attr.expression().as<ApplyExpression>() ) {
                auto subject = t->expressions()[0];
                if ( auto p = subject->as<LiteralExpression>() ) {
                    if ( p->token().lexeme() == "\"intrininst\"" ) {
                        auto object = t->expressions()[1]->as<LiteralExpression>();
                        setIntrinsic(object->token().lexeme(), d);
                    }
                }
            }
        }
    }

    for ( auto ss : s->childDefinitions() )
        findIntrinsics(ss);
}

bool AxiomsModule::init(Diagnostics& dgn)
{
    std::stringstream s(source);
    try {
        parse(dgn, s);
        if ( dgn.errorCount() )
            return false;

        findIntrinsics(scope());

        resolveImports(dgn);
        if ( dgn.errorCount() )
            return false;

        semantics(dgn);
        if ( dgn.errorCount() )
            return false;

        myDataProductDecls[u1  ] = resolveIndirections(scope()->findEquivalent("u1"  ).single())->as<DataProductDeclaration>();
        myDataProductDecls[u8  ] = resolveIndirections(scope()->findEquivalent("u8"  ).single())->as<DataProductDeclaration>();
        myDataProductDecls[u16 ] = resolveIndirections(scope()->findEquivalent("u16" ).single())->as<DataProductDeclaration>();
        myDataProductDecls[u32 ] = resolveIndirections(scope()->findEquivalent("u32" ).single())->as<DataProductDeclaration>();
        myDataProductDecls[u64 ] = resolveIndirections(scope()->findEquivalent("u64" ).single())->as<DataProductDeclaration>();
        myDataProductDecls[u128] = resolveIndirections(scope()->findEquivalent("u128").single())->as<DataProductDeclaration>();

        myDataProductDecls[s8  ] = resolveIndirections(scope()->findEquivalent("s8"  ).single())->as<DataProductDeclaration>();
        myDataProductDecls[s16 ] = resolveIndirections(scope()->findEquivalent("s16" ).single())->as<DataProductDeclaration>();
        myDataProductDecls[s32 ] = resolveIndirections(scope()->findEquivalent("s32" ).single())->as<DataProductDeclaration>();
        myDataProductDecls[s64 ] = resolveIndirections(scope()->findEquivalent("s64" ).single())->as<DataProductDeclaration>();
        myDataProductDecls[s128] = resolveIndirections(scope()->findEquivalent("s128").single())->as<DataProductDeclaration>();

        myDataProductDecls[ascii ] = resolveIndirections(scope()->findEquivalent("ascii").single())->as<DataProductDeclaration>();
        myDataProductDecls[size_t] = resolveIndirections(scope()->findEquivalent("uz").single())->as<DataProductDeclaration>();

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

    myIntegerMetaData[6 ] = IntegerMetaData{ intrinsic(s8  ), -8   };
    myIntegerMetaData[7 ] = IntegerMetaData{ intrinsic(s16 ), -16  };
    myIntegerMetaData[8 ] = IntegerMetaData{ intrinsic(s32 ), -32  };
    myIntegerMetaData[9 ] = IntegerMetaData{ intrinsic(s64 ), -64  };
    myIntegerMetaData[10] = IntegerMetaData{ intrinsic(s128), -128 };
}

} // namespace kyfoo::ast
