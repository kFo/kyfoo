#pragma once

#include <array>

#include <kyfoo/Types.hpp>
#include <kyfoo/ast/Module.hpp>

namespace kyfoo::ast {

class DataSumDeclaration;
class DataProductDeclaration;
class ProcedureDeclaration;
class ModuleSet;

#define INTRINSIC_DATASUMS(X) \
    X(IntegerLiteralType    ) \
    X(RationalLiteralType   ) \
    X(StringLiteralType     ) \
    X(PointerNullLiteralType) \
    \
    X(ReferenceTemplate) \
    X(PointerTemplate  )

enum DataSumIntrinsics
{
#define X(a) a,
    INTRINSIC_DATASUMS(X)
#undef X
    DataSumIntrinsicsCount
};

#define INTRINSIC_DATAPRODUCTS(X) \
    X(ArrayStaticTemplate ) \
    X(ArrayDynamicTemplate) \
    X(SliceTemplate       ) \
    \
    X(UnsignedTemplate) \
    X(SignedTemplate  ) \
    \
    X(u1  ) \
    X(u8  ) \
    X(u16 ) \
    X(u32 ) \
    X(u64 ) \
    X(u128) \
    \
    X(s8  ) \
    X(s16 ) \
    X(s32 ) \
    X(s64 ) \
    X(s128) \
    \
    X(ascii) \
    X(size_t)

enum DataProductIntrinsics
{
#define X(a) a,
    INTRINSIC_DATAPRODUCTS(X)
#undef X
    DataProductIntrinsicsCount
};

#define INTRINSIC_INSTRUCTIONS(X) \
    X(UnsignedFromInteger       ) \
    X(UnsignedFromUnsigned      ) \
    X(implicitIntegerToUnsigned ) \
    X(implicitUnsignedToUnsigned) \
    X(SignedFromInteger         ) \
    X(SignedFromSigned          ) \
    X(implicitIntegerToSigned   ) \
    X(implicitSignedToSigned    ) \
    \
    X(UnsignedInc               ) \
    X(UnsignedDec               ) \
    X(SignedInc                 ) \
    X(SignedDec                 ) \
    \
    X(Array_idx            ) \
    X(Slice_idx            ) \
    X(implicitStringToAscii) \
    \
    X(Addu   ) \
    X(Adds   ) \
    X(Subu   ) \
    X(Subs   ) \
    X(Mulu   ) \
    X(Muls   ) \
    X(Divu   ) \
    X(Divs   ) \
    X(Remu   ) \
    X(Rems   ) \
    X(Shlu   ) \
    X(Shls   ) \
    X(Shru   ) \
    X(Shrs   ) \
    X(Bitandu) \
    X(Bitands) \
    X(Bitoru ) \
    X(Bitors ) \
    X(Bitxoru) \
    X(Bitxors) \
    \
    X(Equ ) \
    X(Eqs ) \
    X(Neu ) \
    X(Nes ) \
    X(Gtu ) \
    X(Gts ) \
    X(Geu ) \
    X(Ges ) \
    X(Ltu ) \
    X(Lts ) \
    X(Leu ) \
    X(Les ) \
    \
    X(Not   ) \
    X(Truncu) \
    X(Truncs) \
    X(Addr  ) \
    X(Cast  )

enum InstructionIntrinsics
{
#define X(a) a,
    INTRINSIC_INSTRUCTIONS(X)
#undef X
    InstructionIntrinsicsCount
};

class AxiomsModule : public Module
{
public:
    struct IntegerMetaData
    {
        DataProductDeclaration const* decl;
        int bits;
    };

protected:
    friend class ModuleSet;
    AxiomsModule(ModuleSet* moduleSet, std::string name);
    
    bool init(Diagnostics& dgn);

public:
    ~AxiomsModule() KYFOO_DEBUG_OVERRIDE;

public:
    /**
     * Intrinsic accessors by ordinal
     */
    /** { */
    DataSumDeclaration const* intrinsic(DataSumIntrinsics i) const;
    DataProductDeclaration const* intrinsic(DataProductIntrinsics i) const;
    ProcedureDeclaration const* intrinsic(InstructionIntrinsics i) const;
    /** } */

    /**
     * Test whether a declaration is an intrinsic
     */
    /** { */
    bool isIntrinsic(DataSumDeclaration const& decl) const;
    bool isIntrinsic(DataProductDeclaration const& decl) const;
    bool isIntrinsic(ProcedureDeclaration const& decl) const;
    bool isIntrinsic(Declaration const& decl) const;
    /** } */

    /**
     * Test whether a declaration is a literal
     */
    /** { */
    bool isLiteral(DataSumDeclaration const& decl) const;
    bool isLiteral(DataProductDeclaration const& decl) const;
    bool isLiteral(Declaration const& decl) const;
    /** } */

    IntegerMetaData const* integerMetaData(Declaration const& decl) const;

private:
    void setIntrinsic(stringv name, Declaration const* decl);
    void findIntrinsics(Scope const* s);

    DataSumDeclaration const* myDataSumDecls[DataSumIntrinsicsCount];
    DataProductDeclaration const* myDataProductDecls[DataProductIntrinsicsCount];
    ProcedureDeclaration const* myInstructionDecls[InstructionIntrinsicsCount];

private:
    void buildMetaData();

    std::array<IntegerMetaData, s128 - u1 + 1> myIntegerMetaData;
};

} // namespace kyfoo::ast
