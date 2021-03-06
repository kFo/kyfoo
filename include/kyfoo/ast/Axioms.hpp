#pragma once

#include <array>

#include <kyfoo/Types.hpp>
#include <kyfoo/ast/Module.hpp>

namespace kyfoo::ast {

class DataTypeDeclaration;
class ProcedureDeclaration;
class ModuleSet;

#define INTRINSIC_DATATYPES(X) \
    X(IntegerLiteralType    ) \
    X(RationalLiteralType   ) \
    X(StringLiteralType     ) \
    X(PointerNullLiteralType) \
    \
    X(ReferenceTemplate) \
    X(PointerTemplate  ) \
    \
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

namespace intrin::type {
    enum Enum
    {
#define X(a) a,
        INTRINSIC_DATATYPES(X)
#undef X
        ECount
    };
}

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
    X(UnsignedSucc) \
    X(UnsignedPred) \
    X(UnsignedInc ) \
    X(UnsignedDec ) \
    X(SignedSucc  ) \
    X(SignedPred  ) \
    X(SignedInc   ) \
    X(SignedDec   ) \
    \
    X(Array_idx            ) \
    X(Slice_idx            ) \
    X(Slice_apply          ) \
    X(implicitStringToAscii) \
    \
    X(Copyu  ) \
    X(Copys  ) \
    X(Bind   ) \
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

namespace intrin::instr {
    enum Enum
    {
#define X(a) a,
        INTRINSIC_INSTRUCTIONS(X)
#undef X
        ECount
    };
}

class AxiomsModule : public Module
{
public:
    struct IntegerMetaData
    {
        DataTypeDeclaration const* decl;
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
    DataTypeDeclaration const* intrinsic(intrin::type::Enum i) const;
    ProcedureDeclaration const* intrinsic(intrin::instr::Enum i) const;
    /** } */

    /**
     * Test whether a declaration is an intrinsic
     */
    /** { */
    bool isIntrinsic(DataTypeDeclaration const& decl) const;
    bool isIntrinsic(ProcedureDeclaration const& decl) const;
    bool isIntrinsic(Declaration const& decl) const;
    /** } */

    /**
     * Test whether a declaration is a literal
     */
    /** { */
    bool isLiteral(DataTypeDeclaration const& decl) const;
    bool isLiteral(Declaration const& decl) const;
    /** } */

    IntegerMetaData const* integerMetaData(Declaration const& decl) const;

private:
    void setIntrinsic(stringv name, Declaration const* decl);
    void findIntrinsics(Scope const* s);

    DataTypeDeclaration const* myDataTypeDecls[intrin::type::ECount];
    ProcedureDeclaration const* myInstructionDecls[intrin::instr::ECount];

private:
    void buildMetaData();

    std::array<IntegerMetaData, intrin::type::s128 - intrin::type::u1 + 1> myIntegerMetaData;
};

} // namespace kyfoo::ast
