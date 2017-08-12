#pragma once

#include <array>
#include <memory>

#include <kyfoo/ast/Module.hpp>

namespace kyfoo {
    namespace ast {

class DataSumDeclaration;
class ProcedureDeclaration;
class ModuleSet;

enum DataSumIntrinsics
{
    EmptyLiteralType,
    IntegerLiteralType,
    RationalLiteralType,
    StringLiteralType,
    PointerNullLiteralType,

    UnsignedTemplate,
    SignedTemplate,

    u1,
    u8,
    u16,
    u32,
    u64,
    u128,

    i8,
    i16,
    i32,
    i64,
    i128,

    PointerTemplate,
    ArrayStaticTemplate,
    ArrayDynamicTemplate,

    DataSumInstrinsicsCount
};

enum InstructionIntrinsics
{
    Addu1,
    Addu8,
    Addu16,
    Addu32,
    Addu64,
    Addu128,
    Addi8,
    Addi16,
    Addi32,
    Addi64,
    Addi128,

    Truncu1u8,
    Truncu1u16,
    Truncu1u32,
    Truncu1u64,
    Truncu1u128,
    Truncu8u16,
    Truncu8u32,
    Truncu8u64,
    Truncu8u128,
    Truncu16u32,
    Truncu16u64,
    Truncu16u128,
    Truncu32u64,
    Truncu32u128,
    Truncu64u128,
    Trunci8i16,
    Trunci8i32,
    Trunci8i64,
    Trunci8i128,
    Trunci16i32,
    Trunci16i64,
    Trunci16i128,
    Trunci32i64,
    Trunci32i128,
    Trunci64i128,

    Addr,

    InstructionIntrinsicsCount
};

class AxiomsModule : public Module
{
public:
    struct IntegerMetaData
    {
        DataSumDeclaration const* decl;
        int bits;
    };

protected:
    friend class ModuleSet;
    AxiomsModule(ModuleSet* moduleSet, std::string const& name);
    
    bool init(Diagnostics& dgn);

public:
    ~AxiomsModule();

public:
    /**
     * Empty expression ()
     */
    DataSumDeclaration const* emptyLiteralType() const;

    /**
     * Integer literal types
     */
    DataSumDeclaration const* integerLiteralType() const;

    /**
     * Rational literal types
     */
    DataSumDeclaration const* rationalLiteralType() const;

    /**
     * String literal types
     */
    DataSumDeclaration const* stringLiteralType() const;

    /**
     * Null literal type
     */
    DataSumDeclaration const* pointerNullLiteralType() const;

    /**
     * Intrinsic accessors by ordinal
     */
    /** { */
    DataSumDeclaration const* intrinsic(DataSumIntrinsics i) const;
    ProcedureDeclaration const* intrinsic(InstructionIntrinsics i) const;
    /** } */

    /**
     * Test whether a declaration is an intrinsic
     */
    /** { */
    bool isIntrinsic(DataSumDeclaration const& decl) const;
    bool isIntrinsic(ProcedureDeclaration const& decl) const;
    bool isIntrinsic(Declaration const& decl) const;
    /** } */

    /**
     * Test whether a declaration is a literal
     */
    /** { */
    bool isLiteral(DataSumDeclaration const& decl) const;
    bool isLiteral(Declaration const& decl) const;
    /** } */

    IntegerMetaData const* integerMetaData(Declaration const& decl) const;

private:
    std::unique_ptr<DataSumDeclaration> myEmptyLiteralType;
    std::unique_ptr<DataSumDeclaration> myIntegerLiteralType;
    std::unique_ptr<DataSumDeclaration> myRationalLiteralType;
    std::unique_ptr<DataSumDeclaration> myStringLiteralType;
    std::unique_ptr<DataSumDeclaration> myPointerNullLiteralType;

    DataSumDeclaration const* myDataSumDecls[DataSumInstrinsicsCount];
    ProcedureDeclaration const* myInstructionDecls[InstructionIntrinsicsCount];

private:
    void buildMetaData();

    std::array<IntegerMetaData, i128 - u1 + 1> myIntegerMetaData;
};

    } // namespace ast
} // namespace kyfoo
