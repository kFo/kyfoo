#pragma once

#include <memory>

#include <kyfoo/ast/Module.hpp>

namespace kyfoo {
    namespace ast {

class DataSumDeclaration;
class ProcedureDeclaration;
class ModuleSet;

enum DataSumIntrinsics
{
    Empty,
    Integer,
    Rational,
    String,

    PointerTemplate,
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

    InstructionIntrinsicsCount
};

class AxiomsModule : public Module
{
protected:
    friend class ModuleSet;
    AxiomsModule(ModuleSet* moduleSet, std::string const& name);
    
    bool init();

public:
    ~AxiomsModule();

public:
    /**
     * Empty expression ()
     */
    DataSumDeclaration const* emptyType() const;

    /**
     * Integer literal types
     */
    DataSumDeclaration const* integerType() const;

    /**
     * Rational literal types
     */
    DataSumDeclaration const* rationalType() const;

    /**
     * String literal types
     */
    DataSumDeclaration const* stringType() const;

    DataSumDeclaration const* intrinsic(DataSumIntrinsics i) const;
    ProcedureDeclaration const* intrinsic(InstructionIntrinsics i) const;

private:
    std::unique_ptr<DataSumDeclaration> myEmptyType;
    std::unique_ptr<DataSumDeclaration> myIntegerType;
    std::unique_ptr<DataSumDeclaration> myRationalType;
    std::unique_ptr<DataSumDeclaration> myStringType;

    DataSumDeclaration const* myDataSumDecls[DataSumInstrinsicsCount];
    ProcedureDeclaration const* myInstructionDecls[InstructionIntrinsicsCount];
};

    } // namespace ast
} // namespace kyfoo
