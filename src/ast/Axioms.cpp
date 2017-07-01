auto source = R"axioms(
; todo: attributes for intrinsics
:| integer
:| integer<\n : integer>
:| pointer<\T>

wordSize = 64
size_t = integer<wordSize>

staticSize<\T>(p : pointer T) : size_t => wordSize

:& array<\T>
    ptr : pointer T
    count : size_t
)axioms";

#include <kyfoo/ast/Axioms.hpp>

#include <kyfoo/ast/Expressions.hpp>
#include <kyfoo/ast/Declarations.hpp>
#include <kyfoo/ast/Module.hpp>
#include <kyfoo/ast/Scopes.hpp>

namespace kyfoo {
    namespace ast {

//
// AxiomsModule

AxiomsModule::AxiomsModule(ModuleSet* moduleSet, std::string const& name)
    : Module(moduleSet, name)
    , myEmptyType(std::make_unique<DataSumDeclaration>(Symbol("")))
{
}

AxiomsModule::~AxiomsModule() = default;

DataSumDeclaration const* AxiomsModule::emptyType() const
{
    return myEmptyType.get();
}

DataSumDeclaration const* AxiomsModule::integerType() const
{
    return myIntegerType;
}

DataSumDeclaration const* AxiomsModule::integerTemplate() const
{
    return myIntegerTemplate;
}

DataSumDeclaration const* AxiomsModule::pointerTemplate() const
{
    return myPointerTemplate;
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

        for ( auto const& decl : scope()->childDeclarations() ) {
            auto const& sym = decl->symbol();
            if ( sym.name() == "integer" ) {
                if ( sym.parameters().empty() )
                    myIntegerType = decl->as<DataSumDeclaration>();
                else if ( sym.parameters().size() == 1 )
                    myIntegerTemplate = decl->as<DataSumDeclaration>();
            }
            else if ( sym.name() == "pointer" && sym.parameters().size() == 1 ) {
                myPointerTemplate = decl->as<DataSumDeclaration>();
            }
        }

        semantics(dgn);
        if ( dgn.errorCount() )
            return false;

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
