auto source = R"axioms(
:| integer

wordSize = 32
:| integer<\n : integer>

:| integer<wordSize>

:| pointer<\T>

staticSize<\T>(p : pointer T) => wordSize
ssize = staticSize

:& ascii ; todo: replace with instance of array<integer<7>>

:& array<\T>
    ptr : pointer T
    count : integer wordSize
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
    , myEmptyType(std::make_unique<DataSumDeclaration>(Symbol(lexer::Token())))
{
}

AxiomsModule::~AxiomsModule() = default;

DataSumDeclaration const* AxiomsModule::emptyType() const
{
    return myEmptyType.get();
}

//
// ModuleSet (partial)

std::unique_ptr<AxiomsModule> ModuleSet::createAxiomsModule()
{
    std::stringstream s(source);
    auto ret = std::make_unique<AxiomsModule>(this, "axioms");
    Diagnostics dgn;
    try {
        ret->parse(dgn, s);
        if ( !dgn.errorCount() )
            return ret;
    }
    catch (Diagnostics*) {
        // fall through
    }
    catch (std::exception const&) {
        // fall through
    }

    return nullptr;
}

    } // namespace ast
} // namespace kyfoo
