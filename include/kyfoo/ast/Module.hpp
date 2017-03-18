#pragma once

#include <memory>
#include <string>

#include <kyfoo/ast/Node.hpp>

namespace kyfoo {

    namespace lexer {
        class Scanner;
    }

    namespace ast {

class DeclarationScope;

class Module : public INode
{
public:
    Module(const char* name, std::unique_ptr<DeclarationScope> scope);
    ~Module();

    void io(IStream& stream) override;

private:
    std::string myName;
    std::unique_ptr<DeclarationScope> myScope;
};

    } // namespace ast
} // namespace kyfoo
