#pragma once

#include <memory>
#include <string>

namespace kyfoo {

    namespace lexer {
        class Scanner;
    }

    namespace ast {

class DeclarationScope;

class Module
{
public:
    Module(const char* name, std::unique_ptr<DeclarationScope> scope);
    ~Module();

private:
    std::string myName;
    std::unique_ptr<DeclarationScope> myScope;
};

    } // namespace ast
} // namespace kyfoo
