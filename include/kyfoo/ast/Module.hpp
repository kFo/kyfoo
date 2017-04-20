#pragma once

#include <filesystem>
#include <memory>
#include <string>

#include <kyfoo/ast/Node.hpp>

namespace kyfoo {

    class Diagnostics;

    namespace lexer {
        class Scanner;
    }

    namespace ast {

class DeclarationScope;

class Module : public INode
{
public:
    explicit Module(std::string const& name);
    Module(std::experimental::filesystem::path const& path);
    ~Module();

    // IIO
public:
    void io(IStream& stream) override;

public:
    std::string const& name() const;
    std::experimental::filesystem::path const& path() const;


public:
    void parse(Diagnostics& dgn);
    void semantics(Diagnostics& dgn);

private:
    std::experimental::filesystem::path myPath;
    std::string myName;
    std::unique_ptr<DeclarationScope> myScope;
};

    } // namespace ast
} // namespace kyfoo
