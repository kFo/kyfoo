#include <iostream>
#include <fstream>
#include <filesystem>

#include <kyfoo/Diagnostics.hpp>

#include <kyfoo/lexer/Scanner.hpp>

#include <kyfoo/parser/Parse.hpp>

#include <kyfoo/ast/Module.hpp>
#include <kyfoo/ast/Node.hpp>
#include <kyfoo/ast/Semantics.hpp>

namespace fs = std::experimental::filesystem;

int runScannerDump(fs::path const& file)
{
    std::ifstream fin(file);
    kyfoo::lexer::Scanner scanner(fin);
    if ( !scanner ) {
        std::cout << "could not open file: " << file << std::endl;
        return EXIT_FAILURE;
    }

    while (scanner)
    {
        auto token = scanner.next();
        std::cout << 'L' << token.line() << '\t';
        switch (token.kind())
        {
        case kyfoo::lexer::TokenKind::EndOfFile:
            std::cout << "<eof>\n";
            break;

        case kyfoo::lexer::TokenKind::Indent:
            std::cout << "<indent> " << token.lexeme().size() << '\n';
            break;

        case kyfoo::lexer::TokenKind::LineBreak:
            std::cout << "<br>\n";
            break;

        case kyfoo::lexer::TokenKind::Identifier:
            std::cout << '\'' << token.lexeme() << "'\n";
            break;

        default:
            std::cout << token.lexeme() << " : " << kyfoo::lexer::toString(token.kind()) << '\n';
        }
    }

    return EXIT_SUCCESS;
}

int runParserTest(fs::path const& filepath)
{
    kyfoo::Diagnostics dgn;
    auto main = std::make_unique<kyfoo::ast::Module>(filepath);
    try {
        main->parse(dgn);
        kyfoo::ast::JsonOutput output(std::cout);
        main->io(output);
    }
    catch (kyfoo::Diagnostics*) {
        // Handled below
    }
    catch (std::exception const& e) {
        std::cout << filepath << ": ICE: " << e.what() << std::endl;
        return EXIT_FAILURE;
    }

    dgn.dumpErrors(std::cout);

    if ( dgn.errorCount() )
        return EXIT_FAILURE;

    return EXIT_SUCCESS;
}

int runSemanticsTest(fs::path const& filepath)
{
    kyfoo::Diagnostics dgn;
    auto main = std::make_unique<kyfoo::ast::Module>(filepath);
    try {
        main->parse(dgn);
        if ( dgn.errorCount() == 0 )
            main->semantics(dgn);
    }
    catch (kyfoo::Diagnostics*) {
        // Handled below
    }
    catch (std::exception const& e) {
        std::cout << filepath << ": ICE: " << e.what() << std::endl;
        return EXIT_FAILURE;
    }

    dgn.dumpErrors(std::cout);
    std::cout << main->path().generic_string() << ": completed with " << dgn.errorCount() << " errors" << std::endl;

    if ( dgn.errorCount() )
        return EXIT_FAILURE;

    return EXIT_SUCCESS;
}

int main(int argc, char* argv[])
{
    if ( argc != 3 ) {
        std::cout << "only expected 2 arguments" << std::endl;
        return EXIT_FAILURE;
    }

    std::string command = argv[1];
    std::string file = argv[2];

    if ( command == "scan" || command == "lex" || command == "lexer" )
        return runScannerDump(file);
    else if ( command == "parse" || command == "grammar" )
        return runParserTest(file);
    else if ( command == "semantics" )
        return runSemanticsTest(file);

    std::cout << "Unknown option: " << command << std::endl;
    return EXIT_FAILURE;
}
