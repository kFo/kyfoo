#include <iostream>
#include <fstream>

#include <kyfoo/Diagnostics.hpp>

#include <kyfoo/lexer/Scanner.hpp>

#include <kyfoo/parser/Parse.hpp>

#include <kyfoo/ast/Module.hpp>
#include <kyfoo/ast/Node.hpp>
#include <kyfoo/ast/Semantics.hpp>

int runScannerDump(kyfoo::lexer::Scanner& scanner)
{
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

int runParserTest(const char* file, kyfoo::lexer::Scanner& scanner)
{
    kyfoo::Diagnostics dgn;
    try {
        auto main = kyfoo::parser::parseModule(dgn, file, scanner);
        kyfoo::ast::JsonOutput output(std::cout);
        main->io(output);
    }
    catch (kyfoo::Diagnostics*) {
        // Handled below
    }
    catch (std::exception const& e) {
        std::cout << file << ": ICE: " << e.what() << std::endl;
        return EXIT_FAILURE;
    }

    dgn.dumpErrors(std::cout);

    if ( dgn.errorCount() )
        return EXIT_FAILURE;

    return EXIT_SUCCESS;
}

int runSemanticsTest(const char* file, kyfoo::lexer::Scanner& scanner)
{
    kyfoo::Diagnostics dgn;
    try {
        auto main = kyfoo::parser::parseModule(dgn, file, scanner);
        main->semantics(dgn);
    }
    catch (kyfoo::Diagnostics*) {
        // Handled below
    }
    catch (std::exception const& e) {
        std::cout << file << ": ICE: " << e.what() << std::endl;
        return EXIT_FAILURE;
    }

    dgn.dumpErrors(std::cout);
    std::cout << file << ": completed with " << dgn.errorCount() << " errors" << std::endl;

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
    
    std::ifstream fin(file);
    kyfoo::lexer::Scanner scanner(fin);
    if ( !scanner ) {
        std::cout << "could not open file: " << file << std::endl;
        return EXIT_FAILURE;
    }

    if ( command == "scan" || command == "lex" || command == "lexer" )
        return runScannerDump(scanner);
    else if ( command == "parse" || command == "grammar" )
        return runParserTest(file.c_str(), scanner);
    else if ( command == "semantics" )
        return runSemanticsTest(file.c_str(), scanner);

    std::cout << "Unknown option: " << command << std::endl;
    return EXIT_FAILURE;
}
