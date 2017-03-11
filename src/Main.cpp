#include <iostream>
#include <fstream>

#include <kyfoo/Error.hpp>
#include <kyfoo/lexer/Scanner.hpp>
#include <kyfoo/parser/Parse.hpp>
#include <kyfoo/ast/Module.hpp>

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
    try {
        auto main = kyfoo::parser::parseModule(file, scanner);
    }
    catch (kyfoo::Error const& e) {
        std::cout << file << e;
        return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
}

int main(int argc, char* argv[])
{
    if ( argc != 3 ) {
        std::cout << "only expected 2 argument" << std::endl;
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

    if ( command == "scan" )
        return runScannerDump(scanner);
    else if ( command == "parse" )
        return runParserTest(file.c_str(), scanner);
}
