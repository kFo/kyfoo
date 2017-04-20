#include <iostream>
#include <fstream>
#include <filesystem>
#include <vector>

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

int runSemanticsTest(std::vector<fs::path> const& files)
{
    auto ret = EXIT_SUCCESS;
    std::vector<std::unique_ptr<kyfoo::ast::Module>> modules;

    for ( auto& f : files ) {
        kyfoo::Diagnostics dgn;
        auto modFile = canonical(f).make_preferred().string();
        std::chrono::duration<double> parseTime;
        std::chrono::duration<double> semTime;
        try {
            for ( auto const& m : modules )
                if ( m->path() == f )
                    goto L_nextFile;

            kyfoo::StopWatch sw;

            modules.emplace_back(std::make_unique<kyfoo::ast::Module>(f));
            auto& m = modules.back();
            m->parse(dgn);

            parseTime = sw.reset();

            if ( dgn.errorCount() == 0 )
                m->semantics(dgn);

            semTime = sw.elapsed();
        }
        catch (kyfoo::Diagnostics*) {
            // Handled below
        }
        catch (std::exception const& e) {
            std::cout << modFile << ": ICE: " << e.what() << std::endl;
            return EXIT_FAILURE;
        }

        dgn.dumpErrors(std::cout);
        std::cout << modFile << ": done; errors: " << dgn.errorCount() << "; parse: " << parseTime.count() << "; sem: " << semTime.count() << std::endl;

        if ( dgn.errorCount() )
            ret = EXIT_FAILURE;

    L_nextFile:
        continue;
    }

    return ret;
}

void printHelp(fs::path const& arg0)
{
    auto cmd = arg0.filename().string();

    std::cout << cmd <<
        " COMMAND FILE [FILE2 FILE3 ...]\n"
        "\n"
        "COMMAND:\n"
        "  scan, lex, lexer    Prints the lexer output of the module\n"
        " parse, grammar       Prints the parse tree as JSON\n"
        " semantics            Checks the module for semantic errors"
        << std::endl;
}

int main(int argc, char* argv[])
{
    if ( argc < 3 ) {
        printHelp(argv[0]);
        return EXIT_FAILURE;
    }

    std::string command = argv[1];
    std::string file = argv[2];

    if ( command == "scan" || command == "lex" || command == "lexer" ) {
        if ( argc != 3 ) {
            printHelp(argv[0]);
            return EXIT_FAILURE;
        }

        return runScannerDump(file);
    }
    else if ( command == "parse" || command == "grammar" ) {
        if ( argc != 3 ) {
            printHelp(argv[0]);
            return EXIT_FAILURE;
        }

        return runParserTest(file);
    }
    else if ( command == "semantics" ) {
        std::vector<fs::path> files;
        for ( int i = 2; i != argc; ++i )
            files.push_back(argv[i]);

        return runSemanticsTest(files);
    }

    std::cout << "Unknown option: " << command << std::endl;
    return EXIT_FAILURE;
}
