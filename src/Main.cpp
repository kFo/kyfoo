#include <iostream>
#include <fstream>
#include <filesystem>
#include <queue>
#include <set>
#include <vector>

#include <kyfoo/Diagnostics.hpp>

#include <kyfoo/lexer/Scanner.hpp>

#include <kyfoo/parser/Parse.hpp>

#include <kyfoo/ast/Module.hpp>
#include <kyfoo/ast/Node.hpp>
#include <kyfoo/ast/Semantics.hpp>

#include "ast/Axioms.hpp"

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
    kyfoo::ast::ModuleSet moduleSet;
    auto main = moduleSet.create(filepath);
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
    kyfoo::ast::ModuleSet moduleSet;
    try {
        createAxiomsModule(&moduleSet);
    }
    catch (std::exception const& e) {
        std::cout << "ICE: " << e.what() << std::endl;
        return EXIT_FAILURE;
    }

    std::set<kyfoo::ast::Module*> visited;
    std::queue<kyfoo::ast::Module*> queue;

    auto append = [&](kyfoo::ast::Module* m) {
        if ( visited.find(m) == end(visited) )
            queue.push(m);
    };

    auto take = [&] {
        auto ret = queue.front();
        queue.pop();
        visited.insert(ret);
        return ret;
    };

    // seed with modules created directly from input files
    for ( auto const& f : files )
        append(moduleSet.create(f));

    // parse and imports extraction
    // Every module will be parsed before the semantics pass so that symbols
    // may be resolved across module boundaries

    // TODO: lazily parse when this consumes too much memory
    std::chrono::duration<double> parseTime;
    while ( !queue.empty() ) {
        auto m = take();

        kyfoo::Diagnostics dgn;
        kyfoo::StopWatch sw;
        try {
            if ( m->parsed() )
                continue;

            m->parse(dgn);
            m->resolveImports(dgn);
            for ( auto const& i : m->imports() )
                append(i);
        }
        catch (kyfoo::Diagnostics*) {
            // Handled below
        }
        catch (std::exception const& e) {
            std::cout << m->path() << ": ICE: " << e.what() << std::endl;
            return EXIT_FAILURE;
        }

        parseTime = sw.reset();
        dgn.dumpErrors(std::cout);
        std::cout << "parse: " << m->path() << "; errors: " << dgn.errorCount() << "; time: " << parseTime.count() << std::endl;

        if ( dgn.errorCount() )
            ret = EXIT_FAILURE;
    }

    if ( ret != EXIT_SUCCESS )
        return ret;

    // semantic pass
    for ( auto const& v : visited )
        queue.push(v);
    visited.clear();

    std::chrono::duration<double> semTime;
    while ( !queue.empty() ) {
        auto m = take();

        kyfoo::Diagnostics dgn;
        kyfoo::StopWatch sw;
        try {
            m->semantics(dgn);
        }
        catch (kyfoo::Diagnostics*) {
            // Handled below
        }
        catch (std::exception const& e) {
            std::cout << m->path() << ": ICE: " << e.what() << std::endl;
            return EXIT_FAILURE;
        }

        semTime = sw.reset();
        dgn.dumpErrors(std::cout);
        std::cout << "semantics: " << m->name() << "; errors: " << dgn.errorCount() << "; parse: " << semTime.count() << std::endl;

        if ( dgn.errorCount() )
            ret = EXIT_FAILURE;
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
        "  scan, lexer, lex    Prints the lexer output of the module\n"
        "  parse, grammar      Prints the parse tree as JSON\n"
        "  semantics, sem      Checks the module for semantic errors"
        << std::endl;
}

int main(int argc, char* argv[])
{
    try {
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
        else if ( command == "semantics" || command == "sem" ) {
            std::vector<fs::path> files;
            for ( int i = 2; i != argc; ++i )
                files.push_back(argv[i]);

            return runSemanticsTest(files);
        }

        std::cout << "Unknown option: " << command << std::endl;
        printHelp(argv[0]);
    }
    catch (std::exception const& e) {
        std::cout << "ICE: " << e.what() << std::endl;
    }

    return EXIT_FAILURE;
}
