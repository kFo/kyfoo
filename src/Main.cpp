#include <iomanip>
#include <iostream>
#include <fstream>
#include <filesystem>
#include <queue>
#include <set>
#include <vector>

#include <kyfoo/Diagnostics.hpp>

#include <kyfoo/lexer/Scanner.hpp>

#include <kyfoo/ast/Axioms.hpp>
#include <kyfoo/ast/Module.hpp>
#include <kyfoo/ast/Node.hpp>
#include <kyfoo/ast/Semantics.hpp>

#include <kyfoo/codegen/Codegen.hpp>
#include <kyfoo/codegen/LLVM.hpp>

namespace kyfoo {

int runScannerDump(std::filesystem::path const& file)
{
    std::ifstream fin(file);
    lexer::Scanner scanner(fin);
    if ( !scanner ) {
        std::cout << "could not open file: " << file << std::endl;
        return EXIT_FAILURE;
    }

    while (scanner)
    {
        auto token = scanner.next();
        std::cout << 'L' << std::left << std::setw(10) << token.line();
        switch (token.kind())
        {
        case lexer::TokenKind::Undefined:
            std::cout << "<undefined>\n";
            break;

        case lexer::TokenKind::EndOfFile:
            std::cout << "<eof>\n";
            break;

        case lexer::TokenKind::IndentLT:
        case lexer::TokenKind::IndentEQ:
        case lexer::TokenKind::IndentGT:
        case lexer::TokenKind::IndentError:
            std::cout << "<" << to_string(token.kind()) << "> " << '\n';
            break;

        case lexer::TokenKind::Identifier:
            std::cout << '\'' << token.lexeme() << "'\n";
            break;

        default:
            std::cout << token.lexeme() << " : " << to_string(token.kind()) << '\n';
        }
    }

    return EXIT_SUCCESS;
}

int runParserTest(std::filesystem::path const& filepath)
{
    Diagnostics dgn;
    ast::ModuleSet moduleSet;
    auto main = moduleSet.create(filepath);
    try {
        main->parse(dgn);
        ast::JsonOutput output(std::cout);
        main->io(output);
    }
    catch (Diagnostics*) {
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

int analyzeModule(ast::Module& m, bool treeDump)
{
    Diagnostics dgn;
    StopWatch sw;
    try {
        m.semantics(dgn);
        if ( treeDump ) {
            std::ofstream fout(std::string(m.name()) + ".astdump.json");
            if ( fout ) {
                ast::JsonOutput out(fout);
                m.io(out);
            }
        }
    }
    catch (Diagnostics*) {
        // Handled below
    }
    catch (std::exception const& e) {
        std::cout << m << ": ICE: " << e.what() << std::endl;
        return EXIT_FAILURE;
    }

    auto semTime = sw.reset();
    dgn.dumpErrors(std::cout);
    std::cout << "semantics: " << m << "; errors: " << dgn.errorCount() << "; time: " << semTime.count() << std::endl;

    if ( dgn.errorCount() )
        return EXIT_FAILURE;

    return EXIT_SUCCESS;
}

int codegenModule(Diagnostics& dgn,
                  codegen::LLVMGenerator& gen,
                  ast::Module const& m,
                  bool writeIR)
{
    StopWatch sw;

    auto IRFilepath = [](std::filesystem::path p) {
        return p.replace_extension(".ll");
    };

    try {
        gen.generate(m);
        if ( writeIR )
            gen.writeIR(m, IRFilepath(m.path()));
        else
            gen.write(m, codegen::toObjectFilepath(m.path()));
    }
    catch (Diagnostics*) {
        // Handled below
    }
    catch (std::exception const& e) {
        std::cout << m << ": ICE: " << e.what() << std::endl;
        return EXIT_FAILURE;
    }

    auto semTime = sw.reset();
    dgn.dumpErrors(std::cout);
    std::cout << "codegen: " << m << "; errors: " << dgn.errorCount() << "; time: " << semTime.count() << std::endl;

    if ( dgn.errorCount() )
        return EXIT_FAILURE;

    return EXIT_SUCCESS;
}

enum Options
{
    None          = 0,
    TreeDump      = 1 << 0,
    SemanticsOnly = 1 << 1,
    IR            = 1 << 2,
};

int compile(std::vector<std::filesystem::path> const& files, u32 options)
{
    auto ret = EXIT_SUCCESS;
    ast::ModuleSet moduleSet;
    {
        Diagnostics dgn;
        try {
            if ( !moduleSet.init(dgn) ) {
                dgn.dumpErrors(std::cout);
                return EXIT_FAILURE;
            }
        }
        catch (std::exception const& e) {
            std::cout << "ICE: " << e.what() << std::endl;
            return EXIT_FAILURE;
        }
    }

    std::set<ast::Module*> visited;
    std::queue<ast::Module*> queue;

    auto append = [&](ast::Module* m) {
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

    moduleSet.initBaseModules();

    // parse and imports extraction
    // Every module will be parsed before the semantics pass so that symbols
    // may be resolved across module boundaries

    // TODO: lazily parse when this consumes too much memory
    std::chrono::duration<double> parseTime;
    while ( !queue.empty() ) {
        auto m = take();

        Diagnostics dgn;
        StopWatch sw;
        try {
            if ( m->parsed() )
                continue;

            m->parse(dgn);
            m->resolveImports(dgn);
            for ( auto const& i : m->imports() ) {
                if ( i != &moduleSet.axioms() )
                    append(i);
            }
        }
        catch (Diagnostics*) {
            // Handled below
        }
        catch (std::exception const& e) {
            std::cout << *m << ": ICE: " << e.what() << std::endl;
            return EXIT_FAILURE;
        }

        parseTime = sw.reset();
        dgn.dumpErrors(std::cout);
        std::cout << "parse: " << *m << "; errors: " << dgn.errorCount() << "; time: " << parseTime.count() << std::endl;

        if ( dgn.errorCount() )
            ret = EXIT_FAILURE;
    }

    if ( ret != EXIT_SUCCESS )
        return ret;

    // semantic pass
    std::vector<ast::Module*> allModules(begin(moduleSet.modules()), end(moduleSet.modules()));
    allModules.erase(find(begin(allModules), end(allModules), &moduleSet.axioms()));

    for ( auto m = begin(allModules); m != end(allModules); ++m ) {
        for ( auto mm = next(m); mm != end(allModules); ++mm ) {
            if ( (*m)->imports(*mm) ) {
                iter_swap(m, mm);
                mm = next(m);
            }
        }
    }

    Diagnostics dgn;
    codegen::LLVMGenerator gen(dgn, moduleSet);

    try {
        gen.generate(moduleSet.axioms());
    }
    catch (std::exception const& e) {
        std::cout << "ICE: " << e.what() << std::endl;
        return EXIT_FAILURE;
    }
    catch (Diagnostics*) {
        // nop
    }

    if ( dgn.errorCount() ) {
        dgn.dumpErrors(std::cout);
        return EXIT_FAILURE;
    }

    for ( auto m : allModules ) {
        if ( (ret = analyzeModule(*m, options & TreeDump)) != EXIT_SUCCESS )
            return ret;

        if ( options & SemanticsOnly )
            continue;

        if ( (ret = codegenModule(dgn, gen, *m, options & IR)) != EXIT_SUCCESS )
            return ret;
    }

    return ret;
}

void printHelp(std::filesystem::path const& arg0)
{
    auto cmd = arg0.filename().string();

    std::cout << cmd <<
        " COMMAND FILE [FILE2 FILE3 ...]\n"
        "\n"
        "COMMAND:\n"
        "  scan, lexer, lex    Prints the lexer output of the module\n"
        "  parse, grammar      Prints the parse tree as JSON\n"
        "  semantics, sem      Checks the module for semantic errors\n"
        "  semdump             Checks semantics and prints tree\n"
        "  c, compile          Compiles the module\n"
        "  ir                  Generates LLVM IR code"
        << std::endl;
}

} // namespace kyfoo

int main(int argc, char* argv[])
{
    using namespace kyfoo;

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
        else if ( command == "semantics" || command == "sem" || command == "semdump" ) {
            std::vector<std::filesystem::path> files;
            for ( int i = 2; i != argc; ++i )
                files.push_back(argv[i]);

            u32 options = SemanticsOnly;
            if ( command == "semdump" )
                options |= TreeDump;

            return compile(files, options);
        }
        else if ( command == "compile" || command == "c" || command == "ir" ) {
            std::vector<std::filesystem::path> files;
            for ( int i = 2; i != argc; ++i )
                files.push_back(argv[i]);

            return compile(files, command == "ir" ? IR : None);
        }

        std::cout << "Unknown option: " << command << std::endl;
        printHelp(argv[0]);
    }
    catch (std::exception const& e) {
        std::cout << "ICE: " << e.what() << std::endl;
    }

    return EXIT_FAILURE;
}
