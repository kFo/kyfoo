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

int runScannerDump(std::filesystem::path const& file)
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
        std::cout << 'L' << std::left << std::setw(10) << token.line();
        switch (token.kind())
        {
        case kyfoo::lexer::TokenKind::Undefined:
            std::cout << "<undefined>\n";
            break;

        case kyfoo::lexer::TokenKind::EndOfFile:
            std::cout << "<eof>\n";
            break;

        case kyfoo::lexer::TokenKind::IndentLT:
        case kyfoo::lexer::TokenKind::IndentEQ:
        case kyfoo::lexer::TokenKind::IndentGT:
        case kyfoo::lexer::TokenKind::IndentError:
            std::cout << "<" << to_string(token.kind()) << "> " << '\n';
            break;

        case kyfoo::lexer::TokenKind::Identifier:
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

int analyzeModule(kyfoo::ast::Module& m, bool treeDump)
{
    kyfoo::Diagnostics dgn;
    kyfoo::StopWatch sw;
    try {
        m.semantics(dgn);
        if ( treeDump ) {
            std::ofstream fout(m.name() + ".astdump.json");
            if ( fout ) {
                kyfoo::ast::JsonOutput out(fout);
                m.io(out);
            }
        }
    }
    catch (kyfoo::Diagnostics*) {
        // Handled below
    }
    catch (std::exception const& e) {
        std::cout << m.path() << ": ICE: " << e.what() << std::endl;
        return EXIT_FAILURE;
    }

    auto semTime = sw.reset();
    dgn.dumpErrors(std::cout);
    std::cout << "semantics: " << m.name() << "; errors: " << dgn.errorCount() << "; time: " << semTime.count() << std::endl;

    if ( dgn.errorCount() )
        return EXIT_FAILURE;

    return EXIT_SUCCESS;
}

int codegenModule(kyfoo::Diagnostics& dgn,
                  kyfoo::codegen::LLVMGenerator& gen,
                  kyfoo::ast::Module const& m,
                  bool writeIR)
{
    kyfoo::StopWatch sw;

    auto IRFilepath = [](std::filesystem::path p) {
        return p.replace_extension(".ll");
    };

    try {
        gen.generate(m);
        if ( writeIR )
            gen.writeIR(m, IRFilepath(m.path()));
        else
            gen.write(m, kyfoo::codegen::toObjectFilepath(m.path()));
    }
    catch (kyfoo::Diagnostics*) {
        // Handled below
    }
    catch (std::exception const& e) {
        std::cout << m.path() << ": ICE: " << e.what() << std::endl;
        return EXIT_FAILURE;
    }

    auto semTime = sw.reset();
    dgn.dumpErrors(std::cout);
    std::cout << "codegen: " << m.name() << "; errors: " << dgn.errorCount() << "; time: " << semTime.count() << std::endl;

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

int compile(std::vector<std::filesystem::path> const& files, std::uint32_t options)
{
    auto ret = EXIT_SUCCESS;
    kyfoo::ast::ModuleSet moduleSet;
    {
        kyfoo::Diagnostics dgn;
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
            for ( auto const& i : m->imports() ) {
                if ( i != &moduleSet.axioms() )
                    append(i);
            }
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
    std::vector<kyfoo::ast::Module*> allModules(begin(moduleSet.modules()), end(moduleSet.modules()));
    allModules.erase(find(begin(allModules), end(allModules), &moduleSet.axioms()));

    for ( auto m = begin(allModules); m != end(allModules); ++m ) {
        for ( auto mm = next(m); mm != end(allModules); ++mm ) {
            if ( (*m)->imports(*mm) ) {
                iter_swap(m, mm);
                mm = next(m);
            }
        }
    }

    kyfoo::Diagnostics dgn;
    kyfoo::codegen::LLVMGenerator gen(dgn, moduleSet);

    try {
        gen.generate(moduleSet.axioms());
    }
    catch (std::exception const& e) {
        std::cout << "ICE: " << e.what() << std::endl;
        return EXIT_FAILURE;
    }
    catch (kyfoo::Diagnostics*) {
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

    // link
    

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
        else if ( command == "semantics" || command == "sem" || command == "semdump" ) {
            std::vector<std::filesystem::path> files;
            for ( int i = 2; i != argc; ++i )
                files.push_back(argv[i]);

            std::uint32_t options = SemanticsOnly;
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
