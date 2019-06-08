#include <filesystem>
#include <queue>
#include <set>
#include <vector>

#include <kyfoo/Diagnostics.hpp>
#include <kyfoo/Stream.hpp>

#include <kyfoo/lexer/Scanner.hpp>

#include <kyfoo/ast/Axioms.hpp>
#include <kyfoo/ast/Clone.hpp>
#include <kyfoo/ast/DotWriter.hpp>
#include <kyfoo/ast/Module.hpp>
#include <kyfoo/ast/Semantics.hpp>

#include <kyfoo/codegen/Codegen.hpp>
#include <kyfoo/codegen/llvm/Generator.hpp>

namespace kyfoo {

int runScannerDump(DefaultOutStream& out, std::filesystem::path const& file)
{
    GUARD( auto fin, MMFile::open(file) ) {
        out("Error: ")(file)(": ")(ec)();
        return EXIT_FAILURE;
    }

    lexer::DefaultTokenFactory tokenFactory;
    lexer::Scanner scanner(tokenFactory, fin.view());
    if ( !scanner ) {
        out("could not open file: ")(file)();
        return EXIT_FAILURE;
    }

    while (scanner)
    {
        auto const& token = scanner.next();
        //std::cout << std::right << std::setfill('0')
        //          << 'L' << std::setw(4) << token.line()
        //          << 'C' << std::setw(3) << token.column()
        //          << std::left << std::setfill(' ')
        //          << '[' << std::setw(20) << to_string(token.kind()) << "]["
        //          << std::setw(0) << token.lexeme() << "]\n";
        out('L')(token.line())('C')(token.column())
           ('[')(to_string(token.kind()))("][")(token.lexeme())("]\n");
    }

    return EXIT_SUCCESS;
}

int runParserDump(DefaultOutStream& out, std::filesystem::path const& filepath)
{
    Diagnostics dgn;
    lexer::DefaultTokenFactory tokenFactory;
    ast::ModuleSet moduleSet(tokenFactory);
    auto main = moduleSet.create(filepath);
    try {
        main->parse(dgn);
        auto outFilepath = filepath;
        outFilepath.replace_extension(".dot");
        writeDot(*main, outFilepath);
    }
    catch (RuntimeException const& e) {
        out(filepath)(": ICE:")(e.file())(":")(e.line())(": ")(e.what())();
        return EXIT_FAILURE;
    }
    catch (std::exception const& e) {
        out(filepath)(": ICE: ")(e.what())();
        return EXIT_FAILURE;
    }
    catch (Diagnostics*) {
        // Handled below
    }

    dgn.dumpErrors(out);

    if ( dgn.errorCount() )
        return EXIT_FAILURE;

    return EXIT_SUCCESS;
}

int analyzeModule(DefaultOutStream& out, ast::Module& m, bool treeDump)
{
    Diagnostics dgn;
    StopWatch sw;
    try {
        m.semantics(dgn);
        if ( treeDump ) {
            auto path = m.path();
            path.replace_extension(".astdump.dot");
            writeDot(m, path);
        }
    }
    catch (RuntimeException const& e) {
        out(m)(": ICE:")(e.file())(":")(e.line())(": ")(e.what())();
        return EXIT_FAILURE;
    }
    catch (std::exception const& e) {
        out(m)(": ICE: ")(e.what())();
        return EXIT_FAILURE;
    }
    catch (Diagnostics*) {
        // Handled below
    }

    auto semTime = sw.reset();
    dgn.dumpErrors(out);
    out("semantics: ")(m)("; errors: ")(dgn.errorCount())("; time: ")(semTime.count())();

    if ( dgn.errorCount() )
        return EXIT_FAILURE;

    return EXIT_SUCCESS;
}

int codegenModule(DefaultOutStream& out,
                  Diagnostics& dgn,
                  codegen::llvm::Generator& gen,
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
    catch (RuntimeException const& e) {
        out(m)(": ICE:")(e.file())(":")(e.line())(": ")(e.what())();
        return EXIT_FAILURE;
    }
    catch (std::exception const& e) {
        out(m)(": ICE: ")(e.what())();
        return EXIT_FAILURE;
    }
    catch (Diagnostics*) {
        // Handled below
    }

    auto semTime = sw.reset();
    dgn.dumpErrors(out);
    out("codegen: ")(m)("; errors: ")(dgn.errorCount())("; time: ")(semTime.count())();

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

int compile(DefaultOutStream& out, std::vector<std::filesystem::path> const& files, u32 options)
{
    auto ret = EXIT_SUCCESS;
    lexer::DefaultTokenFactory tokenFactory;
    ast::ModuleSet moduleSet(tokenFactory);
    {
        Diagnostics dgn;
        try {
            if ( !moduleSet.init(dgn) ) {
                dgn.dumpErrors(out);
                return EXIT_FAILURE;
            }
        }
        catch (RuntimeException const& e) {
            out("ICE:")(e.file())(":")(e.line())(": ")(e.what())();
            return EXIT_FAILURE;
        }
        catch (std::exception const& e) {
            out("ICE: ")(e.what())();
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
        catch (RuntimeException const& e) {
            out(*m)(": ICE:")(e.file())(":")(e.line())(": ")(e.what())();
            return EXIT_FAILURE;
        }
        catch (std::exception const& e) {
            out(*m)(": ICE: ")(e.what())();
            return EXIT_FAILURE;
        }
        catch (Diagnostics*) {
            // Handled below
        }

        parseTime = sw.reset();
        dgn.dumpErrors(out);
        out("parse: ")(*m)("; errors: ")(dgn.errorCount())("; time: ")(parseTime.count())();

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
    codegen::llvm::Generator gen(dgn, moduleSet);

    try {
        gen.generate(moduleSet.axioms());
    }
    catch (RuntimeException const& e) {
        out(moduleSet.axioms())(": ICE:")(e.file())(":")(e.line())(": ")(e.what())();
        return EXIT_FAILURE;
    }
    catch (std::exception const& e) {
        out(moduleSet.axioms())(": ICE: ")(e.what())();
        return EXIT_FAILURE;
    }
    catch (Diagnostics*) {
        // nop
    }

    if ( dgn.errorCount() ) {
        dgn.dumpErrors(out);
        return EXIT_FAILURE;
    }

    for ( auto m : allModules ) {
        if ( (ret = analyzeModule(out, *m, options & TreeDump)) != EXIT_SUCCESS )
            return ret;

        if ( options & SemanticsOnly )
            continue;

        if ( (ret = codegenModule(out, dgn, gen, *m, options & IR)) != EXIT_SUCCESS )
            return ret;
    }

    return ret;
}

void printHelp(DefaultOutStream& out, std::filesystem::path const& arg0)
{
    auto cmd = arg0.filename().string();

    out(cmd)(
        " COMMAND FILE [FILE2 FILE3 ...]\n"
        "\n"
        "COMMAND:\n"
        "  scan, lexer, lex    Prints the lexer output of the module\n"
        "  parse, grammar      Prints the parse tree as JSON\n"
        "  semantics, sem      Checks the module for semantic errors\n"
        "  semdump             Checks semantics and prints tree\n"
        "  c, compile          Compiles the module\n"
        "  ir                  Generates LLVM IR code"
        )();
}

} // namespace kyfoo

int main(int argc, char const* argv[])
{
    using namespace kyfoo;

    DefaultOutStream out(openStdout());
    try {
        if ( argc < 3 ) {
            printHelp(out, argv[0]);
            return EXIT_FAILURE;
        }

        std::string command = argv[1];
        std::string file = argv[2];

        if ( command == "scan" || command == "lex" || command == "lexer" ) {
            if ( argc != 3 ) {
                printHelp(out, argv[0]);
                return EXIT_FAILURE;
            }

            return runScannerDump(out, file);
        }

        if ( command == "parse" || command == "grammar" ) {
            if ( argc != 3 ) {
                printHelp(out, argv[0]);
                return EXIT_FAILURE;
            }

            return runParserDump(out, file);
        }

        if ( command == "semantics" || command == "sem" || command == "semdump" ) {
            std::vector<std::filesystem::path> files;
            for ( int i = 2; i != argc; ++i )
                files.emplace_back(argv[i]);

            u32 options = SemanticsOnly;
            if ( command == "semdump" )
                options |= TreeDump;

            return compile(out, files, options);
        }

        if ( command == "compile" || command == "c" || command == "ir" ) {
            std::vector<std::filesystem::path> files;
            for ( int i = 2; i != argc; ++i )
                files.emplace_back(argv[i]);

            return compile(out, files, command == "ir" ? IR : None);
        }

        out("Unknown option: ")(command)();
        printHelp(out, argv[0]);
    }
    catch (kyfoo::RuntimeException const& e) {
        out("ICE:")(e.file())(":")(e.line())(": ")(e.what())();
    }
    catch (std::exception const& e) {
        out("ICE: ")(e.what())();
    }

    return EXIT_FAILURE;
}
