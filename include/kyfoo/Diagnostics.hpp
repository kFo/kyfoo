#pragma once

#include <chrono>
#include <sstream>
#include <vector>

#include <kyfoo/Slice.hpp>
#include <kyfoo/Stream.hpp>
#include <kyfoo/Types.hpp>

#include <kyfoo/lexer/Token.hpp>

namespace kyfoo::ast {
    class Expression;
    class Declaration;
    class Scope;
    class Junction;
    class Lookup;
    class Module;
    class Statement;
}

namespace kyfoo {

class StopWatch
{
public:
    StopWatch()
        : myStart(std::chrono::system_clock::now())
    {
    }

    std::chrono::duration<double> elapsed()
    {
        return std::chrono::system_clock::now() - myStart;
    }

    std::chrono::duration<double> reset()
    {
        auto now = std::chrono::system_clock::now();
        auto elapsed = now - myStart;
        myStart = now;
        return elapsed;
    }

private:
    std::chrono::system_clock::time_point myStart;
};

#define DEFINE_DIAGNOSTIC_KINDS(X) \
    X(unresolved_symbol, "unresolved symbols in expression") \
    X(unresolved, "is an unresolved expression") \
    X(unused, "declared but never used") \
    X(multiple_definition, "already defined") \
    X(cycle, "definition cycle") \
    X(ambiguous, "is ambiguous") \
    X(conflict_types, "has conflicting types") \
    X(instantiate_error, "errors occured during instantiation") \
    X(not_implemented, "feature is not implemented") \
    \
    X(no_import, "import does not exist") \
    X(no_type, "expression is not typed") \
    X(no_conversion, "no conversion exists") \
    X(no_invocation, "no suitable invocation was found") \
    X(no_branch, "missing initial branch statement") \
    X(no_match_initial_branch, "match-statements must start with a branch") \
    X(no_match_branches, "match-statement is missing branches") \
    X(no_match_datatype_variations, "data-type does not have any variations") \
    X(no_declaration, "is not declared") \
    X(no_definition, "declaration is missing its definition") \
    X(no_block_jump, "jump does not occur in a block") \
    X(no_block, "block does not exist") \
    X(no_field, "field does not exist") \
    X(no_member, "member does not exist") \
    X(no_member_or_application, "no matching member access or application") \
    X(no_deduction, "no deduction exists for parameter") \
    \
    X(missing_match_variation, "variation is not covered by match-statement") \
    X(missing_control_flow_definition, "is not defined on all paths") \
    \
    X(expected_variation, "expression does not identify a variation") \
    X(expected_datatype, "expression is not a datatype") \
    X(expected_constructor, "expected a constructor") \
    X(expected_terminal_junction, "expected terminating junction") \
    X(expected_procedure, "expected a procedure-declaration") \
    X(expected_symbol_tuple_identifier, "expected identifier at the start of a symbol-tuple") \
    X(expected_integer, "expected integer") \
    X(expected_composite, "expected composite") \
    X(expected_applicable, "expected an applicable data-type") \
    X(expected_tuple, "expected a tuple-expression") \
    X(expected_arity, "expected a different arity than was received") \
    X(expected_one_type, "too many types provided") \
    X(expected_string_escape, "expected a string escape") \
    X(expected_string_escape_two_hex, "expected two hex characters for escape sequence") \
    X(expected_template, "expected template declaration") \
    \
    X(unexpected_meta_variable, "meta-variable not expected") \
    X(unexpected_assign_expression, "assign-expression is in an unexpected context") \
    X(unexpected_negative_integer, "unexpected negative integer") \
    \
    X(module_no_name, "cannot determine output name for module") \
    X(module_cannot_write_object_file, "failed to write object file") \
    X(module_unsupported_target, "cannot emit a file of this type for target machine") \
    \
    X(codegen, "codegen ICE") \
    X(codegen_multiple_definitions, "defined more than once") \
    X(codegen_notype, "does not have a type") \
    \
    X(parser_expected_expression, "expected expression") \
    X(parser_expected_scope, "expected scope") \
    X(parser_expected_declaration, "expected declaration") \
    X(parser_unexpected, "grammar at this point is not recognized") \
    X(parser_expected_preceding_branch, "expcted preceding branch-statement") \
    X(parser_branch_else_proceeds, "else-branch-statement must proceed a branch-statement") \
    X(parser_branch_multiple_else, "preceding branch-statement already has an else-branch-statement") \
    X(parser_unreachable, "is unreachable") \
    X(parser_unexpected_scope, "unexpected scope") \
    X(parser_expected_scope_end, "expected end of scope") \
    X(parser_indentation_mismatch, "indentation doesn't match an existing scope") \
    \
    X(lexer_error, "a lexical error has occured")

enum class diag
{
#define X(a,b) a,
    DEFINE_DIAGNOSTIC_KINDS(X)
#undef X
};

class ExpressionAnchor
{
protected:
    ExpressionAnchor(ast::Scope const& scope)
        : myScope(&scope)
    {
    }

public:
    ast::Scope const& scope() const { return *myScope; }

private:
    ast::Scope const* myScope = nullptr;
};

class SingleExpressionAnchor : public ExpressionAnchor
{
public:
    SingleExpressionAnchor(ast::Scope const& scope, ast::Expression const& expr)
        : ExpressionAnchor(scope)
        , myExpr(&expr)
    {
    }

public:
    ast::Expression const& expr() const { return *myExpr; }

private:
    ast::Expression const* myExpr = nullptr;
};

class MultiExpressionAnchor : public ExpressionAnchor
{
public:
    MultiExpressionAnchor(ast::Scope const& scope,
                          Slice<ast::Expression const*> exprs)
        : ExpressionAnchor(scope)
        , myExprs(exprs)
    {
    }

public:
    Slice<ast::Expression const*> exprs() const { return myExprs; }

private:
    Slice<ast::Expression const*> myExprs;
};

class Anchor
{
public:
    enum class Kind
    {
        Declaration,
        Single,
        Multi,
    };

public:
    /*implicit*/ Anchor(ast::Declaration const& decl)
        : myKind(Kind::Declaration)
        , myDecl(&decl)
    {
    }

    /*implicit*/ Anchor(SingleExpressionAnchor single)
        : myKind(Kind::Single)
        , mySingle(std::move(single))
    {
    }

    /*implicit*/ Anchor(MultiExpressionAnchor multi)
        : myKind(Kind::Multi)
        , myMulti(std::move(multi))
    {
    }

public:
    Kind kind() const { return myKind; }

    template <typename T> T const* as() const = delete;

    template<>
    ast::Declaration const* as<ast::Declaration>() const
    {
        if ( myKind == Kind::Declaration )
            return myDecl;

        return nullptr;
    }

    template<>
    SingleExpressionAnchor const* as<SingleExpressionAnchor>() const
    {
        if ( myKind == Kind::Single )
            return &mySingle;

        return nullptr;
    }

    template<>
    MultiExpressionAnchor const* as<MultiExpressionAnchor>() const
    {
        if ( myKind == Kind::Multi )
            return &myMulti;

        return nullptr;
    }

private:
    Kind myKind;
    union {
        ast::Declaration const* myDecl;
        SingleExpressionAnchor mySingle;
        MultiExpressionAnchor myMulti;
    };
};

class Subject
{
public:
    #define DEFINE_SUBJECT_KINDS(X) \
        X(Token      , lexer::Token    , tok  ) \
        X(Expression , ast::Expression , expr ) \
        X(Statement  , ast::Statement  , stmt ) \
        X(Junction   , ast::Junction   , junc ) \
        X(Declaration, ast::Declaration, decl )

    enum Kind
    {
        None,
        #define X(a,b,c) a,
        DEFINE_SUBJECT_KINDS(X)
        #undef X
    };

    Subject() = default;

    #define X(a,b,c) /*implicit*/ Subject(b const& c) : myKind(a), c(&c) {}
    DEFINE_SUBJECT_KINDS(X)
    #undef X

    Kind kind() const { return myKind; }

    template <typename T> T const* as() const = delete;

    #define X(a,b,c) template<> b const* as<b>() const { if (myKind == a) return c; return nullptr; }
    DEFINE_SUBJECT_KINDS(X)
    #undef X

    template <typename F>
    auto visit(F&& f) const
    {
        switch (myKind)
        {
        #define X(a,b,c) case a: return f(*c);
        DEFINE_SUBJECT_KINDS(X)
        #undef X
        }

        ENFORCEU("unknown word kind");
    }

    template <typename F>
    auto tryVisit(F&& f) const
    {
        switch (myKind)
        {
        #define X(a,b,c) case a: return f(*c);
        DEFINE_SUBJECT_KINDS(X)
        #undef X
        }

        return f(void);
    }

private:
    Kind myKind = None;
    union
    {
        void const* ptr = nullptr;
        #define X(a,b,c) b const* c;
        DEFINE_SUBJECT_KINDS(X)
        #undef X
    };
};

class Word
{
public:
    enum class Kind
    {
        Anchor,
        Lookup,
    };

public:
    /*implicit*/ Word(Anchor rhs)
        : myKind(Kind::Anchor)
        , myAnchor(std::move(rhs))
    {
    }

    /*implicit*/ Word(ast::Lookup const& rhs)
        : myKind(Kind::Lookup)
        , myLookup(&rhs)
    {
    }

public:
    Kind kind() const { return myKind; }

    template <typename T> T const* as() const = delete;

    template<>
    Anchor const* as<Anchor>() const
    {
        if ( myKind == Kind::Anchor )
            return &myAnchor;

        return nullptr;
    }

    template<>
    ast::Lookup const* as<ast::Lookup>() const
    {
        if ( myKind == Kind::Lookup )
            return myLookup;

        return nullptr;
    }

public:
    Kind myKind;
    union {
        Anchor myAnchor;
        ast::Lookup const* myLookup;
    };
};

class Report
{
public:
    using Subject = Subject;

public:
    Report(ast::Module const& mod, diag code);
    Report(ast::Module const& mod, diag code, Subject subj);

    Report(Report& rhs) = delete;
    void operator = (Report&) = delete;

public:
    ast::Module const& module() const;
    diag code() const;
    Subject subject() const;
    Slice<Word const> sentence() const;

public:
    void append(Word word);

private:
    ast::Module const* myModule = nullptr;
    diag myCode;
    Subject mySubject;
    std::vector<Word> mySentence;
};

class ReportProxy;

class Diagnostics
{
public:
    Diagnostics() noexcept;
    ~Diagnostics() noexcept;

public:
    void die(const char* reason = "");

    ReportProxy error(ast::Module const& mod, diag code);
    ReportProxy error(ast::Module const& mod, diag code, Report::Subject subj);

    const char* dieReason() const;
    uz errorCount() const;

    void dumpErrors(DefaultOutStream& stream);

    ast::Expression const& bunkExpression(Box<ast::Expression> expr);
    ast::Lookup const& bunkLookup(ast::Lookup lookup);

private:
    const char* myDieReason = nullptr;
    std::vector<Box<Report>> myErrors;
    std::vector<Box<ast::Expression>> myBunkedExpressions;
    std::vector<Box<ast::Lookup>> myBunkedLookups;
};

class DiagnosticsContext
{
public:
    DiagnosticsContext(Diagnostics& dgn, ast::Module const& mod);

public:
    ReportProxy error(diag code);
    ReportProxy error(diag code, Report::Subject subj);

public:
    Diagnostics& diagnostics();
    ast::Module const& module();

private:
    Diagnostics* myDiagnostics = nullptr;
    ast::Module const* myModule = nullptr;
};

class ReportProxy
{
public:
    /*implicit*/ ReportProxy(Diagnostics& d, Report& r)
        : myDiagnostics(&d)
        , myReport(&r)
    {
    }

public:
    ReportProxy& see(ast::Declaration const& declaration);
    ReportProxy& see(ast::Scope const& scope, ast::Expression const& expression);
    ReportProxy& see(ast::Lookup miss);
    ReportProxy& expected(ast::Scope const& scope, Slice<ast::Expression const*> exprs);
    ReportProxy& received(ast::Scope const& scope, Slice<ast::Expression const*> exprs);

private:
    Diagnostics* myDiagnostics = nullptr;
    Report* myReport = nullptr;
};

namespace ascii {
    void write(DefaultOutStream& sink, Report const& err);
}

} // namespace kyfoo
