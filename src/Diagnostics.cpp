#include <kyfoo/Diagnostics.hpp>

#include <kyfoo/Stream.hpp>

#include <kyfoo/lexer/Token.hpp>

#include <kyfoo/ast/Module.hpp>
#include <kyfoo/ast/Declarations.hpp>
#include <kyfoo/ast/Expressions.hpp>
#include <kyfoo/ast/Overloading.hpp>
#include <kyfoo/ast/Scopes.hpp>
#include <kyfoo/ast/Semantics.hpp>

namespace kyfoo {

namespace {
    constexpr const char* toString(diag d) {
        const char* DIAG_STRING[] = {
        #define X(a,b) b,
            DEFINE_DIAGNOSTIC_KINDS(X)
        #undef X
        };

        return DIAG_STRING[unsigned(d)];
    }
}

//
// Error

Report::Report(ast::Module const& mod, diag code)
    : myModule(&mod)
    , myCode(code)
{
}

Report::Report(ast::Module const& mod,
             diag code,
             Subject subj)
    : myModule(&mod)
    , myCode(code)
    , mySubject(subj)
{
}

ast::Module const& Report::module() const
{
    return *myModule;
}

diag Report::code() const
{
    return myCode;
}

Report::Subject Report::subject() const
{
    return mySubject;
}

Slice<Word const> Report::sentence() const
{
    return mySentence;
}

void Report::append(Word word)
{
    mySentence.emplace_back(std::move(word));
}

lexer::Token const& front(Report::Subject const& subj)
{
    return *subj.visit([](auto const& r) -> lexer::Token const* { return &ast::front(r); });
}

//
// Diagnostics

Diagnostics::Diagnostics() noexcept = default;

Diagnostics::~Diagnostics() noexcept = default;

void Diagnostics::die(const char* reason)
{
    myDieReason = reason;
    throw this;
}

ReportProxy Diagnostics::error(ast::Module const& mod, diag code)
{
    myErrors.emplace_back(mk<Report>(mod, code));
    return { *this, *myErrors.back() };
}

ReportProxy Diagnostics::error(ast::Module const& mod, diag code, Report::Subject subj)
{
    myErrors.emplace_back(mk<Report>(mod, code, subj));
    return { *this, *myErrors.back() };
}

const char* Diagnostics::dieReason() const
{
    return myDieReason;
}

uz Diagnostics::errorCount() const
{
    return myErrors.size();
}

void Diagnostics::dumpErrors(DefaultOutStream& stream)
{
    for ( auto const& e : myErrors )
        stream(*e);
}

ast::Expression const& Diagnostics::bunkExpression(Box<ast::Expression> expr)
{
    myBunkedExpressions.emplace_back(std::move(expr));
    return *myBunkedExpressions.back();
}

ast::Lookup const& Diagnostics::bunkLookup(ast::Lookup lookup)
{
    myBunkedLookups.emplace_back(mk<ast::Lookup>(std::move(lookup)));
    return *myBunkedLookups.back();
}

//
// DiagnosticsContext

DiagnosticsContext::DiagnosticsContext(Diagnostics& dgn, ast::Module const& mod)
    : myDiagnostics(&dgn)
    , myModule(&mod)
{
}

ReportProxy DiagnosticsContext::error(diag code)
{
    return myDiagnostics->error(*myModule, code);
}

ReportProxy DiagnosticsContext::error(diag code, Report::Subject subj)
{
    return myDiagnostics->error(*myModule, code, subj);
}

Diagnostics& DiagnosticsContext::diagnostics()
{
    return *myDiagnostics;
}

ast::Module const& DiagnosticsContext::module()
{
    return *myModule;
}

//
// ReportProxy

ReportProxy& ReportProxy::see(ast::Declaration const& declaration)
{
    myReport->append(Anchor(declaration));
    return *this;
}

ReportProxy& ReportProxy::see(ast::Scope const& scope,
                              ast::Expression const& expression)
{
    myReport->append(Anchor(SingleExpressionAnchor(scope, expression)));
    return *this;
}

ReportProxy& ReportProxy::see(ast::Lookup miss)
{
    myReport->append(myDiagnostics->bunkLookup(std::move(miss)));
    return *this;
}

ReportProxy& ReportProxy::expected(ast::Scope const& scope, Slice<ast::Expression const*> exprs)
{
    myReport->append(Anchor(MultiExpressionAnchor(scope, exprs)));
    return *this;
}

ReportProxy& ReportProxy::received(ast::Scope const& scope, Slice<ast::Expression const*> exprs)
{
    myReport->append(Anchor(MultiExpressionAnchor(scope, exprs)));
    return *this;
}

//
// misc

void reportSubject(DefaultOutStream& sink, Subject subj)
{
    if ( auto tok = subj.as<lexer::Token>() ) {
        sink("'")(tok->lexeme())("'");
        return;
    }

    if ( auto expr = subj.as<ast::Expression>() ){
        sink("'")(*expr)("'");
        return;
    }

    if ( auto stmt = subj.as<ast::Statement>() ) {
        sink("'")(front(*stmt).lexeme())("'");
        return;
    }

    if ( auto junc = subj.as<ast::Junction>() ) {
        sink("'")(front(*junc).lexeme())("'");
        return;
    }

    if ( auto decl = subj.as<ast::Declaration>() ) {
        for (;;) {
            if ( auto proc = decl->as<ast::ProcedureDeclaration>() ) {
                decl = proc->scope().declaration();
                continue;
            }
            break;
        }

        sink("'")(decl->symbol().token().lexeme())("'");
        return;
    }

    ENFORCEU("invalid subject");
}

void reportLine(DefaultOutStream& sink, ast::Module const& mod, lexer::SourceLocation loc)
{
    sink(mod)(':')(loc);
}

void reportAnchor(DefaultOutStream& sink, Anchor anchor)
{
    if ( auto decl = anchor.as<ast::Declaration>() ) {
        reportLine(sink, decl->scope().module(), decl->token().location());
        sink(": see '")(*decl)("' declared as ")(to_string(decl->kind()))('\n');
        return;
    }
    
    if ( auto single = anchor.as<SingleExpressionAnchor>() ) {
        reportLine(sink, single->scope().module(), front(single->expr()).location());
        sink(": see '")(single->expr())("'\n");
        return;
    }

    if ( auto multi = anchor.as<MultiExpressionAnchor>() ) {
        reportLine(sink, multi->scope().module(), front(*multi->exprs().front()).location());
        sink(": see: ")(multi->exprs());
        return;
    }

    ENFORCEU("invalid anchor");
}

void reportLookup(DefaultOutStream& sink, ast::Lookup const& lookup)
{
    switch ( lookup.viable().result() ) {
    case ast::ViableSet::None:
        if ( lookup.symSpace() ) {
            sink(": did not match any of:\n");
            for ( auto const& p : lookup.symSpace()->prototypes() ) {
                auto d = p.proto.decl;
                reportLine(sink, d->scope().module(), d->token().location());
                sink(": ")(*d)('\n');
            }
        }
        else {
            sink(": did not match anything\n");
        }
        break;

    case ast::ViableSet::Single:
    case ast::ViableSet::NeedsConversion:
        sink(": has a matching overload\n");
        break;

    case ast::ViableSet::Ambiguous:
        sink(": invocation is ambiguous -- it could be any of these:\n");
        auto const& set = lookup.viable();
        auto const rank = set.best().rank();
        for ( auto const& v : set ) {
            if ( v.rank() != rank )
                break;

            auto d = v.prototype().proto.decl;
            reportLine(sink, d->scope().module(), d->token().location());
            sink(": ")(*d)('\n');
        }
        break;
    }
}

    namespace ascii {
        void write(DefaultOutStream& sink, Report const& err)
        {
            auto const& frontToken = front(err.subject());
            reportLine(sink, err.module(), frontToken.location());
            sink(": error: ");
            reportSubject(sink, err.subject());
            sink(" ")(toString(err.code()))('\n');

            for ( auto& word : err.sentence() ) {
                if ( auto anchor = word.as<Anchor>() ) {
                    reportAnchor(sink, *anchor);
                    continue;
                }

                if ( auto lookup = word.as<ast::Lookup>() ) {
                    reportLine(sink, err.module(), frontToken.location());
                    reportLookup(sink, *lookup);
                    continue;
                }
            }
        }
    }

} // namespace kyfoo
