#include <kyfoo/Diagnostics.hpp>

#include <kyfoo/ast/Module.hpp>
#include <kyfoo/ast/Declarations.hpp>
#include <kyfoo/ast/Overloading.hpp>
#include <kyfoo/ast/Scopes.hpp>
#include <kyfoo/ast/Semantics.hpp>

namespace kyfoo {

//
// ContextReference

ContextReference::~ContextReference()
{
    if ( myKind == SeeLookup )
        delete myContext.miss;
}

ContextReference::Ctx::Ctx(ast::Lookup&& miss)
    : miss(new ast::Lookup(std::move(miss)))
{
}

//
// Error

Error::Error(ast::Module const& mod)
    : myModule(&mod)
{
}

Error::Error(ast::Module const& mod, Code code)
    : myModule(&mod)
    , myCode(code)
{
}

Error::Error(ast::Module const& mod,
             Error::Code code,
             lexer::Token const& tok)
    : myModule(&mod)
    , myCode(code)
    , myContext(&tok)
{
}

Error::Error(ast::Module const& mod,
             Code code,
             ast::Expression const& expr)
    : myModule(&mod)
    , myCode(code)
    , myContext(&expr)
{
}

Error::Error(ast::Module const& mod,
             Code code,
             ast::Statement const& stmt)
    : myModule(&mod)
    , myCode(code)
    , myContext(&stmt)
{
}

Error::Error(ast::Module const& mod,
             Code code,
             ast::Junction const& junc)
    : myModule(&mod)
    , myCode(code)
    , myContext(&junc)
{
}

Error::Error(ast::Module const& mod,
             Code code,
             ast::Declaration const& decl)
    : myModule(&mod)
    , myCode(code)
    , myContext(&decl)
{
}

ast::Module const& Error::module() const
{
    return *myModule;
}

Error::Code Error::code() const
{
    return myCode;
}

Error::context_t const& Error::context() const
{
    return myContext;
}

std::string Error::what() const
{
    return myInfo.str();
}

Slice<ContextReference const> Error::references() const
{
    return myReferences;
}

Error& Error::see(ast::Declaration const& declaration)
{
    myReferences.emplace_back(declaration);
    return *this;
}

Error& Error::see(ast::Scope const& scope,
                  ast::Expression const& expression)
{
    myReferences.emplace_back(ContextReference::ExpressionContextBase::Value, scope, expression);
    return *this;
}

Error& Error::see(ast::Lookup&& miss)
{
    myReferences.emplace_back(std::move(miss));
    return *this;
}

Error& Error::expected(ast::Scope const& scope, Slice<ast::Expression const*> exprs)
{
    myReferences.emplace_back(ContextReference::MismatchExpected,
                              ContextReference::ExpressionContextBase::Value,
                              scope,
                              exprs);
    return *this;
}

Error& Error::expectedTypes(ast::Scope const& scope, Slice<ast::Expression const*> exprs)
{
    myReferences.emplace_back(ContextReference::MismatchExpected,
                              ContextReference::ExpressionContextBase::Type,
                              scope,
                              exprs);
    return *this;
}

Error& Error::received(ast::Scope const& scope, Slice<ast::Expression const*> exprs)
{
    myReferences.emplace_back(ContextReference::MismatchReceived,
                              ContextReference::ExpressionContextBase::Value,
                              scope,
                              exprs);
    return *this;
}

Error& Error::receivedTypes(ast::Scope const& scope, Slice<ast::Expression const*> exprs)
{
    myReferences.emplace_back(ContextReference::MismatchReceived,
                              ContextReference::ExpressionContextBase::Type,
                              scope,
                              exprs);
    return *this;
}

std::ostream& Error::stream()
{
    return myInfo;
}

Error& Error::operator << (lexer::Token const& token)
{
    myInfo << '\'' << token.lexeme() << '\'';
    return *this;
}

lexer::Token const& front(Error::context_t const& ctx)
{
    return std::visit([](auto r) -> lexer::Token const& { return ast::front(*r); }, ctx);
}

struct PrintId {
    std::ostream& stream;

    void operator()(lexer::Token const* tok) const
    {
        stream << "'" << tok->lexeme() << "'";
    }

    void operator()(ast::Expression const* expr) const
    {
        stream << "'";
        print(stream, *expr);
        stream << "'";
    }

    void operator()(ast::Statement const* stmt) const
    {
        return operator()(&front(*stmt));
    }

    void operator()(ast::Junction const* junc) const
    {
        return operator()(&front(*junc));
    }

    void operator()(ast::Declaration const* decl) const
    {
        if ( auto proc = decl->as<ast::ProcedureDeclaration>() )
            return operator()(proc->scope().declaration());

        return operator()(&decl->symbol().token());
    }
};

std::ostream& operator << (std::ostream& sink, Error const& err)
{
    auto startLine = [&sink](ast::Module const& mod,
                             lexer::SourceLocation loc) -> std::ostream& {
        return sink << mod << ':' << loc;
    };

    auto const& frontToken = front(err.context());
    startLine(err.module(), frontToken.location()) << ": error: ";

    auto printId = [&sink](Error const& err) -> std::ostream& {
        std::visit(PrintId{sink}, err.context());
        return sink;
    };

    switch (err.code()) {
    case Error::General:
        printId(err) << " " << err.what() << std::endl;
        break;

    case Error::Undeclared:
        printId(err);
        sink << ": undeclared identifier" << std::endl;
        break;

    default:
        throw std::runtime_error("unknown error");
    }

    for ( auto const& r : err.references() ) {
        if ( auto decl = r.seeDecl() ) {
            startLine(decl->scope().module(),
                      decl->token().location())
                 << ":  see '";
            print(sink, *decl);
            sink << "' declared as " << to_string(decl->kind()) << std::endl;
        }
        else if ( auto exprCtx = r.seeExpr() ) {
            startLine(exprCtx.scope->module(),
                      front(*exprCtx.expr).location())
                 << ":  see '";
            print(sink, *exprCtx.expr);
            sink << "'" << std::endl;
        }
        else if ( auto miss = r.seeLookup() ) {
            startLine(err.module(), frontToken.location());
            switch ( miss->viable().result() ) {
            case ast::ViableSet::None:
                if ( miss->symSpace() ) {
                    sink << ": did not match any of:\n";
                    for ( auto const& p : miss->symSpace()->prototypes() ) {
                        auto d = p.proto.decl;
                        startLine(d->scope().module(), d->token().location())
                            << ":  ";
                        print(sink, *d);
                        sink << std::endl;
                    }
                }
                else {
                    sink << ": did not match anything" << std::endl;
                }
                break;

            case ast::ViableSet::Single:
            case ast::ViableSet::NeedsConversion:
                sink << ": has a matching overload" << std::endl;
                break;

            case ast::ViableSet::Ambiguous:
                sink << ": could be one of:\n";
                auto const& set = miss->viable();
                auto const rank = set.best().rank();
                for ( auto const& v : set ) {
                    if ( v.rank() != rank )
                        break;

                    auto d = v.prototype().proto.decl;
                    startLine(d->scope().module(), d->token().location())
                        << ":  ";
                    print(sink, *d);
                    sink << std::endl;
                }
                break;
            }
        }
        else if ( auto expected = r.expected() ) {
            startLine(expected.scope->module(),
                      front(*expected.exprs.front()).location())
                 << ":  expected: ";
            if ( expected.isValue() )
                sink << ast::get_types(expected.exprs);
            else
                sink << expected.exprs;
            sink << std::endl;
        }
        else if ( auto received = r.received() ) {
            startLine(received.scope->module(),
                      front(*received.exprs.front()).location())
                 << ":  received: ";
            if ( received.isValue() )
                sink << ast::get_types(received.exprs);
            else
                sink << received.exprs;
            sink << std::endl;
        }
    }

    return sink;
}

//
// Diagnostics

void Diagnostics::die()
{
    throw this;
}

Error& Diagnostics::error(ast::Module const& mod)
{
    myErrors.emplace_back(mk<Error>(mod));
    return *myErrors.back();
}

Error& Diagnostics::error(ast::Module const& mod, lexer::Token const& token)
{
    myErrors.emplace_back(mk<Error>(mod, Error::Code::General, token));
    return *myErrors.back();
}

Error& Diagnostics::error(ast::Module const& mod, ast::Expression const& expr)
{
    myErrors.emplace_back(mk<Error>(mod, Error::Code::General, expr));
    return *myErrors.back();
}

Error& Diagnostics::error(ast::Module const& mod, ast::Statement const& stmt)
{
    myErrors.emplace_back(mk<Error>(mod, Error::Code::General, stmt));
    return *myErrors.back();
}

Error& Diagnostics::error(ast::Module const& mod, ast::Junction const& junc)
{
    myErrors.emplace_back(mk<Error>(mod, Error::Code::General, junc));
    return *myErrors.back();
}

Error& Diagnostics::error(ast::Module const& mod, ast::Declaration const& decl)
{
    myErrors.emplace_back(mk<Error>(mod, Error::Code::General, decl));
    return *myErrors.back();
}

void Diagnostics::dumpErrors(std::ostream& stream)
{
    for ( auto&& e : myErrors )
        stream << *e;
}

uz Diagnostics::errorCount() const
{
    return myErrors.size();
}

void Diagnostics::bunkExpression(Box<ast::Expression> expr)
{
    myBunkedExpressions.emplace_back(std::move(expr));
}

//
// DiagnosticsContext

DiagnosticsContext::DiagnosticsContext(Diagnostics& dgn, ast::Module const& mod)
    : myDiagnostics(&dgn)
    , myModule(&mod)
{
}

Error& DiagnosticsContext::error()
{
    return myDiagnostics->error(*myModule);
}

Error& DiagnosticsContext::error(lexer::Token const& token)
{
    return myDiagnostics->error(*myModule, token);
}

Error& DiagnosticsContext::error(ast::Expression const& expr)
{
    return myDiagnostics->error(*myModule, expr);
}

Error& DiagnosticsContext::error(ast::Statement const& stmt)
{
    return myDiagnostics->error(*myModule, stmt);
}

Error& DiagnosticsContext::error(ast::Junction const& junc)
{
    return myDiagnostics->error(*myModule, junc);
}

Error& DiagnosticsContext::error(ast::Declaration const& decl)
{
    return myDiagnostics->error(*myModule, decl);
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
// misc

std::ostream& operator << (std::ostream& sink, ast::Module const& mod)
{
    if ( mod.path().empty() )
        return sink << mod.name();

    return sink << relative(mod.path(), mod.moduleSet().path()).string();
}

std::ostream& operator << (std::ostream& sink, lexer::SourceLocation loc)
{
    return sink << loc.line << ':' << loc.column;
}

} // namespace kyfoo
