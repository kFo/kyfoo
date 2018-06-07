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

Error::Error(ast::Module const& module)
    : myModule(&module)
{
}

Error::Error(ast::Module const& module,
             lexer::Token const& token)
    : Error(module, token, General)
{
}

Error::Error(ast::Module const& module,
             lexer::Token const& token,
             Error::Code code)
    : myModule(&module)
    , myToken(token)
    , myCode(code)
{
}

Error::Error(ast::Module const& module,
             ast::Expression const& expr,
             Code code)
    : myModule(&module)
    , myExpression(&expr)
    , myToken(front(expr))
    , myCode(code)
{
}

ast::Module const& Error::module() const
{
    return *myModule;
}

std::string Error::what() const
{
    return myInfo.str();
}

ast::Expression const* Error::expression() const
{
    return myExpression;
}

lexer::Token const& Error::token() const
{
    return myToken;
}

Error::Code Error::code() const
{
    return myCode;
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

Error& Error::see(ast::DeclarationScope const& scope,
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

Error& Error::expected(ast::DeclarationScope const& scope, Slice<ast::Expression const*> exprs)
{
    myReferences.emplace_back(ContextReference::MismatchExpected,
                              ContextReference::ExpressionContextBase::Value,
                              scope,
                              exprs);
    return *this;
}

Error& Error::expectedTypes(ast::DeclarationScope const& scope, Slice<ast::Expression const*> exprs)
{
    myReferences.emplace_back(ContextReference::MismatchExpected,
                              ContextReference::ExpressionContextBase::Type,
                              scope,
                              exprs);
    return *this;
}

Error& Error::received(ast::DeclarationScope const& scope, Slice<ast::Expression const*> exprs)
{
    myReferences.emplace_back(ContextReference::MismatchReceived,
                              ContextReference::ExpressionContextBase::Value,
                              scope,
                              exprs);
    return *this;
}

Error& Error::receivedTypes(ast::DeclarationScope const& scope, Slice<ast::Expression const*> exprs)
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

std::ostream& operator << (std::ostream& sink, Error const& err)
{
    auto startLine = [&sink](ast::Module const& mod,
                             lexer::SourceLocation loc) -> std::ostream& {
        return sink << mod << ':' << loc;
    };

    startLine(err.module(), err.token().location()) << ": error: ";

    switch (err.code()) {
    case Error::General:
        if ( err.expression() ) {
            sink << "'";
            print(sink, *err.expression());
            sink << "' ";
        }
        else if ( !err.token().lexeme().empty() ) {
            sink << "'" << err.token().lexeme() << "' ";
        }

        sink << err.what() << std::endl;
        break;

    case Error::Undeclared:
        sink << "'" << err.token().lexeme() << "': undeclared identifier" << std::endl;
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
            startLine(err.module(), err.token().location());
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
    myErrors.emplace_back(mk<Error>(mod, token));
    return *myErrors.back();
}

Error& Diagnostics::error(ast::Module const& mod, ast::Expression const& expr)
{
    myErrors.emplace_back(mk<Error>(mod, expr, Error::Code::General));
    return *myErrors.back();
}

Error& Diagnostics::error(ast::Module const& mod, ast::Statement const& stmt)
{
    return error(mod, front(stmt));
}

Error& Diagnostics::error(ast::Module const& mod, ast::Junction const& junc)
{
    return error(mod, front(junc));
}

Error& Diagnostics::error(ast::Module const& mod, ast::Declaration const& decl)
{
    return error(mod, decl.symbol().token());
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
