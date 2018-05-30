#include <kyfoo/Diagnostics.hpp>

#include <kyfoo/ast/Module.hpp>
#include <kyfoo/ast/Declarations.hpp>
#include <kyfoo/ast/Scopes.hpp>
#include <kyfoo/ast/Semantics.hpp>

namespace kyfoo {

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
    auto startLine = [&](ast::Module const& mod) {
        if ( !mod.path().empty() )
            sink << mod.path().string();
        else
            sink << mod.name();
    };
    startLine(err.module());

    auto startPos = [&](lexer::Token const& tok) {
        sink << "(" << tok.line() << ", " << tok.column() << "): ";
    };
    startPos(err.token());
    sink << "error: ";

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
            startLine(decl->scope().module());
            auto const& id = decl->token();
            startPos(id);
            sink << "    see '";
            print(sink, *decl);
            sink << "' declared as " << to_string(decl->kind()) << std::endl;
        }
        else if ( auto exprCtx = r.seeExpr() ) {
            startLine(exprCtx.scope->module());
            auto const& id = front(*exprCtx.expr);
            startPos(id);
            sink << "    see '";
            print(sink, *exprCtx.expr);
            sink << "'" << std::endl;
        }
        else if ( auto expected = r.expected() ) {
            startLine(expected.scope->module());
            auto const& id = front(*expected.exprs.front());
            startPos(id);
            sink << "    expected: ";
            if ( expected.isValue() )
                sink << ast::get_types(expected.exprs);
            else
                sink << expected.exprs;
            sink << std::endl;
        }
        else if ( auto received = r.received() ) {
            startLine(received.scope->module());
            auto const& id = front(*received.exprs.front());
            startPos(id);
            sink << "    received: ";
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

Error& Diagnostics::error(ast::Module const& module)
{
    myErrors.emplace_back(mk<Error>(module));
    return *myErrors.back();
}

Error& Diagnostics::error(ast::Module const& module, lexer::Token const& token)
{
    myErrors.emplace_back(mk<Error>(module, token));
    return *myErrors.back();
}

Error& Diagnostics::error(ast::Module const& module, ast::Expression const& expr)
{
    myErrors.emplace_back(mk<Error>(module, expr, Error::Code::General));
    return *myErrors.back();
}

Error& Diagnostics::error(ast::Module const& module, ast::Statement const& stmt)
{
    return error(module, front(stmt));
}

Error& Diagnostics::error(ast::Module const& module, ast::Junction const& junc)
{
    return error(module, front(junc));
}

Error& Diagnostics::error(ast::Module const& module, ast::Declaration const& decl)
{
    return error(module, decl.symbol().token());
}

Error& Diagnostics::undeclared(ast::Module const& module, lexer::Token const& token)
{
    myErrors.emplace_back(mk<Error>(module, token, Error::Undeclared));
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

} // namespace kyfoo
