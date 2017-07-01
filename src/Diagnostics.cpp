#include <kyfoo/Diagnostics.hpp>

#include <kyfoo/ast/Module.hpp>
#include <kyfoo/ast/Declarations.hpp>

namespace kyfoo {

//
// Error

Error::Error(ast::Module const* module)
    : myModule(module)
{
}

Error::Error(ast::Module const* module,
             lexer::Token const& token)
    : Error(module, token, General)
{
}

Error::Error(ast::Module const* module,
             lexer::Token const& token,
             Error::Code code)
    : myModule(module)
    , myToken(token)
    , myCode(code)
{
}

Error::Error(ast::Module const* module,
             ast::Expression const& expr,
             Code code)
    : myModule(module)
    , myExpression(&expr)
    , myToken(front(expr))
    , myCode(code)
{
}

ast::Module const* Error::module() const
{
    return myModule;
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

std::vector<ast::Declaration const*> const& Error::references() const
{
    return myReferences;
}

void Error::see(ast::Declaration const* declaration)
{
    myReferences.push_back(declaration);
}

Error& Error::operator << (lexer::Token const& token)
{
    myInfo << '\'' << token.lexeme() << '\'';
    return *this;
}

std::ostream& operator << (std::ostream& sink, Error const& err)
{
    auto startLine = [&] {
        if ( !err.module()->path().empty() )
            sink << err.module()->path().string();
        else
            sink << err.module()->name();
    };
    startLine();
    sink << "(" << err.token().line() << ", " << err.token().column() << "): error: ";

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

    for ( auto&& e : err.references() ) {
        startLine();
        auto const& id = e->identifier();
        sink << "(" << id.line() << ", " << id.column() << "):     see '" << id.lexeme() << "' declared as " << to_string(e->kind()) << std::endl;
    }

    return sink;
}

//
// Diagnostics

void Diagnostics::die()
{
    throw this;
}

Error& Diagnostics::error(ast::Module const* module)
{
    myErrors.emplace_back(std::make_unique<Error>(module));
    return *myErrors.back();
}

Error& Diagnostics::error(ast::Module const* module, lexer::Token const& token)
{
    myErrors.emplace_back(std::make_unique<Error>(module, token));
    return *myErrors.back();
}

Error& Diagnostics::error(ast::Module const* module, ast::Expression const& expr)
{
    myErrors.emplace_back(std::make_unique<Error>(module, expr, Error::Code::General));
    return *myErrors.back();
}

Error& Diagnostics::undeclared(ast::Module const* module, lexer::Token const& token)
{
    myErrors.emplace_back(std::make_unique<Error>(module, token, Error::Undeclared));
    return *myErrors.back();
}

void Diagnostics::dumpErrors(std::ostream& stream)
{
    for ( auto&& e : myErrors )
        stream << *e;
}

std::size_t Diagnostics::errorCount() const
{
    return myErrors.size();
}

} // namespace kyfoo
