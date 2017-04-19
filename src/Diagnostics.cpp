#include <kyfoo/Diagnostics.hpp>

#include <kyfoo/ast/Declarations.hpp>

namespace kyfoo {

//
// Error

Error::Error(lexer::Token const& token)
    : Error(token, General)
{
}

Error::Error(lexer::Token const& token, Error::Code code)
    : myToken(token)
    , myCode(code)
{
}

std::string Error::what() const
{
    return myInfo.str();
}

lexer::Token const& Error::token() const
{
    return myToken;
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

Error& Error::operator << (std::string const& rhs)
{
    myInfo << rhs;
    return *this;
}

std::ostream& operator << (std::ostream& sink, Error const& err)
{
    sink << '(' << err.token().line() << ", " << err.token().column() << "): error: ";
    if ( !err.token().lexeme().empty() )
        sink << "'" << err.token().lexeme() << "' ";

    sink << err.what() << std::endl;

    for ( auto&& e : err.references() ) {
        auto const& id = e->identifier();
        sink << '(' << id.line() << ", " << id.column() << "):     see '" << id.lexeme() << "' declared as " << to_string(e->kind()) << std::endl;
    }

    return sink;
}

//
// Diagnostics

void Diagnostics::die()
{
    throw this;
}

Error& Diagnostics::error(lexer::Token const& token)
{
    myErrors.emplace_back(std::make_unique<Error>(token));
    return *myErrors.back();
}

Error& Diagnostics::undeclared(lexer::Token const& token)
{
    myErrors.emplace_back(std::make_unique<Error>(token, Error::Undeclared));
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
