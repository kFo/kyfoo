#include "Axioms.hpp"

#include <kyfoo/ast/Expressions.hpp>
#include <kyfoo/ast/Declarations.hpp>
#include <kyfoo/ast/Module.hpp>
#include <kyfoo/ast/Scopes.hpp>

namespace kyfoo {
    namespace ast {

lexer::Token identifier(const char* name) { return lexer::Token(lexer::TokenKind::Identifier, 0, 0, name); }
lexer::Token integer(int value) { return lexer::Token(lexer::TokenKind::Integer, 0, 0, std::to_string(value)); }

std::unique_ptr<Module> importAxioms()
{
    auto scope = std::make_unique<DeclarationScope>(nullptr);

    std::vector<TypeParameter> params;
    params.emplace_back(std::make_unique<PrimaryExpression>(integer(32)));
    scope->append(std::make_unique<TypeDeclaration>(identifier("integer"), std::move(params)));

    return std::make_unique<Module>("axioms", std::move(scope));
}

    } // namespace ast
} // namespace kyfoo
