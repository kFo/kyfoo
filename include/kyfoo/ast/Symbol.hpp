#pragma once

#include <memory>
#include <string>
#include <vector>

#include <kyfoo/lexer/Token.hpp>
#include <kyfoo/ast/IO.hpp>

namespace kyfoo {
    namespace ast {

class Expression;
class TupleExpression;

class Symbol : public IIO
{
public:
    using paramlist_t = std::vector<std::unique_ptr<Expression>>;

public:
    Symbol(lexer::Token const& identifier,
           std::vector<std::unique_ptr<Expression>>&& parameters);
    Symbol(lexer::Token const& identifier,
           std::unique_ptr<TupleExpression> symbolTuple);
    Symbol(std::unique_ptr<TupleExpression> symbolTuple);
    explicit Symbol(lexer::Token const& identifier);

    Symbol(Symbol const&) = delete;

    Symbol(Symbol&& rhs);
    Symbol& operator = (Symbol&& rhs);

    ~Symbol();

    // IIO
public:
    void io(IStream& stream) const override;

public:
    lexer::Token const& identifier() const;
    std::string const& name() const;
    paramlist_t const& parameters() const;

private:
    lexer::Token myIdentifier;
    paramlist_t myParameters;
};

class Declaration;
class SymbolSet
{
public:
    using paramlist_t = Symbol::paramlist_t;

    struct pair_t {
        paramlist_t const* paramlist;
        Declaration* declaration;
    };

public:
    explicit SymbolSet(std::string const& name);

    SymbolSet(SymbolSet const&) = delete;

    SymbolSet(SymbolSet&& rhs);
    SymbolSet& operator = (SymbolSet&& rhs);

    ~SymbolSet();

public:
    bool operator < (std::string const& rhs) const { return myName < rhs; }
    bool operator == (std::string const& rhs) const { return myName == rhs; }

public:
    std::string const& name() const;

    void append(paramlist_t const& paramlist, Declaration& declaration);
    Declaration* find(paramlist_t const& paramlist);

private:
    std::string myName;
    std::vector<pair_t> mySet;
};

    } // namesapce ast
} // namespace kyfoo
