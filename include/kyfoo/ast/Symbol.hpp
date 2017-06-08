#pragma once

#include <memory>
#include <string>
#include <vector>

#include <kyfoo/Slice.hpp>
#include <kyfoo/lexer/Token.hpp>
#include <kyfoo/ast/IO.hpp>

namespace kyfoo {

    class Diagnostics;

    namespace ast {

class IResolver;
class Expression;
class SymbolExpression;
class SymbolVariable;

class Symbol : public IIO
{
public:
    using paramlist_t = std::vector<std::unique_ptr<Expression>>;

public:
    Symbol(lexer::Token const& identifier,
           std::vector<std::unique_ptr<Expression>>&& parameters);
    explicit Symbol(lexer::Token const& identifier);

    Symbol(Symbol const&) = delete;

    Symbol(Symbol&& rhs);
    Symbol& operator = (Symbol&& rhs);

    ~Symbol();

    // IIO
public:
    void io(IStream& stream) const override;

public:
    bool operator == (Symbol const& rhs) const;

public:
    void resolveSymbols(Diagnostics& dgn, IResolver& resolver);
    SymbolVariable* findVariable(std::string const& identifier);
    SymbolVariable const* findVariable(std::string const& identifier) const;
    SymbolVariable* createVariable(std::string const& identifier);

public:
    lexer::Token const& identifier() const;
    std::string const& name() const;
    paramlist_t const& parameters() const;

private:
    lexer::Token myIdentifier;
    paramlist_t myParameters;
    std::vector<std::unique_ptr<SymbolVariable>> myVariables;
};

class SymbolReference
{
public:
    using paramlist_t = Slice<Expression*>;

public:
    /*implicit*/ SymbolReference(Symbol const& symbol);
    /*implicit*/ SymbolReference(std::string const& name);
    SymbolReference(std::string const& name, paramlist_t parameters);
    ~SymbolReference();

public:
    std::string const& name() const;
    paramlist_t const& parameters() const;

private:
    std::string const* myName;
    paramlist_t myParameters;
};

class Declaration;
class SymbolSet
{
public:
    using paramlist_t = SymbolReference::paramlist_t;

    struct pair_t {
        std::vector<Expression*> paramlist;
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
    Slice<pair_t> const declarations() const;

    void append(paramlist_t const& paramlist, Declaration& declaration);

    Declaration* findEquivalent(paramlist_t const& paramlist);
    Declaration const* findEquivalent(paramlist_t const& paramlist) const;

    Declaration* findValue(paramlist_t const& paramlist);
    Declaration const* findValue(paramlist_t const& paramlist) const;

private:
    std::string myName;
    std::vector<pair_t> mySet;
};

    } // namesapce ast
} // namespace kyfoo
