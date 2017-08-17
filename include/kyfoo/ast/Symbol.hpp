#pragma once

#include <map>
#include <memory>
#include <ostream>
#include <string>
#include <vector>

#include <kyfoo/FlatMap.hpp>
#include <kyfoo/Slice.hpp>
#include <kyfoo/lexer/Token.hpp>
#include <kyfoo/ast/IO.hpp>
#include <kyfoo/ast/Node.hpp>

namespace kyfoo {

    class Diagnostics;

    namespace ast {

class Context;
class Declaration;
class DeclarationScope;
class IResolver;
class Expression;
class SymbolExpression;
class SymbolDeclaration;
class SymbolVariable;

using binding_set_t = FlatMap<SymbolVariable const*, Expression const*>;
using Pattern = std::vector<std::unique_ptr<Expression>>;

class PatternsPrototype : public IIO
{
public:
    PatternsPrototype();
    PatternsPrototype(std::vector<std::unique_ptr<Expression>>&& pattern);

protected:
    PatternsPrototype(PatternsPrototype const& rhs);
    PatternsPrototype& operator = (PatternsPrototype const& rhs);

public:
    PatternsPrototype(PatternsPrototype&& rhs);
    PatternsPrototype& operator = (PatternsPrototype&& rhs);

    ~PatternsPrototype();

    void swap(PatternsPrototype& rhs);

    // IIO
public:
    void io(IStream& stream) const override;

public:
    DECL_CLONE_ALL_NOBASE(PatternsPrototype)

public:
    void resolveSymbols(Diagnostics& dgn, IResolver& resolver);

public:
    Slice<Expression*> pattern() const;
    Slice<SymbolVariable*> symbolVariables() const;

public:
    void bindVariables(Context& ctx, binding_set_t const& bindings);
    SymbolVariable* findVariable(std::string const& identifier);
    SymbolVariable const* findVariable(std::string const& identifier) const;
    SymbolVariable* createVariable(lexer::Token const& identifier);
    bool isConcrete() const;
    bool hasFreeVariables() const;

private:
    Pattern myPattern;
    std::vector<std::unique_ptr<SymbolVariable>> myVariables;
};

class Symbol : public IIO
{
public:
    friend class SymbolSpace;

public:
    Symbol(lexer::Token const& identifier,
           PatternsPrototype&& params);
    explicit Symbol(lexer::Token const& identifier);

protected:
    Symbol(Symbol const& rhs);
    Symbol& operator = (Symbol const& rhs);

public:
    Symbol(Symbol&& rhs);
    Symbol& operator = (Symbol&& rhs);

    ~Symbol();

    void swap(Symbol& rhs);

public:
    void io(IStream& stream) const override;

public:
    DECL_CLONE_ALL_NOBASE(Symbol);

public:
    void resolveSymbols(Diagnostics& dgn, IResolver& resolver);

public:
    lexer::Token const& identifier() const;
    PatternsPrototype const& prototype() const;
    Symbol const* prototypeParent() const;

private:
    lexer::Token myIdentifier;
    std::unique_ptr<PatternsPrototype> myPrototype;
    Symbol const* myPrototypeParent = nullptr;
};

class SymbolReference
{
public:
    using pattern_t = Slice<Expression*>;

public:
    SymbolReference(const char* name, pattern_t pattern);
    /*implicit*/ SymbolReference(Symbol const& sym);
    /*implicit*/ SymbolReference(std::string const& name);
    /*implicit*/ SymbolReference(const char* name);
    ~SymbolReference();

public:
    const char* name() const;
    pattern_t const& pattern() const;

private:
    char const* myName;
    pattern_t myPattern;
};

class SymbolSpace
{
public:
    using pattern_t = SymbolReference::pattern_t;

    struct PatternsDecl {
        PatternsPrototype const* params;
        Declaration const* decl;
    };

    struct Prototype {
        PatternsDecl proto;
        std::vector<PatternsDecl> instances;
    };

    struct DeclInstance {
        Declaration const* parent;
        Declaration const* instance;
    };

public:
    SymbolSpace(DeclarationScope* scope, std::string const& name);

    SymbolSpace(SymbolSpace const& rhs) = delete;
    SymbolSpace& operator = (SymbolSpace const& rhs) = delete;

    SymbolSpace(SymbolSpace&& rhs);
    SymbolSpace& operator = (SymbolSpace&& rhs);

    ~SymbolSpace();

    void swap(SymbolSpace& rhs);

public:
    std::string const& name() const;
    Slice<Prototype> prototypes() const;

    void append(Context& ctx,
                PatternsPrototype const& prototype,
                Declaration& declaration);

    Declaration const* findEquivalent(Diagnostics& dgn, pattern_t const& paramlist) const;
    Declaration* findEquivalent(Diagnostics& dgn, pattern_t const& paramlist);
    DeclInstance findValue(Diagnostics& dgn, pattern_t const& paramlist);

private:
    DeclInstance instantiate(Context& ctx,
                             Prototype& proto,
                             binding_set_t&& bindingSet);

private:
    DeclarationScope* myScope = nullptr;
    std::string myName;
    std::vector<Prototype> myPrototypes;
};

std::ostream& print(std::ostream& stream, Symbol const& sym);

    } // namesapce ast
} // namespace kyfoo
