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
class PrimaryExpression;
class SymbolExpression;
class SymbolDeclaration;
class SymbolVariable;
class VarianceResult;

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
    SymbolVariable* createVariable(PrimaryExpression const& primary);
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
    lexer::Token& identifier();

    PatternsPrototype const& prototype() const;
    PatternsPrototype& prototype();

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
    SymbolReference(std::string const& name, pattern_t pattern);
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

struct PatternsDecl {
    PatternsPrototype const* params;
    Declaration* decl;
};

struct Prototype {
    PatternsDecl proto;
    std::vector<PatternsDecl> instances;
};

struct Candidate
{
    enum {
        Exact,
        Parametric,
        Covariant,
    } rank;
    Prototype* proto;
    binding_set_t bindings;
};

class CandidateSet
{
public:
    bool empty() const;
    std::vector<Candidate>::const_iterator begin() const;
    std::vector<Candidate>::const_iterator end() const;
    Candidate const& operator[](std::size_t index) const;
    Candidate& operator[](std::size_t index);

    void append(VarianceResult const& v,
                Prototype& proto,
                binding_set_t&& bindings);

private:
    std::vector<Candidate> myCandidates;
};

class SymbolSpace
{
public:
    using pattern_t = SymbolReference::pattern_t;

    struct DeclInstance {
        Declaration* parent;
        Declaration* instance;
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
    
    CandidateSet findCandidates(Diagnostics& dgn, pattern_t const& paramlist);
    DeclInstance findOverload(Diagnostics& dgn, pattern_t const& paramlist);

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
