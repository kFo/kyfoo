#pragma once

#include <map>
#include <memory>
#include <ostream>
#include <string>
#include <vector>

#include <kyfoo/Slice.hpp>
#include <kyfoo/lexer/Token.hpp>
#include <kyfoo/ast/IO.hpp>
#include <kyfoo/ast/Node.hpp>
#include <kyfoo/ast/Substitutions.hpp>

namespace kyfoo {

    class Diagnostics;

    namespace ast {

class Context;
class Declaration;
class DeclarationScope;
class IResolver;
class Expression;
class Module;
class LiteralExpression;
class SymbolExpression;
class SymbolDeclaration;
class SymbolVariable;
class SymRes;
class VarianceResult;

using Pattern = std::vector<std::unique_ptr<Expression>>;
using pattern_t = Slice<Expression*>;
using const_pattern_t = Slice<Expression const*>;

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
    void resolveVariables(DeclarationScope const& scope);
    SymRes resolveSymbols(Context& ctx);

public:
    Slice<Expression*> pattern();
    Slice<Expression const*> pattern() const;

    Slice<SymbolVariable const*> symbolVariables() const;

public:
    void bindVariables(Substitutions const& substs);
    SymbolVariable* findVariable(std::string const& token);
    SymbolVariable const* findVariable(std::string const& token) const;
    bool isConcrete() const;
    std::size_t metaVariableCount() const;

private:
    Pattern myPattern;
    std::vector<std::unique_ptr<SymbolVariable>> myVariables;
};

class Symbol : public IIO
{
public:
    friend class SymbolSpace;

public:
    Symbol(lexer::Token const& token,
           PatternsPrototype&& params);
    explicit Symbol(lexer::Token const& token);
    Symbol(std::unique_ptr<SymbolExpression> symExpr);

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
    SymRes resolveSymbols(Context& ctx);

public:
    lexer::Token const& token() const;
    lexer::Token& token();

    PatternsPrototype const& prototype() const;
    PatternsPrototype& prototype();

    Symbol const* prototypeParent() const;

private:
    lexer::Token myToken;
    std::unique_ptr<PatternsPrototype> myPrototype;
    Symbol const* myPrototypeParent = nullptr;
};

class SymbolReference
{
public:
    SymbolReference(const char* name, const_pattern_t pattern);
    SymbolReference(std::string const& name, const_pattern_t pattern);
    /*implicit*/ SymbolReference(Symbol const& sym);
    /*implicit*/ SymbolReference(std::string const& name);
    /*implicit*/ SymbolReference(const char* name);
    ~SymbolReference();

public:
    const char* name() const;
    const_pattern_t const& pattern() const;

private:
    char const* myName;
    const_pattern_t myPattern;
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
    Substitutions substs;
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
                Substitutions&& substs);

private:
    std::vector<Candidate> myCandidates;
};

class SymbolSpace
{
public:
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
    Slice<Prototype const> prototypes() const;

    void append(PatternsPrototype const& prototype,
                Declaration& declaration);

    Declaration const* findEquivalent(const_pattern_t paramlist) const;
    Declaration* findEquivalent(const_pattern_t paramlist);
    
    CandidateSet findCandidates(Module& endModule, Diagnostics& dgn, const_pattern_t paramlist);
    DeclInstance findOverload(Module& endModule, Diagnostics& dgn, const_pattern_t paramlist);

private:
    DeclInstance instantiate(Context& ctx,
                             Prototype& proto,
                             Substitutions&& bindingSet);

private:
    DeclarationScope* myScope = nullptr;
    std::string myName;
    std::vector<Prototype> myPrototypes;

    std::vector<Substitutions> myBunkSubsts;
};

std::ostream& print(std::ostream& stream, Symbol const& sym);

    } // namesapce ast
} // namespace kyfoo
