#pragma once

#include <algorithm>
#include <vector>

#include <kyfoo/Slice.hpp>
#include <kyfoo/ast/Substitutions.hpp>
#include <kyfoo/ast/Symbol.hpp>
#include <kyfoo/ast/Variance.hpp>

namespace kyfoo::ast {

class Context;
class Declaration;
class Expression;
class PatternsPrototype;
class ProcedureDeclaration;
struct Prototype;
class SymbolSpace;
class SymbolVariable;

class Viability
{
public:
    explicit Viability(Variance v, ProcedureDeclaration const* conversion);

public:
    Variance variance() const;
    ProcedureDeclaration const* conversion() const;

public:
    explicit operator bool() const;

private:
    Variance myVariance;
    ProcedureDeclaration const* myConversion;
};

class OverloadViability
{
public:
    explicit OverloadViability() = default;

    OverloadViability(OverloadViability const&) = delete;
    void operator = (OverloadViability const&) = delete;

    OverloadViability(OverloadViability&&) = default;
    OverloadViability& operator = (OverloadViability&&) = default;

    ~OverloadViability() = default;

public:
    void append(Variance v, ProcedureDeclaration const* conversion);
    void append(Viability pv);

public:
    std::vector<Viability>::const_iterator begin() const;
    std::vector<Viability>::iterator begin();

    std::vector<Viability>::const_iterator end() const;
    std::vector<Viability>::iterator end();

    bool empty() const;
    uz size() const;

    Viability const& operator [] (uz index) const;
    Viability& operator [] (uz index);

    Variance variance() const;

    explicit operator bool() const;

private:
    std::vector<Viability> myViabilities;
};

class Via
{
public:
    enum Rank
    {
        Exact,
        Parametric,
        Covariant,
        Conversion,
    };

public:
    Via(Rank rank, OverloadViability&& viability, Prototype& proto, Substitutions&& substs);

    Via(Via const&) = delete;
    void operator = (Via const&) = delete;

    Via(Via&&) = default;
    Via& operator = (Via&&) = default;

    ~Via() = default;

public:
    Declaration* instantiate(Context& ctx);

public:
    Rank rank() const;

    OverloadViability const& viability() const;
    OverloadViability& viability();

    Prototype const& prototype() const;
    Prototype& prototype();

    bool operator < (Via const& rhs) const;

private:
    Rank myRank;
    OverloadViability myViability;
    Prototype* myProto;
    Substitutions mySubsts;
};

class ViableSet
{
public:
    enum Result
    {
        None,
        Single,
        NeedsConversion,
        Ambiguous,
    };

public:
    ViableSet() = default;

    ViableSet(ViableSet const&) = delete;
    void operator = (ViableSet const&) = delete;

    ViableSet(ViableSet&&) = default;
    ViableSet& operator = (ViableSet&&) = default;

    ~ViableSet() = default;

    void swap(ViableSet& rhs) noexcept;

public:
    bool empty() const;
    uz size() const;

    std::vector<Via>::const_iterator begin() const;
    std::vector<Via>::const_iterator end() const;

    Via const& operator [] (uz index) const;
    Via& operator [] (uz index);

    Declaration const* single() const;
    Declaration* single();

    Via& best();
    Via const& best() const;

    Result result() const;

public:
    void append(OverloadViability&& viability, Prototype& proto, Substitutions&& substs);
    void merge(ViableSet&& rhs);

    void condense(Context& ctx);
    void clear();

private:
    std::vector<Via> myVias;
    Declaration* myDeclaration = nullptr;
};

struct PatternsDecl {
    PatternsPrototype const* params;
    Declaration* decl;
};

struct Prototype {
    Prototype(PatternsPrototype const& proto, Declaration& decl)
        : proto({&proto, &decl})
    {
    }

    PatternsDecl proto;
    std::vector<PatternsDecl> instances;

    std::vector<Box<Declaration>> ownDeclarations;
    std::vector<Box<Scope>> ownDefinitions;
};

class SymbolSpace
{
public:
    SymbolSpace(Scope* scope, std::string name);

    SymbolSpace(SymbolSpace const& rhs) = delete;
    SymbolSpace& operator = (SymbolSpace const& rhs) = delete;

    SymbolSpace(SymbolSpace&& rhs);
    SymbolSpace& operator = (SymbolSpace&& rhs);

    ~SymbolSpace();

    void swap(SymbolSpace& rhs) noexcept;

public:
    std::string_view name() const;
    Slice<Prototype const> prototypes() const;

    void append(PatternsPrototype const& prototype,
                Declaration& declaration);

    Declaration const* findEquivalent(Slice<Expression const*> paramlist) const;
    Declaration* findEquivalent(Slice<Expression const*> paramlist);
    
    ViableSet findViableOverloads(Context& ctx, Slice<Expression const*> paramlist);

private:
    Scope* myScope = nullptr;
    std::string myName;
    std::vector<Prototype> myPrototypes;
};

class Lookup
{
public:
    explicit Lookup(SymbolReference query);

    Lookup(Lookup const&) = delete;
    void operator = (Lookup const&) = delete;

    Lookup(Lookup&& rhs);
    Lookup& operator = (Lookup&& rhs);

    ~Lookup();

    void swap(Lookup& rhs);

public:
    explicit operator bool () const;

public:
    void appendTrace(SymbolSpace const& space);
    Lookup& resolveTo(ViableSet&& set);
    Lookup& resolveTo(Declaration& decl);

    Lookup& append(Lookup&& rhs);

public:
    SymbolReference query() const;

    SymbolSpace const* symSpace() const;
    Slice<SymbolSpace const* const> trace() const;

    ViableSet const& viable() const;
    ViableSet& viable();

    Declaration const* single() const;
    Declaration* single();

    template <typename T>
    T const* singleAs() const
    {
        if ( single() )
            return single()->as<T>();

        return nullptr;
    }

    template <typename T>
    T* singleAs()
    {
        if ( single() )
            return single()->as<T>();

        return nullptr;
    }

private:
    SymbolReference myQuery;
    std::vector<SymbolSpace const*> mySpaces;
    ViableSet mySet;
    Declaration* myDecl = nullptr;
};

ProcedureDeclaration const* findImplicitConversion(Context& ctx, Expression const& dest, Expression const& src);
Viability implicitViability(Context& ctx, Expression const& dest, Expression const& src);
OverloadViability implicitViability(Context& ctx, Slice<Expression const*> dest, Slice<Expression const*> src);

} // namespace kyfoo::ast
