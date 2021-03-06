#pragma once

#include <algorithm>

#include <kyfoo/Array.hpp>
#include <kyfoo/Slice.hpp>
#include <kyfoo/ast/Declarations.hpp>
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
    ab<Viability>::ConstIterator begin() const;
    ab<Viability>::Iterator begin();

    ab<Viability>::ConstIterator end() const;
    ab<Viability>::Iterator end();

    bool empty() const;
    uz card() const;

    Viability const& operator [] (uz index) const;
    Viability& operator [] (uz index);

    Variance variance() const;

    explicit operator bool() const;

private:
    ab<Viability> myViabilities;
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
    uz card() const;

    ab<Via>::ConstIterator begin() const;
    ab<Via>::Iterator       begin();

    ab<Via>::ConstIterator end() const;
    ab<Via>::Iterator       end();

    Via const& operator [] (uz index) const;
    Via& operator [] (uz index);

    Declaration const* single() const;
    Declaration* single();

    Via& best();
    Via const& best() const;

    Result result() const;

public:
    void append(OverloadViability viability, Prototype& proto, Substitutions substs);
    void merge(ViableSet rhs);

    void condense(Context& ctx);
    void clear();

private:
    ab<Via> myVias;
    Declaration* myDeclaration = nullptr;
};

inline ab<Via>::ConstIterator begin(ViableSet const& v) { return v.begin(); }
inline ab<Via>::Iterator       begin(ViableSet      & v) { return v.begin(); }
inline ab<Via>::ConstIterator end  (ViableSet const& v) { return v.end  (); }
inline ab<Via>::Iterator       end  (ViableSet      & v) { return v.end  (); }

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
    ab<PatternsDecl> instances;

    ab<Box<Declaration>> ownDeclarations;
    ab<Box<Scope>> ownDefinitions;
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
    stringv name() const;
    Slice<Prototype const> prototypes() const;

    void append(PatternsPrototype const& prototype,
                Declaration& declaration);

    Declaration const* findEquivalent(Slice<Expression const*> paramlist) const;
    Declaration* findEquivalent(Slice<Expression const*> paramlist);
    
    ViableSet findViableOverloads(Context& ctx, Slice<Expression const*> paramlist);

private:
    Scope* myScope = nullptr;
    std::string myName;
    ab<Prototype> myPrototypes;
};

class Lookup
{
public:
    explicit Lookup(SymbolReference query);

    Lookup(Lookup const&) = delete;
    void operator = (Lookup const&) = delete;

    Lookup(Lookup&& rhs) noexcept = default;
    Lookup& operator = (Lookup&& rhs) noexcept = default;

    ~Lookup();

    void swap(Lookup& rhs);

public:
    explicit operator bool () const;

public:
    void appendTrace(SymbolSpace const& space);
    Lookup& resolveTo(ViableSet set);
    Lookup& resolveTo(Declaration& decl);

    Lookup& append(Lookup rhs);

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
    ab<SymbolSpace const*> mySpaces;
    ViableSet mySet;
    Declaration* myDecl = nullptr;
};

Via::Rank rank(Variance v, bool hasSubsts);
ProcedureDeclaration const* findImplicitConversion(Context& ctx, Expression const& dest, Expression const& src);
Viability implicitViability(Context& ctx, Expression const& dest, Expression const& src);
Viability implicitViability(Context& ctx, Resolver& implicitResolver, Expression const& dest, Expression const& src);
OverloadViability implicitViability(Context& ctx, Resolver& implicitResolver, Slice<Expression const*> dest, Slice<Expression const*> src);

} // namespace kyfoo::ast
