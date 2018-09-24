#pragma once

#include <kyfoo/Slice.hpp>
#include <kyfoo/Diagnostics.hpp>

namespace kyfoo::lexer {
    class Token;
}

namespace kyfoo::ast {

class Expression;
class Declaration;
class SymbolReference;

class Variance
{
public:
    enum Kind
    {
        Covariant,
        Exact,
        Contravariant,
        Invariant,
    };

public:
    /*implicit*/ Variance(Kind kind)
        : myKind(kind)
    {
    }

public:
    bool covariant() const { return myKind <= Exact; }
    bool exact() const { return myKind == Exact; }
    bool contravariant() const { return myKind == Contravariant; }
    bool invariant() const { return myKind == Invariant; }

    Kind value() const { return myKind; }

    explicit operator bool() const { return covariant(); }

private:
    Kind myKind = Invariant;
};

Variance variance(lexer::Token const& target, lexer::Token const& query);
Variance variance(DiagnosticsContext dgn, Declaration const&       target, lexer::Token     const&  query);
Variance variance(DiagnosticsContext dgn, Declaration const&       target, Declaration      const&  query);
Variance variance(DiagnosticsContext dgn, Expression  const&       target, Expression       const&  query);
Variance variance(DiagnosticsContext dgn, Slice<Expression const*> lhs   , Slice<Expression const*> rhs);
Variance variance(DiagnosticsContext dgn, Expression const&        lhs   , Slice<Expression const*> rhs);
Variance variance(DiagnosticsContext dgn, Slice<Expression const*> lhs   , Expression       const&  rhs);
Variance variance(DiagnosticsContext dgn, SymbolReference const&   lhs   , SymbolReference  const&  rhs);

} // namespace kyfoo::ast
