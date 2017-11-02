#pragma once

#include <vector>
#include <map>
#include <set>

#include <kyfoo/Slice.hpp>
#include <kyfoo/ast/Expressions.hpp>
#include <kyfoo/ast/Declarations.hpp>

namespace kyfoo {
    namespace lexer {
        class Token;
    }

    namespace ast {

class Module;

template <template<class> typename Op>
class ShallowApply
{
public:
    using operator_t = Op<ShallowApply>;

    ShallowApply()
        : myOperator(*this)
    {
    }

    template <typename... Args>
    ShallowApply(Args&&... args)
        : myOperator(*this, std::forward<Args>(args)...)
    {
    }

    template <typename U>
    typename operator_t::result_t operator()(U&) = delete;

    template<>
    typename operator_t::result_t operator()(Expression const& expr)
    {
#define X(a,b) if ( auto e = expr.as<b>() ) return myOperator.expr##a(*e);
        EXPRESSION_KINDS(X)
#undef X

        throw std::runtime_error("invalid expression kind");
    }

    template<>
    typename operator_t::result_t operator()(Expression& expr)
    {
#define X(a,b) if ( auto e = expr.as<b>() ) return myOperator.expr##a(*e);
        EXPRESSION_KINDS(X)
#undef X

        throw std::runtime_error("invalid expression kind");
    }

    template <>
    typename operator_t::result_t operator()(Declaration& decl)
    {
#define X(a,b,c) if ( auto d = decl.as<c>() ) return myOperator.decl##a(*d);
        DECLARATION_KINDS(X)
#undef X

        throw std::runtime_error("invalid declaration kind");
    }

    template <>
    typename operator_t::result_t operator()(Declaration const& decl)
    {
#define X(a,b,c) if ( auto d = decl.as<c>() ) return myOperator.decl##a(*d);
        DECLARATION_KINDS(X)
#undef X

        throw std::runtime_error("invalid declaration kind");
    }

private:
    operator_t myOperator;
};

template <template<class> typename Op>
class DeepApply
{
public:
    using operator_t = Op<DeepApply>;

    DeepApply()
        : myOperator(*this)
    {
    }

    template <typename... Args>
    DeepApply(Args&&... args)
        : myOperator(*this, std::forward<Args>(args)...)
    {
    }

    template <typename U>
    typename operator_t::result_t operator()(U&) = delete;

    template<>
    typename operator_t::result_t operator()(Expression const& expr)
    {
#define X(a,b) if ( auto e = expr.as<b>() ) { myOperator.expr##a(*e); for ( auto c : e->constraints() ) operator()(*c); return; }
        EXPRESSION_KINDS(X)
#undef X

        throw std::runtime_error("invalid expression kind");
    }

    template<>
    typename operator_t::result_t operator()(Expression& expr)
    {
#define X(a,b) if ( auto e = expr.as<b>() ) { myOperator.expr##a(*e); for ( auto c : e->constraints() ) operator()(*c); return; }
        EXPRESSION_KINDS(X)
#undef X

        throw std::runtime_error("invalid expression kind");
    }

    template <>
    typename operator_t::result_t operator()(Declaration& decl)
    {
#define X(a,b,c) if ( auto d = decl.as<c>() ) return myOperator.decl##a(*d);
        DECLARATION_KINDS(X)
#undef X

        throw std::runtime_error("invalid declaration kind");
    }

    template <>
    typename operator_t::result_t operator()(Declaration const& decl)
    {
#define X(a,b,c) if ( auto d = decl.as<c>() ) return myOperator.decl##a(*d);
        DECLARATION_KINDS(X)
#undef X

        throw std::runtime_error("invalid declaration kind");
    }

private:
    operator_t myOperator;
};

struct SymbolDependencyTracker
{
    struct SymGroup {
        std::string name;
        std::size_t arity;
        int pass = 0;
        std::vector<Declaration*> declarations;
        std::vector<SymGroup*> dependents;

        SymGroup(std::string const& name, std::size_t arity)
            : name(name)
            , arity(arity)
        {
        }

        bool operator < (SymGroup const& rhs) const
        {
            return std::tie(name, arity) < std::tie(rhs.name, rhs.arity);
        }

        bool operator == (SymGroup const& rhs) const
        {
            return std::tie(name, arity) == std::tie(rhs.name, rhs.arity);
        }

        void add(Declaration& decl)
        {
            declarations.push_back(&decl);
        }

        void addDependent(SymGroup& group)
        {
            dependents.push_back(&group);
        }

        bool defer(SymGroup* startSym, int deferPass)
        {
            if ( pass >= deferPass )
                return true;

            pass = deferPass;
            for ( auto& d : dependents ) {
                if ( d == startSym )
                    return false;

                d->defer(startSym, pass + 1);
            }

            return true;
        }
    };

    Module& mod;
    Diagnostics& dgn;
    std::vector<std::unique_ptr<SymGroup>> groups;

    SymbolDependencyTracker(Module& mod, Diagnostics& dgn);

    SymGroup* create(std::string const& name, std::size_t arity);
    SymGroup* findOrCreate(std::string const& name, std::size_t arity);

    void add(Declaration& decl);
    void addDependency(Declaration& decl,
                       std::string const& name,
                       std::size_t arity);

    void sortPasses();
};

void traceDependencies(SymbolDependencyTracker& tracker, Declaration& decl);

enum Variance
{
    Covariant,
    Exact,
    Contravariant,
    Invariant,
};

class VarianceResult
{
    Variance myVariance = Invariant;

public:
    /*implicit*/ VarianceResult(Variance variance)
        : myVariance(variance)
    {
    }

    bool covariant() const { return myVariance <= Exact; }
    bool exact() const { return myVariance == Exact; }
    bool contravariant() const { return myVariance == Contravariant; }
    bool invariant() const { return myVariance == Invariant; }

    Variance value() const { return myVariance; }

    explicit operator bool() const { return covariant(); }
};

VarianceResult variance(lexer::Token const& target, lexer::Token const& query);
VarianceResult variance(Context& ctx, Declaration const& target, lexer::Token const& query);
VarianceResult variance(Context& ctx, Declaration const& target, Declaration const& query);
VarianceResult variance(Context& ctx, binding_set_t& leftBindings, Expression const& lhs, Expression const& rhs);
VarianceResult variance(Context& ctx, binding_set_t& leftBindings, Slice<Expression*> lhs, Slice<Expression*> rhs);
VarianceResult variance(Context& ctx, Slice<Expression*> lhs, Slice<Expression*> rhs);
VarianceResult variance(Context& ctx, SymbolReference const& lhs, SymbolReference const& rhs);

Expression const* lookThrough(Declaration const* decl);
Declaration const* resolveIndirections(Declaration const* decl);
Expression const* resolveIndirections(Expression const* expr);
Symbol const* rootTemplate(Symbol const& symbol);
bool descendsFromTemplate(Symbol const& parent, Symbol const& instance);
DeclarationScope const* memberScope(Declaration const& decl);
Declaration const* outerDataDeclaration(Declaration const& decl);
Declaration* outerDataDeclaration(Declaration& decl);
Declaration const* callingContextDeclaration(Declaration const& decl);
Declaration* callingContextDeclaration(Declaration& decl);
Declaration const* dataType(Declaration const* decl);
Declaration const* dataType(Context& ctx, Slice<Expression*> constraints);

// todo: functor like ValueMatcher
bool matchEquivalent(Expression const& lhs, Expression const& rhs);
bool matchEquivalent(Slice<Expression*> lhs, Slice<Expression*> rhs);

std::vector<PrimaryExpression*> gatherFreeVariables(Expression& expr);
bool hasFreeVariable(Expression const& expr);

    } // namespace ast
} // namespace kyfoo
