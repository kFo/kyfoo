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

struct ValueMatcher
{
    Context& ctx;
    binding_set_t leftBindings;
    binding_set_t rightBindings;

    explicit ValueMatcher(Context& ctx);

    void reset();
    bool matchValue(Expression const& lhs, Expression const& rhs);
    bool matchValue(Slice<Expression*> lhs, Slice<Expression*> rhs);
};

void traceDependencies(SymbolDependencyTracker& tracker, Declaration& decl);

bool isCovariant(Context& ctx, lexer::Token const& target, lexer::Token const& query);
bool isCovariant(Context& ctx, Declaration const& target, lexer::Token const& query);
bool isCovariant(Context& ctx, Declaration const& target, Declaration const& query);

Expression const* lookThrough(Declaration const* decl);
Declaration const* resolveIndirections(Declaration const* decl);
Expression const* resolveIndirections(Expression const* expr);
Symbol const* rootTemplate(Symbol const& symbol);
bool descendsFromTemplate(Symbol const& parent, Symbol const& instance);
DeclarationScope const* memberScope(Declaration const& decl);

// todo: functor like ValueMatcher
bool matchEquivalent(Context& ctx, Expression const& lhs, Expression const& rhs);
bool matchEquivalent(Context& ctx, Slice<Expression*> lhs, Slice<Expression*> rhs);

std::vector<PrimaryExpression*> gatherFreeVariables(Expression& expr);
bool hasFreeVariable(Expression const& expr);

    } // namespace ast
} // namespace kyfoo
