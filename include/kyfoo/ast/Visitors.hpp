#pragma once

namespace kyfoo::ast {

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

    // Expressions

    typename operator_t::result_t operator()(Expression const& expr)
    {
#define X(a,b) if ( auto e = expr.as<b>() ) return myOperator.expr##a(*e);
        EXPRESSION_KINDS(X)
#undef X

        throw std::runtime_error("invalid expression kind");
    }

    typename operator_t::result_t operator()(Expression& expr)
    {
#define X(a,b) if ( auto e = expr.as<b>() ) return myOperator.expr##a(*e);
        EXPRESSION_KINDS(X)
#undef X

        throw std::runtime_error("invalid expression kind");
    }

    // Statements

    typename operator_t::result_t operator()(Statement const& stmt)
    {
#define X(a,b) if ( auto e = stmt.as<b>() ) return myOperator.stmt##a(*e);
        STATEMENT_KINDS(X)
#undef X

        throw std::runtime_error("invalid statement kind");
    }

    typename operator_t::result_t operator()(Statement& stmt)
    {
#define X(a,b) if ( auto e = stmt.as<b>() ) return myOperator.stmt##a(*e);
        STATEMENT_KINDS(X)
#undef X

        throw std::runtime_error("invalid statement kind");
    }

    // Junctions

    typename operator_t::result_t operator()(Junction const& junc)
    {
#define X(a,b) if ( auto e = junc.as<b>() ) return myOperator.junc##a(*e);
        JUNCTION_KINDS(X)
#undef X

        throw std::runtime_error("invalid control-junction kind");
    }

    typename operator_t::result_t operator()(Junction& junc)
    {
#define X(a,b) if ( auto e = junc.as<b>() ) return myOperator.junc##a(*e);
        JUNCTION_KINDS(X)
#undef X

        throw std::runtime_error("invalid control-junction kind");
    }

    // Declarations

    typename operator_t::result_t operator()(Declaration& decl)
    {
#define X(a,b,c) if ( auto d = decl.as<c>() ) return myOperator.decl##a(*d);
        DECLARATION_KINDS(X)
#undef X

        throw std::runtime_error("invalid declaration kind");
    }

    typename operator_t::result_t operator()(Declaration const& decl)
    {
#define X(a,b,c) if ( auto d = decl.as<c>() ) return myOperator.decl##a(*d);
        DECLARATION_KINDS(X)
#undef X

        throw std::runtime_error("invalid declaration kind");
    }

    operator_t& getOperator()
    {
        return myOperator;
    }

    void procScope(ProcedureScope& p)
    {
        for ( auto& bb : p.basicBlocks() ) {
            for ( auto& s : bb->statements() )
                operator()(*s);

            if ( bb->junction() )
                operator()(*bb->junction());
        }
    }

private:
    operator_t myOperator;
};

template <template<class> typename Op>
class DeepApply
{
public:
    using operator_t = Op<DeepApply>;
    using result_t = typename operator_t::result_t;

    DeepApply()
        : myOperator(*this)
    {
    }

    template <typename... Args>
    DeepApply(Args&&... args)
        : myOperator(*this, std::forward<Args>(args)...)
    {
    }

    result_t operator()(Expression const& expr)
    {
#define X(a,b) if ( auto e = expr.as<b>() ) { result_t ret = myOperator.expr##a(*e); for ( auto c : e->constraints() ) ret |= operator()(*c); return ret; }
        EXPRESSION_KINDS(X)
#undef X

        throw std::runtime_error("invalid expression kind");
    }

    result_t operator()(Expression& expr)
    {
#define X(a,b) if ( auto e = expr.as<b>() ) { result_t ret = myOperator.expr##a(*e); for ( auto c : e->constraints() ) ret |= operator()(*c); return ret; }
        EXPRESSION_KINDS(X)
#undef X

        throw std::runtime_error("invalid expression kind");
    }

    result_t operator()(Declaration& decl)
    {
#define X(a,b,c) if ( auto d = decl.as<c>() ) return myOperator.decl##a(*d);
        DECLARATION_KINDS(X)
#undef X

        throw std::runtime_error("invalid declaration kind");
    }

    result_t operator()(Declaration const& decl)
    {
#define X(a,b,c) if ( auto d = decl.as<c>() ) return myOperator.decl##a(*d);
        DECLARATION_KINDS(X)
#undef X

        throw std::runtime_error("invalid declaration kind");
    }

    result_t operator()(Slice<Expression*> exprs)
    {
        auto ret = result_t();
        for ( ; exprs; exprs.popFront() )
            ret |= operator()(*exprs.front());

        return ret;
    }

    result_t operator()(Slice<Expression const*> exprs)
    {
        result_t ret;
        for ( ; exprs; exprs.popFront() )
            ret |= operator()(*exprs.front());

        return ret;
    }

private:
    operator_t myOperator;
};

template <typename O>
auto noncommute(O& o, Expression const& lhs, Expression const& rhs)
{
    auto other = [&o, &rhs](auto l) {
#define RHS(a,b) if ( auto r = rhs.as<b>() ) return o(*l, *r);
        EXPRESSION_KINDS(RHS)
#undef RHS
        throw std::runtime_error("invalid dispatch");
    };

#define LHS(a,b) if ( auto l = lhs.as<b>() ) return other(l);
    EXPRESSION_KINDS(LHS)
#undef LHS

    throw std::runtime_error("invalid dispatch");
}

} // namespace kyfoo::ast
