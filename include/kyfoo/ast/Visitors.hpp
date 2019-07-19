#pragma once

namespace kyfoo::ast {

template <template<class> typename Op>
class ShallowApply
{
public:
    using Operator = Op<ShallowApply>;

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

    typename Operator::Result operator()(Expression const& expr)
    {
#define X(a,b) if ( auto e = expr.as<b>() ) return myOperator.expr##a(*e);
        EXPRESSION_KINDS(X)
#undef X

        ENFORCEU("invalid expression kind");
    }

    typename Operator::Result operator()(Expression& expr)
    {
#define X(a,b) if ( auto e = expr.as<b>() ) return myOperator.expr##a(*e);
        EXPRESSION_KINDS(X)
#undef X

        ENFORCEU("invalid expression kind");
    }

    // Statements

    typename Operator::Result operator()(Statement const& stmt)
    {
#define X(a,b) if ( auto e = stmt.as<b>() ) return myOperator.stmt##a(*e);
        STATEMENT_KINDS(X)
#undef X

        ENFORCEU("invalid statement kind");
    }

    typename Operator::Result operator()(Statement& stmt)
    {
#define X(a,b) if ( auto e = stmt.as<b>() ) return myOperator.stmt##a(*e);
        STATEMENT_KINDS(X)
#undef X

        ENFORCEU("invalid statement kind");
    }

    // Junctions

    typename Operator::Result operator()(Junction const& junc)
    {
#define X(a,b) if ( auto e = junc.as<b>() ) return myOperator.junc##a(*e);
        JUNCTION_KINDS(X)
#undef X

        ENFORCEU("invalid control-junction kind");
    }

    typename Operator::Result operator()(Junction& junc)
    {
#define X(a,b) if ( auto e = junc.as<b>() ) return myOperator.junc##a(*e);
        JUNCTION_KINDS(X)
#undef X

        ENFORCEU("invalid control-junction kind");
    }

    // Declarations

    typename Operator::Result operator()(Declaration& decl)
    {
#define X(a,b,c) if ( auto d = decl.as<c>() ) return myOperator.decl##a(*d);
        DECLARATION_KINDS(X)
#undef X

        ENFORCEU("invalid declaration kind");
    }

    typename Operator::Result operator()(Declaration const& decl)
    {
#define X(a,b,c) if ( auto d = decl.as<c>() ) return myOperator.decl##a(*d);
        DECLARATION_KINDS(X)
#undef X

        ENFORCEU("invalid declaration kind");
    }

    Operator& getOperator()
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
    Operator myOperator;
};

template <template<class> typename Op>
class DeepApply
{
public:
    using Operator = Op<DeepApply>;
    using Result = typename Operator::Result;

    DeepApply()
        : myOperator(*this)
    {
    }

    template <typename... Args>
    DeepApply(Args&&... args)
        : myOperator(*this, std::forward<Args>(args)...)
    {
    }

    Result operator()(Expression const& expr)
    {
#define X(a,b) if ( auto e = expr.as<b>() ) { Result ret = myOperator.expr##a(*e); for ( auto c : e->constraints() ) ret |= operator()(*c); return ret; }
        EXPRESSION_KINDS(X)
#undef X

        ENFORCEU("invalid expression kind");
    }

    Result operator()(Expression& expr)
    {
#define X(a,b) if ( auto e = expr.as<b>() ) { Result ret = myOperator.expr##a(*e); for ( auto c : e->constraints() ) ret |= operator()(*c); return ret; }
        EXPRESSION_KINDS(X)
#undef X

        ENFORCEU("invalid expression kind");
    }

    Result operator()(Declaration& decl)
    {
#define X(a,b,c) if ( auto d = decl.as<c>() ) return myOperator.decl##a(*d);
        DECLARATION_KINDS(X)
#undef X

        ENFORCEU("invalid declaration kind");
    }

    Result operator()(Declaration const& decl)
    {
#define X(a,b,c) if ( auto d = decl.as<c>() ) return myOperator.decl##a(*d);
        DECLARATION_KINDS(X)
#undef X

        ENFORCEU("invalid declaration kind");
    }

    Result operator()(Slice<Expression*> exprs)
    {
        auto ret = Result();
        for ( ; exprs; exprs.popFront() )
            ret |= operator()(*exprs.front());

        return ret;
    }

    Result operator()(Slice<Expression const*> exprs)
    {
        Result ret;
        for ( ; exprs; exprs.popFront() )
            ret |= operator()(*exprs.front());

        return ret;
    }

private:
    Operator myOperator;
};

template <typename O>
auto noncommute(O& o, Expression const& lhs, Expression const& rhs)
{
    auto other = [&o, &rhs](auto l) {
#define RHS(a,b) if ( auto r = rhs.as<b>() ) return o(*l, *r);
        EXPRESSION_KINDS(RHS)
#undef RHS
        ENFORCEU("invalid dispatch");
    };

#define LHS(a,b) if ( auto l = lhs.as<b>() ) return other(l);
    EXPRESSION_KINDS(LHS)
#undef LHS

    ENFORCEU("invalid dispatch");
}

} // namespace kyfoo::ast
