#include <kyfoo/ast/Semantics.hpp>

#include <kyfoo/ast/Declarations.hpp>
#include <kyfoo/ast/Module.hpp>
#include <kyfoo/ast/Scopes.hpp>
#include <kyfoo/ast/Symbol.hpp>

namespace kyfoo {
    namespace ast {

//
// ScopeResolver

ScopeResolver::ScopeResolver(DeclarationScope* scope)
    : myScope(scope)
{
}

Module const* ScopeResolver::module() const
{
    return myScope->module();
}

Declaration const* ScopeResolver::inScope(std::string const& symbol) const
{
    auto decl = myScope->find(symbol);
    if ( !decl && myScope->declaration() )
        return myScope->declaration()->symbol().findVariable(symbol);

    return nullptr;
}

Declaration const* ScopeResolver::lookup(std::string const& symbol) const
{
    for ( auto scope = myScope; scope; scope = scope->parent() ) {
        if ( auto d = scope->find(symbol) )
            return d;
        
        if ( auto decl = scope->declaration() )
            if ( auto s = decl->symbol().findVariable(symbol) )
                return s;
    }

    for ( auto m : module()->imports() )
        if ( auto decl = m->scope()->find(symbol) )
            return decl;

    return nullptr;
}

//
// SymbolVariableCreatorFailoverResolver

SymbolVariableCreatorFailoverResolver::SymbolVariableCreatorFailoverResolver(IResolver& resolver, Symbol& symbol)
    : myResolver(&resolver)
    , mySymbol(&symbol)
{
}

SymbolVariableCreatorFailoverResolver::~SymbolVariableCreatorFailoverResolver() = default;

Module const* SymbolVariableCreatorFailoverResolver::module() const
{
    return myResolver->module();
}

Declaration const* SymbolVariableCreatorFailoverResolver::inScope(std::string const& symbol) const
{
    return myResolver->inScope(symbol);
}

Declaration const* SymbolVariableCreatorFailoverResolver::lookup(std::string const& symbol) const
{
    if ( auto decl = myResolver->lookup(symbol) )
        return decl;

    return mySymbol->createVariable(symbol);
}

//
// operators

struct SemanticOperator {
    void operator()(PrimaryExpression const& lhs, PrimaryExpression    const& rhs) = delete;
    void operator()(PrimaryExpression const& lhs, TupleExpression      const& rhs) = delete;
    void operator()(PrimaryExpression const& lhs, ConstraintExpression const& rhs) = delete;

    void operator()(TupleExpression const& lhs, PrimaryExpression    const& rhs) = delete;
    void operator()(TupleExpression const& lhs, TupleExpression      const& rhs) = delete;
    void operator()(TupleExpression const& lhs, ConstraintExpression const& rhs) = delete;

    void operator()(ConstraintExpression const& lhs, PrimaryExpression    const& rhs) = delete;
    void operator()(ConstraintExpression const& lhs, TupleExpression      const& rhs) = delete;
    void operator()(ConstraintExpression const& lhs, ConstraintExpression const& rhs) = delete;
};

template <typename O>
auto commute(O& o, Expression const& lhs, Expression const& rhs)
{
    if ( auto l = lhs.as<PrimaryExpression>() ) {
        if ( auto r = rhs.as<PrimaryExpression>() )    return o(*l, *r);
        if ( auto r = rhs.as<TupleExpression>() )      return o(*l, *r);
        if ( auto r = rhs.as<ConstraintExpression>() ) return o(*l, *r);

        goto L_error;
    }
    
    if ( auto l = lhs.as<TupleExpression>() ) {
        if ( auto r = rhs.as<TupleExpression>() )      return o(*l, *r);
        if ( auto r = rhs.as<ConstraintExpression>() ) return o(*l, *r);

        goto L_error;
    }
    
    if ( auto l = lhs.as<ConstraintExpression>() ) {
        if ( auto r = rhs.as<ConstraintExpression>() ) return o(*l, *r);
    }
    
L_error:
    throw std::runtime_error("invalid dispatch");
}

template <typename O>
auto noncommute(O& o, Expression const& lhs, Expression const& rhs)
{
    if ( auto l = lhs.as<PrimaryExpression>() ) {
        if ( auto r = rhs.as<PrimaryExpression>() )    return o(*l, *r);
        if ( auto r = rhs.as<TupleExpression>() )      return o(*l, *r);
        if ( auto r = rhs.as<ConstraintExpression>() ) return o(*l, *r);
        goto L_error;
    }
    else if ( auto l = lhs.as<TupleExpression>() ) {
        if ( auto r = rhs.as<PrimaryExpression>() )         return o(*l, *r);
        else if ( auto r = rhs.as<TupleExpression>() )      return o(*l, *r);
        else if ( auto r = rhs.as<ConstraintExpression>() ) return o(*l, *r);
        goto L_error;
    }
    else if ( auto l = lhs.as<ConstraintExpression>() ) {
        if ( auto r = rhs.as<PrimaryExpression>() )         return o(*l, *r);
        else if ( auto r = rhs.as<TupleExpression>() )      return o(*l, *r);
        else if ( auto r = rhs.as<ConstraintExpression>() ) return o(*l, *r);
    }

L_error:
    throw std::runtime_error("invalid dispatch");
}

struct Difference {

    bool operator()(PrimaryExpression const& lhs, PrimaryExpression    const& rhs)
    {
        return lhs.token().lexeme() != rhs.token().lexeme();
    }

    bool operator()(PrimaryExpression const& lhs, TupleExpression      const& rhs)
    {
        (void)lhs;
        (void)rhs;
        return true;
    }

    bool operator()(PrimaryExpression const& lhs, ConstraintExpression const& rhs)
    {
        (void)lhs;
        (void)rhs;
        return true;
    }

    bool operator()(TupleExpression const& lhs, TupleExpression const& rhs)
    {
        if ( lhs.kind() != rhs.kind() )
            return true;

        if ( lhs.expressions().size() != rhs.expressions().size() )
            return true;

        // TODO: recursive call
        return false;
    }

    bool operator()(TupleExpression const& lhs, ConstraintExpression const& rhs)
    {
        (void)lhs;
        (void)rhs;
        return true;
    }

    bool operator()(ConstraintExpression const& lhs, ConstraintExpression const& rhs)
    {
        (void)lhs;
        (void)rhs;
        // TODO: recursive call
        return false;
    }
};

bool difference(Expression const& lhs, Expression const& rhs)
{
    Difference op;
    return !commute(op, lhs, rhs);
}

    } // namespace ast
} // namespace kyfoo
