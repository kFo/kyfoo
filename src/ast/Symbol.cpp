#include <kyfoo/ast/Symbol.hpp>

#include <tuple>

#include <kyfoo/Diagnostics.hpp>
#include <kyfoo/Utilities.hpp>
#include <kyfoo/ast/Declarations.hpp>
#include <kyfoo/ast/Expressions.hpp>
#include <kyfoo/ast/Context.hpp>
#include <kyfoo/ast/Module.hpp>
#include <kyfoo/ast/Overloading.hpp>
#include <kyfoo/ast/Scopes.hpp>
#include <kyfoo/ast/Semantics.hpp>
#include <kyfoo/ast/Substitutions.hpp>

namespace kyfoo::ast {

//
// PatternsPrototype

PatternsPrototype::PatternsPrototype() = default;

PatternsPrototype::PatternsPrototype(ab<Box<Expression>> pattern)
    : myPattern(std::move(pattern))
{
}

PatternsPrototype::PatternsPrototype(PatternsPrototype const&)
{
    // clone myPattern
    // clone myVariables
}

PatternsPrototype& PatternsPrototype::operator = (PatternsPrototype const& rhs)
{
    PatternsPrototype(rhs).swap(*this);
    return *this;
}

PatternsPrototype::PatternsPrototype(PatternsPrototype&& rhs)
    : myPattern(std::move(rhs.myPattern))
    , myVariables(std::move(rhs.myVariables))
{
}

PatternsPrototype& PatternsPrototype::operator = (PatternsPrototype&& rhs)
{
    this->~PatternsPrototype();
    new (this) PatternsPrototype(std::move(rhs));

    return *this;
}

PatternsPrototype::~PatternsPrototype() = default;

void PatternsPrototype::swap(PatternsPrototype& rhs) noexcept
{
    using kyfoo::swap;
    swap(myPattern, rhs.myPattern);
    swap(myVariables, rhs.myVariables);
}

IMPL_CLONE_NOBASE_BEGIN(PatternsPrototype, PatternsPrototype)
IMPL_CLONE_CHILD(myPattern)
IMPL_CLONE_CHILD(myVariables)
IMPL_CLONE_END
IMPL_CLONE_REMAP_NOBASE_BEGIN(PatternsPrototype)
IMPL_CLONE_REMAP(myPattern)
IMPL_CLONE_REMAP(myVariables)
IMPL_CLONE_REMAP_END

SymRes PatternsPrototype::resolveVariables(Context& ctx)
{
    SymRes ret;
    if ( !myVariables ) {
        for ( auto& param : myPattern ) {
            auto variables = gatherMetaVariables(*param);
            for ( auto p : variables ) {
                if ( auto hit = ctx.matchOverload(p->token().lexeme()) ) {
                    ctx.error(diag::multiple_definition, p->token())
                        .see(*hit.single());
                    ret |= SymRes::Fail;
                    continue;
                }

                auto existing = std::find_if(begin(myVariables), end(myVariables), [&](auto const& var) {
                    return var->symbol().token().lexeme() == p->token().lexeme();
                    });
                if ( existing != end(myVariables) ) {
                    ctx.error(diag::multiple_definition, p->token())
                        .see(**existing);
                    ret |= SymRes::Fail;
                    continue;
                }

                myVariables.append(mk<SymbolVariable>(p->token(), ctx.resolver().scope(), p->takeConstraints()));
                p->setDeclaration(*myVariables.back());
            }
        }
    }

    return ret;
}

SymRes PatternsPrototype::resolveSymbols(Context& ctx)
{
    Resolver resolver(ctx.resolver().scope());
    resolver.addSupplementaryPrototype(*this);
    REVERT = ctx.pushResolver(resolver);
    return ctx.resolveExpressions(myPattern);
}

Slice<Expression*> PatternsPrototype::pattern()
{
    return myPattern;
}

Slice<Expression const*> PatternsPrototype::pattern() const
{
    return myPattern;
}

Slice<SymbolVariable const*> PatternsPrototype::symbolVariables() const
{
    return myVariables;
}

void PatternsPrototype::bindVariables(Substitutions const& substs)
{
    ENFORCE(substs.card() == myVariables.card(), "template parameter binding mismatch");

    for ( uz i = 0; i < substs.card(); ++i ) {
        auto const& key = substs.var(i);
        auto const& value = substs.expr(i);
        auto var = findVariable(key.symbol().token().lexeme());
        ENFORCE(var, "template parameter binding mismatch");

        var->bindExpression(&value);
    }
}

SymbolVariable* PatternsPrototype::findVariable(stringv token)
{
    for ( auto& e : myVariables )
        if ( e->symbol().token().lexeme() == token )
            return e.get();

    return nullptr;
}

SymbolVariable const* PatternsPrototype::findVariable(stringv token) const
{
    return const_cast<PatternsPrototype*>(this)->findVariable(token);
}

bool PatternsPrototype::isConcrete() const
{
    for ( auto const& v : myVariables ) {
        if ( !lookThrough(v.get()) )
            return false;
    }

    return true;
}

uz PatternsPrototype::metaVariableCount() const
{
    return myVariables.card();
}

//
// Symbol

Symbol::Symbol(lexer::Token token,
               PatternsPrototype params)
    : myToken(std::move(token))
    , myPrototype(mk<PatternsPrototype>(std::move(params)))
{
}

Symbol::Symbol(lexer::Token token)
    : Symbol(std::move(token), PatternsPrototype())
{
}

Symbol::Symbol(Box<SymbolExpression> symExpr)
    : myToken(symExpr->token())
    , myPrototype(mk<PatternsPrototype>(std::move(symExpr->internalExpressions())))
{
}

Symbol::Symbol(Symbol const& rhs)
    : myToken(rhs.myToken)
    , myPrototypeParent(&rhs)
{
    // clone myPrototype
}

Symbol& Symbol::operator = (Symbol const& rhs)
{
    Symbol(rhs).swap(*this);
    return *this;
}

Symbol::Symbol(Symbol&& rhs)
    : myToken(std::move(rhs.myToken))
    , myPrototype(std::move(rhs.myPrototype))
    , myPrototypeParent(rhs.myPrototypeParent)
{
    rhs.myPrototypeParent = nullptr;
}

Symbol& Symbol::operator = (Symbol&& rhs)
{
    this->~Symbol();
    new (this) Symbol(std::move(rhs));
    return *this;
}

Symbol::~Symbol() = default;

void Symbol::swap(Symbol& rhs) noexcept
{
    using kyfoo::swap;
    swap(myToken, rhs.myToken);
    swap(myPrototype, rhs.myPrototype);
    swap(myPrototypeParent, rhs.myPrototypeParent);
}

IMPL_CLONE_NOBASE_BEGIN(Symbol, Symbol)
IMPL_CLONE_CHILD(myPrototype)
IMPL_CLONE_END
IMPL_CLONE_REMAP_NOBASE_BEGIN(Symbol)
IMPL_CLONE_REMAP(myPrototype)
IMPL_CLONE_REMAP_END

SymRes Symbol::resolveSymbols(Context& ctx)
{
    myPrototype->resolveVariables(ctx);
    return myPrototype->resolveSymbols(ctx);
}

lexer::Token const& Symbol::token() const
{
    return myToken;
}

lexer::Token& Symbol::token()
{
    return myToken;
}

PatternsPrototype const& Symbol::prototype() const
{
    return *myPrototype;
}

PatternsPrototype& Symbol::prototype()
{
    return *myPrototype;
}

Symbol const* Symbol::prototypeParent() const
{
    return myPrototypeParent;
}

//
// SymbolReference

SymbolReference::SymbolReference(stringv name, ConstPatternSlice pattern)
    : myName(name)
    , myPattern(pattern)
{
}

SymbolReference::SymbolReference(Symbol const& sym)
    : SymbolReference(sym.token().lexeme(), sym.prototype().pattern())
{
}

SymbolReference::SymbolReference(stringv name)
    : SymbolReference(name, PatternSlice())
{
}

SymbolReference::~SymbolReference() = default;

void SymbolReference::swap(SymbolReference& rhs)
{
    using kyfoo::swap;
    swap(myName, rhs.myName);
    swap(myPattern, rhs.myPattern);
}

stringv SymbolReference::name() const
{
    return myName;
}

ConstPatternSlice const& SymbolReference::pattern() const
{
    return myPattern;
}

} // namespace kyfoo::ast
