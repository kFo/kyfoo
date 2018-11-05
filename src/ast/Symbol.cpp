#include <kyfoo/ast/Symbol.hpp>

#include <tuple>

#include <kyfoo/Diagnostics.hpp>
#include <kyfoo/Utilities.hpp>
#include <kyfoo/ast/Declarations.hpp>
#include <kyfoo/ast/Expressions.hpp>
#include <kyfoo/ast/Context.hpp>
#include <kyfoo/ast/Module.hpp>
#include <kyfoo/ast/Scopes.hpp>
#include <kyfoo/ast/Semantics.hpp>
#include <kyfoo/ast/Substitutions.hpp>

namespace kyfoo::ast {

//
// PatternsPrototype

PatternsPrototype::PatternsPrototype() = default;

PatternsPrototype::PatternsPrototype(std::vector<Box<Expression>>&& pattern)
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

void PatternsPrototype::resolveVariables(Scope const& scope)
{
    if ( myVariables.empty() ) {
        for ( auto const& param : myPattern ) {
            auto fv = gatherMetaVariables(*param);
            for ( auto& p : fv ) {
                myVariables.emplace_back(mk<SymbolVariable>(p->token(), const_cast<Scope&>(scope)));
                p->setDeclaration(*myVariables.back());
                for ( auto c : p->constraints() )
                    myVariables.back()->appendConstraint(*c);
            }
        }
    }
}

SymRes PatternsPrototype::resolveSymbols(Context& ctx)
{
    Resolver resolver(ctx.resolver().scope());
    resolver.addSupplementaryPrototype(*this);
    REVERT = ctx.pushResolver(resolver);
    auto ret = ctx.resolveExpressions(myPattern);

    return ret;
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
    if ( substs.card() != myVariables.size() )
        throw std::runtime_error("template parameter binding mismatch");

    for ( uz i = 0; i < substs.card(); ++i ) {
        auto const& key = substs.var(i);
        auto const& value = substs.expr(i);
        auto var = findVariable(key.symbol().token().lexeme());
        if ( !var )
            throw std::runtime_error("template parameter binding mismatch");

        var->bindExpression(&value);
    }
}

SymbolVariable* PatternsPrototype::findVariable(stringv token)
{
    for ( auto& e : myVariables )
        if ( e->token().lexeme() == token )
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
    return myVariables.size();
}

//
// Symbol

Symbol::Symbol(lexer::Token token,
               PatternsPrototype&& params)
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
    myPrototype->resolveVariables(ctx.resolver().scope());
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

SymbolReference::SymbolReference(stringv name, const_pattern_t pattern)
    : myName(name)
    , myPattern(pattern)
{
}

SymbolReference::SymbolReference(Symbol const& sym)
    : SymbolReference(sym.token().lexeme(), sym.prototype().pattern())
{
}

SymbolReference::SymbolReference(stringv name)
    : SymbolReference(name, pattern_t())
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

const_pattern_t const& SymbolReference::pattern() const
{
    return myPattern;
}

//
// misc

std::ostream& print(std::ostream& stream, Symbol const& sym)
{
    stream << sym.token().lexeme();
    if ( sym.prototype().pattern() ) {
        stream << "<";
        auto first = begin(sym.prototype().pattern());
        auto last = end(sym.prototype().pattern());
        if ( first != last )
            print(stream, **first);

        for ( ++first; first != last; ++first ) {
            stream << ", ";
            print(stream, **first);
        }

        stream << ">";
    }

    return stream;
}

} // namespace kyfoo::ast
