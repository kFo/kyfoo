#pragma once

#include <map>

#include <kyfoo/Array.hpp>
#include <kyfoo/Slice.hpp>
#include <kyfoo/String.hpp>
#include <kyfoo/Types.hpp>
#include <kyfoo/lexer/Token.hpp>
#include <kyfoo/ast/Clone.hpp>
#include <kyfoo/ast/Substitutions.hpp>

namespace kyfoo {

    class Diagnostics;

    namespace ast {

class Binder;
class Context;
class Declaration;
class Expression;
class IdentifierExpression;
class Module;
class LiteralExpression;
class OverloadViability;
class Resolver;
class Scope;
class SymbolExpression;
class SymbolDeclaration;
class SymbolVariable;
class SymRes;
class Variance;
class ViableSet;

using Pattern = ab<Box<Expression>>;
using PatternSlice = Slice<Expression*>;
using ConstPatternSlice = Slice<Expression const*>;

class PatternsPrototype
{
public:
    friend class ProcedureDeclaration;
    friend class PatternsPrototypeBinderHandler;

public:
    PatternsPrototype();
    PatternsPrototype(ab<Box<Expression>> pattern);

protected:
    PatternsPrototype(PatternsPrototype const& rhs);
    PatternsPrototype& operator = (PatternsPrototype const& rhs);

public:
    PatternsPrototype(PatternsPrototype&& rhs);
    PatternsPrototype& operator = (PatternsPrototype&& rhs);

    ~PatternsPrototype();

    void swap(PatternsPrototype& rhs) noexcept;

public:
    DECL_CLONE_ALL_NOBASE(PatternsPrototype)

public:
    SymRes resolveVariables(Context& ctx);
    SymRes resolveSymbols(Context& ctx);

public:
    Slice<Expression*> pattern();
    Slice<Expression const*> pattern() const;

    Slice<SymbolVariable const*> symbolVariables() const;

public:
    void bindVariables(Substitutions const& substs);
    SymbolVariable* findVariable(stringv token);
    SymbolVariable const* findVariable(stringv token) const;
    bool isConcrete() const;
    uz metaVariableCount() const;

private:
    Pattern myPattern;
    ab<Box<SymbolVariable>> myVariables;
};

class Symbol
{
public:
    friend class SymbolSpace;

public:
    Symbol(lexer::Token token,
           PatternsPrototype params);
    explicit Symbol(lexer::Token token);
    Symbol(Box<SymbolExpression> symExpr);

protected:
    Symbol(Symbol const& rhs);
    Symbol& operator = (Symbol const& rhs);

public:
    Symbol(Symbol&& rhs);
    Symbol& operator = (Symbol&& rhs);

    ~Symbol();

    void swap(Symbol& rhs) noexcept;

public:
    DECL_CLONE_ALL_NOBASE(Symbol);

public:
    SymRes resolveSymbols(Context& ctx);

public:
    lexer::Token const& token() const;
    lexer::Token& token();

    PatternsPrototype const& prototype() const;
    PatternsPrototype& prototype();

    Symbol const* prototypeParent() const;

private:
    lexer::Token myToken;
    Box<PatternsPrototype> myPrototype;
    Symbol const* myPrototypeParent = nullptr;
};

class SymbolReference
{
public:
    SymbolReference(stringv name, ConstPatternSlice pattern);
    /*implicit*/ SymbolReference(Symbol const& sym);
    /*implicit*/ SymbolReference(stringv name);

    template <uz N>
    constexpr /*implicit*/ SymbolReference(char const (&name)[N]) noexcept
        : myName(name)
    {
    }

    SymbolReference(SymbolReference const&) noexcept = default;
    SymbolReference& operator = (SymbolReference const&) noexcept = default;

    SymbolReference(SymbolReference&&) noexcept = default;
    SymbolReference& operator = (SymbolReference&&) noexcept = default;

    ~SymbolReference();

    void swap(SymbolReference& rhs);

public:
    stringv name() const;
    ConstPatternSlice const& pattern() const;

private:
    stringv myName;
    ConstPatternSlice myPattern;
};

inline Box<PatternsPrototype> beginClone(PatternsPrototype const& proto, CloneMap& map)
{
    return proto.beginClone(map);
}

inline void remap(PatternsPrototype& proto, CloneMap const& map)
{
    return proto.remapReferences(map);
}

inline Box<Symbol> beginClone(Symbol const& sym, CloneMap& map)
{
    return sym.beginClone(map);
}

inline void remap(Symbol& sym, CloneMap const& map)
{
    return sym.remapReferences(map);
}

    } // namespace ast

    namespace ascii {
        template <typename Sink>
        void write(Sink& stream, ast::Symbol const& sym)
        {
            write(stream, sym.token().lexeme());
            if ( sym.prototype().pattern() ) {
                write(stream, "<");
                auto first = begin(sym.prototype().pattern());
                auto last = end(sym.prototype().pattern());
                if ( first != last )
                    write(stream, **first);

                for ( ++first; first != last; ++first ) {
                    write(stream, ", ");
                    write(stream, **first);
                }

                write(stream, ">");
            }
        }
    }

} // namespace kyfoo
