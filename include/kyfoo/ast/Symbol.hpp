#pragma once

#include <map>
#include <ostream>
#include <string>
#include <vector>

#include <kyfoo/Slice.hpp>
#include <kyfoo/Types.hpp>
#include <kyfoo/lexer/Token.hpp>
#include <kyfoo/ast/IO.hpp>
#include <kyfoo/ast/Node.hpp>
#include <kyfoo/ast/Substitutions.hpp>

namespace kyfoo {

    class Diagnostics;

    namespace ast {

class Context;
class Declaration;
class Scope;
class Resolver;
class Expression;
class Module;
class LiteralExpression;
class OverloadViability;
class SymbolExpression;
class SymbolDeclaration;
class SymbolVariable;
class SymRes;
class Variance;
class ViableSet;

using Pattern = std::vector<Box<Expression>>;
using pattern_t = Slice<Expression*>;
using const_pattern_t = Slice<Expression const*>;

class PatternsPrototype : public IIO
{
public:
    friend class ProcedureDeclaration;

public:
    PatternsPrototype();
    PatternsPrototype(std::vector<Box<Expression>>&& pattern);

protected:
    PatternsPrototype(PatternsPrototype const& rhs);
    PatternsPrototype& operator = (PatternsPrototype const& rhs);

public:
    PatternsPrototype(PatternsPrototype&& rhs);
    PatternsPrototype& operator = (PatternsPrototype&& rhs);

    ~PatternsPrototype();

    void swap(PatternsPrototype& rhs) noexcept;

    // IIO
public:
    void io(IStream& stream) const override;

public:
    DECL_CLONE_ALL_NOBASE(PatternsPrototype)

public:
    void resolveVariables(Scope const& scope);
    SymRes resolveSymbols(Context& ctx);

public:
    Slice<Expression*> pattern();
    Slice<Expression const*> pattern() const;

    Slice<SymbolVariable const*> symbolVariables() const;

public:
    void bindVariables(Substitutions const& substs);
    SymbolVariable* findVariable(std::string_view token);
    SymbolVariable const* findVariable(std::string_view token) const;
    bool isConcrete() const;
    uz metaVariableCount() const;

private:
    Pattern myPattern;
    std::vector<Box<SymbolVariable>> myVariables;
};

class Symbol : public IIO
{
public:
    friend class SymbolSpace;

public:
    Symbol(lexer::Token const& token,
           PatternsPrototype&& params);
    explicit Symbol(lexer::Token const& token);
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
    void io(IStream& stream) const override;

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
    SymbolReference(std::string_view name, const_pattern_t pattern);
    SymbolReference(const char* name, const_pattern_t pattern);
    /*implicit*/ SymbolReference(Symbol const& sym);
    /*implicit*/ SymbolReference(std::string_view name);
    /*implicit*/ SymbolReference(const char* name);
    ~SymbolReference();

    void swap(SymbolReference& rhs);

public:
    std::string_view name() const;
    const_pattern_t const& pattern() const;

private:
    std::string_view myName;
    const_pattern_t myPattern;
};

std::ostream& print(std::ostream& stream, Symbol const& sym);

    } // namesapce ast
} // namespace kyfoo
