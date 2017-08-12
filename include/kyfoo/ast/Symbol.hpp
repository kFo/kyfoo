#pragma once

#include <map>
#include <memory>
#include <ostream>
#include <string>
#include <vector>

#include <kyfoo/FlatMap.hpp>
#include <kyfoo/Slice.hpp>
#include <kyfoo/lexer/Token.hpp>
#include <kyfoo/ast/IO.hpp>
#include <kyfoo/ast/Node.hpp>

namespace kyfoo {

    class Diagnostics;

    namespace ast {

class Context;
class Declaration;
class DeclarationScope;
class IResolver;
class Expression;
class SymbolExpression;
class SymbolDeclaration;
class SymbolVariable;

using binding_set_t = FlatMap<SymbolVariable const*, Expression const*>;
using Parameters = std::vector<std::unique_ptr<Expression>>;

class ParametersPrototype : public IIO
{
public:
    friend class ParametersInstance;

public:
    ParametersPrototype();
    ParametersPrototype(std::vector<std::unique_ptr<Expression>>&& parameters);

protected:
    ParametersPrototype(ParametersPrototype const& rhs);
    ParametersPrototype& operator = (ParametersPrototype const& rhs);

public:
    ParametersPrototype(ParametersPrototype&& rhs);
    ParametersPrototype& operator = (ParametersPrototype&& rhs);

    ~ParametersPrototype();

    void swap(ParametersPrototype& rhs);

    // IIO
public:
    void io(IStream& stream) const override;

public:
    DECL_CLONE_ALL_NOBASE(ParametersPrototype)

public:
    void resolveSymbols(Diagnostics& dgn, IResolver& resolver);

public:
    Slice<Expression*> parameters() const;
    Slice<SymbolVariable*> symbolVariables() const;

public:
    void bindVariables(Context& ctx, binding_set_t const& bindings);
    SymbolVariable* findVariable(std::string const& identifier);
    SymbolVariable const* findVariable(std::string const& identifier) const;
    SymbolVariable* createVariable(lexer::Token const& identifier);
    bool isConcrete() const;
    bool hasFreeVariables() const;

private:
    Parameters myParameters;
    std::vector<std::unique_ptr<SymbolVariable>> myVariables;
};

class Symbol : public IIO
{
public:
    friend class SymbolSpace;

public:
    Symbol(lexer::Token const& identifier,
           ParametersPrototype&& params);
    explicit Symbol(lexer::Token const& identifier);

protected:
    Symbol(Symbol const& rhs);
    Symbol& operator = (Symbol const& rhs);

public:
    Symbol(Symbol&& rhs);
    Symbol& operator = (Symbol&& rhs);

    ~Symbol();

    void swap(Symbol& rhs);

public:
    void io(IStream& stream) const override;

public:
    DECL_CLONE_ALL_NOBASE(Symbol);

public:
    void resolveSymbols(Diagnostics& dgn, IResolver& resolver);

public:
    lexer::Token const& identifier() const;
    ParametersPrototype const& prototype() const;
    Symbol const* prototypeParent() const;

private:
    lexer::Token myIdentifier;
    std::unique_ptr<ParametersPrototype> myPrototype;
    Symbol const* myPrototypeParent = nullptr;
};

class SymbolReference
{
public:
    using param_list_t = Slice<Expression*>;

public:
    SymbolReference(std::string const& name, param_list_t parameters);
    /*implicit*/ SymbolReference(Symbol const& sym);
    /*implicit*/ SymbolReference(std::string const& name);
    /*implicit*/ SymbolReference(const char* name);
    ~SymbolReference();

public:
    std::string const& name() const;
    param_list_t const& parameters() const;

private:
    std::string const* myName;
    param_list_t myParameters;
};

class SymbolSpace
{
public:
    using param_list_t = SymbolReference::param_list_t;

    struct ParametersDecl {
        ParametersPrototype const* params;
        Declaration const* decl;
    };

    struct Prototype {
        ParametersDecl proto;
        std::vector<ParametersDecl> instances;
    };

    struct DeclInstance {
        Declaration const* parent;
        Declaration const* instance;
    };

public:
    SymbolSpace(DeclarationScope* scope, std::string const& name);

    SymbolSpace(SymbolSpace const& rhs) = delete;
    SymbolSpace& operator = (SymbolSpace const& rhs) = delete;

    SymbolSpace(SymbolSpace&& rhs);
    SymbolSpace& operator = (SymbolSpace&& rhs);

    ~SymbolSpace();

    void swap(SymbolSpace& rhs);

public:
    std::string const& name() const;
    Slice<Prototype> prototypes() const;

    void append(Context& ctx,
                ParametersPrototype const& prototype,
                Declaration& declaration);

    Declaration const* findEquivalent(Diagnostics& dgn, param_list_t const& paramlist) const;
    DeclInstance findValue(Diagnostics& dgn, param_list_t const& paramlist);

private:
    DeclInstance instantiate(Context& ctx,
                             Prototype& proto,
                             binding_set_t&& bindingSet);

private:
    DeclarationScope* myScope = nullptr;
    std::string myName;
    std::vector<Prototype> myPrototypes;
};

std::ostream& print(std::ostream& stream, Symbol const& sym);

    } // namesapce ast
} // namespace kyfoo
