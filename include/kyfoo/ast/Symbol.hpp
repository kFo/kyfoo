#pragma once

#include <map>
#include <memory>
#include <ostream>
#include <string>
#include <vector>

#include <kyfoo/Slice.hpp>
#include <kyfoo/lexer/Token.hpp>
#include <kyfoo/ast/IO.hpp>
#include <kyfoo/ast/Node.hpp>

namespace kyfoo {

    class Diagnostics;

    namespace ast {

class Context;
class DeclarationScope;
class IResolver;
class Expression;
class SymbolExpression;
class SymbolDeclaration;
class SymbolVariable;

using binding_set_t = std::map<SymbolVariable const*, Expression const*>;

class Symbol : public IIO
{
public:
    using paramlist_t = std::vector<std::unique_ptr<Expression>>;

public:
    explicit Symbol(std::string const& name);
    Symbol(lexer::Token const& identifier,
           std::vector<std::unique_ptr<Expression>>&& parameters);
    explicit Symbol(lexer::Token const& identifier);

protected:
    Symbol(Symbol const& rhs);
    Symbol& operator = (Symbol const& rhs);

public:
    Symbol(Symbol&& rhs);
    Symbol& operator = (Symbol&& rhs);

    ~Symbol();

    void swap(Symbol& rhs);

    // IIO
public:
    void io(IStream& stream) const override;

public:
    Symbol* clone(clone_map_t& map) const;
    void cloneChildren(Symbol& c, clone_map_t& map) const;
    void remapReferences(clone_map_t const& map);

public:
    void resolveSymbols(Diagnostics& dgn, IResolver& resolver);
    void bindVariables(Context& ctx,
                       Symbol& parentTemplate,
                       binding_set_t const& bindings);
    SymbolVariable* findVariable(std::string const& identifier);
    SymbolVariable const* findVariable(std::string const& identifier) const;
    SymbolVariable* createVariable(std::string const& identifier);

public:
    lexer::Token const& identifier() const;
    std::string const& name() const;
    paramlist_t const& parameters() const;
    Slice<SymbolVariable*> symbolVariables() const;
    Symbol const* parentTemplate() const;
    bool isConcrete() const;
    bool hasFreeVariables() const;

private:
    lexer::Token myIdentifier;
    paramlist_t myParameters;
    std::vector<std::unique_ptr<SymbolVariable>> myVariables;
    Symbol* myParentTemplate = nullptr;
};

class SymbolReference
{
public:
    using paramlist_t = Slice<Expression*>;

public:
    /*implicit*/ SymbolReference(Symbol const& symbol);
    /*implicit*/ SymbolReference(std::string const& name);
    SymbolReference(std::string const& name, paramlist_t parameters);
    ~SymbolReference();

public:
    std::string const& name() const;
    paramlist_t const& parameters() const;

private:
    std::string const* myName;
    paramlist_t myParameters;
};

class Declaration;
class SymbolSet
{
public:
    using paramlist_t = SymbolReference::paramlist_t;

    struct SymbolTemplate {
        std::vector<Expression*> paramlist;
        Declaration* declaration;
        std::vector<binding_set_t> instanceBindings;
        std::vector<Declaration*> instantiations;
    };

public:
    SymbolSet(DeclarationScope* scope, std::string const& name);

    SymbolSet(SymbolSet const& rhs);
    SymbolSet& operator = (SymbolSet const& rhs);

    SymbolSet(SymbolSet&& rhs);
    SymbolSet& operator = (SymbolSet&& rhs);

    ~SymbolSet();

    void swap(SymbolSet& rhs);

public:
    bool operator < (std::string const& rhs) const { return myName < rhs; }
    bool operator == (std::string const& rhs) const { return myName == rhs; }

public:
    std::string const& name() const;
    Slice<SymbolTemplate> const prototypes() const;

    void append(paramlist_t const& paramlist, Declaration& declaration);

    Declaration* findEquivalent(Diagnostics& dgn, paramlist_t const& paramlist);
    Declaration const* findEquivalent(Diagnostics& dgn, paramlist_t const& paramlist) const;

    struct TemplateInstance {
        Declaration const* parent;
        Declaration const* instance;
    };

    TemplateInstance findValue(Diagnostics& dgn, paramlist_t const& paramlist);
    TemplateInstance const findValue(Diagnostics& dgn, paramlist_t const& paramlist) const;

private:
    TemplateInstance instantiate(Context& ctx,
                                 SymbolTemplate& proto,
                                 binding_set_t&& bindingSet);

private:
    DeclarationScope* myScope = nullptr;
    std::string myName;
    std::vector<SymbolTemplate> mySet;
};

std::ostream& print(std::ostream& stream, Symbol const& sym);

    } // namesapce ast
} // namespace kyfoo
