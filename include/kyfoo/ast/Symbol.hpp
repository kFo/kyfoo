#pragma once

#include <memory>
#include <string>
#include <vector>

#include <kyfoo/lexer/Token.hpp>
#include <kyfoo/ast/IO.hpp>

namespace kyfoo {
    namespace ast {

class TypeExpression;
class ValueExpression;

class Expression
{
public:
    enum class Kind
    {
        TypeExpression,
        ValueExpression,
    };

    static const char* to_string(Kind kind);

public:
    /*implciit*/ Expression(std::unique_ptr<TypeExpression> typeExpression);
    /*implicit*/ Expression(std::unique_ptr<ValueExpression> valueExpression);

    Expression(Expression const&) = delete;

    Expression(Expression&& rhs);
    Expression& operator = (Expression&& rhs);

    ~Expression();

public:
    Kind kind() const;
    TypeExpression* typeExpression();
    ValueExpression* valueExpression();

private:
    Kind myKind;
    union Ptr {
        Ptr() : any(nullptr) {}
        Ptr(void* rhs) : any(rhs) {}
        Ptr(TypeExpression* rhs) : asTypeExpression(rhs) {}
        Ptr(ValueExpression* rhs) : asExpression(rhs) {}

        void* any;
        TypeExpression* asTypeExpression;
        ValueExpression* asExpression;
    } myPtr;
};

struct ConstrainedExpression
{
    Expression expression;
    std::unique_ptr<TypeExpression> constraint;
};

class SymbolParameter
{
public:
    enum class Kind
    {
        TypeExpression,
        ValueExpression,
        ConstrainedExpression,
    };

public:
    explicit SymbolParameter(std::unique_ptr<TypeExpression> typeExpression);
    explicit SymbolParameter(std::unique_ptr<ValueExpression> valueExpression);
    SymbolParameter(Expression expression,
                    std::unique_ptr<TypeExpression> typeConstraint);

    SymbolParameter(SymbolParameter const&) = delete;

    SymbolParameter(SymbolParameter&& rhs);
    SymbolParameter& operator = (SymbolParameter&& rhs);

    ~SymbolParameter();

public:
    Kind kind() const;
    TypeExpression const* typeExpression() const;
    ValueExpression const* valueExpression() const;
    ConstrainedExpression const* constrainedExpression() const;

private:
    Kind myKind;
    union Ptr {
        Ptr() : any(nullptr) {}
        Ptr(void* rhs) : any(rhs) {}
        Ptr(TypeExpression* rhs) : asTypeExpression(rhs) {}
        Ptr(ValueExpression* rhs) : asValueExpression(rhs) {}
        Ptr(ConstrainedExpression* rhs) : asConstrainedExpression(rhs) {}

        void* any;
        TypeExpression* asTypeExpression;
        ValueExpression* asValueExpression;
        ConstrainedExpression* asConstrainedExpression;
    } myPtr;
};

class Symbol : public IIO
{
public:
    using paramlist_t = std::vector<SymbolParameter>;

public:
    explicit Symbol(lexer::Token const& identifier,
                    std::vector<SymbolParameter>&& parameters);
    explicit Symbol(lexer::Token const& identifier);

    Symbol(Symbol const&) = delete;

    Symbol(Symbol&& rhs);
    Symbol& operator = (Symbol&& rhs);

    ~Symbol();

    // IIO
public:
    void io(IStream& stream);

public:
    lexer::Token const& identifier() const;
    std::string const& name() const;
    paramlist_t const& parameters() const;

private:
    lexer::Token myIdentifier;
    paramlist_t myParameters;
};

class Declaration;
class SymbolSet
{
public:
    using paramlist_t = Symbol::paramlist_t;

    struct pair_t {
        paramlist_t const* paramlist;
        Declaration* declaration;
    };

public:
    explicit SymbolSet(std::string const& name);

    SymbolSet(SymbolSet const&) = delete;

    SymbolSet(SymbolSet&& rhs);
    SymbolSet& operator = (SymbolSet&& rhs);

    ~SymbolSet();

public:
    bool operator < (std::string const& rhs) const { return myName < rhs; }
    bool operator == (std::string const& rhs) const { return myName == rhs; }

public:
    std::string const& name() const;

    void append(paramlist_t const& paramlist, Declaration& declaration);
    Declaration* find(paramlist_t const& paramlist);

private:
    std::string myName;
    std::vector<pair_t> mySet;
};

    } // namesapce ast
} // namespace kyfoo
