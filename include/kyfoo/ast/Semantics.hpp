#pragma once

#include <algorithm>
#include <vector>
#include <map>
#include <set>

#include <kyfoo/Diagnostics.hpp>
#include <kyfoo/Slice.hpp>
#include <kyfoo/ast/ControlFlow.hpp>
#include <kyfoo/ast/Declarations.hpp>
#include <kyfoo/ast/Expressions.hpp>

namespace kyfoo::lexer {
    class Token;
}

namespace kyfoo::ast {

class Module;

struct SymbolDependencyTracker
{
    struct SymGroup {
        std::string name;
        uz arity;
        int level = 0;
        std::vector<Declaration*> declarations;
        std::vector<SymGroup*> dependents;
        bool upstreamErrors = false;

        SymGroup(std::string name, uz arity)
            : name(std::move(name))
            , arity(arity)
        {
        }

        bool operator < (SymGroup const& rhs) const
        {
            return std::tie(name, arity) < std::tie(rhs.name, rhs.arity);
        }

        bool operator == (SymGroup const& rhs) const
        {
            return std::tie(name, arity) == std::tie(rhs.name, rhs.arity);
        }

        void add(Declaration& decl)
        {
            declarations.push_back(&decl);
        }

        void addDependent(SymGroup& group)
        {
            dependents.push_back(&group);
        }

        bool defer(SymGroup* startSym, int deferPass)
        {
            if ( level >= deferPass )
                return true;

            level = deferPass;
            for ( auto& d : dependents ) {
                if ( d == startSym )
                    return false;

                d->defer(startSym, level + 1);
            }

            return true;
        }
    };

    Module& mod;
    Diagnostics& dgn;
    std::vector<Box<SymGroup>> groups;

    SymbolDependencyTracker(Module& mod, Diagnostics& dgn);

    SymGroup* create(std::string name, uz arity);
    SymGroup* findOrCreate(std::string_view name, uz arity);

    void add(Declaration& decl);
    SymRes addDependency(Declaration& dependent,
                         std::string_view name,
                         uz arity);

    void sortPasses();
};

SymRes traceDependencies(SymbolDependencyTracker& tracker, Declaration& decl);

Expression const* lookThrough(Expression const* expr);
Expression const* lookThrough(Declaration const* decl);
Declaration const* resolveIndirections(Declaration const* decl);
Declaration const* resolveIndirections(Declaration const& decl);
Expression const* resolveIndirections(Expression const* expr);
Expression* resolveIndirections(Expression* expr);
Expression const* resolveIndirections(Expression const& expr);
bool needsSubstitution(Expression const& expr);
bool needsSubstitution(Declaration const& decl);
bool needsSubstitutions(Symbol const& sym);
bool requiresSubstitutions(Symbol const& sym);
bool hasSubstitutions(Symbol const& sym);

Symbol const* rootTemplate(Symbol const& symbol);
bool descendsFromTemplate(Symbol const& parent, Symbol const& instance);

bool isReference(Declaration const& decl);
bool isReference(Expression const& expr);
Expression const& removeReference(Declaration const& decl);
Declaration const* removeAllReferences(Declaration const& decl);

Scope const* staticAccessorScope(Expression const& expr);
Scope const* staticAccessorScope(Declaration const& decl);

struct AccessorScope
{
    Expression const* instance;
    Scope const* scope;
};

AccessorScope instanceAccessorScope(Expression const& expr);

TemplateDeclaration const* procTemplate(ProcedureDeclaration const& proc);
Declaration const* outerDataDeclaration(Declaration const& decl);
Declaration* outerDataDeclaration(Declaration& decl);
Declaration const* callingContextDeclaration(Declaration const& decl);
Declaration* callingContextDeclaration(Declaration& decl);
DataProductDeclaration const* methodType(ProcedureDeclaration const& proc);
Expression const* dataType(Expression const& expr);

struct UnificationResult 
{
    SymRes resolution;
    Expression const* type;
};
UnificationResult unify(Context& ctx, Declaration const& gov, Slice<Expression const*> exprs);

bool matchEquivalent(Expression const& lhs, Expression const& rhs);
bool matchEquivalent(Slice<Expression const*> lhs, Slice<Expression const*> rhs);

std::vector<IdentifierExpression*> gatherMetaVariables(Expression& expr);
bool hasMetaVariable(Expression const& expr);

lexer::Token const& front(lexer::Token const& tok);
lexer::Token const& front(Expression   const& expr);
lexer::Token const& front(Statement    const& stmt);
lexer::Token const& front(Junction     const& junc);
lexer::Token const& front(Declaration  const& decl);

std::ostream& print(std::ostream& stream, Expression   const& expr);
std::ostream& print(std::ostream& stream, Statement    const& stmt);
std::ostream& print(std::ostream& stream, Junction     const& junc);

uz level(Expression const& expr);

} // namespace kyfoo::ast
