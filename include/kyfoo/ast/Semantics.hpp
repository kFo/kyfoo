#pragma once

#include <algorithm>
#include <map>
#include <set>

#include <kyfoo/Array.hpp>
#include <kyfoo/Diagnostics.hpp>
#include <kyfoo/Slice.hpp>
#include <kyfoo/Stream.hpp>
#include <kyfoo/ast/ControlFlow.hpp>
#include <kyfoo/ast/Declarations.hpp>
#include <kyfoo/ast/Expressions.hpp>
#include <kyfoo/ast/Visitors.hpp>

namespace kyfoo {
    namespace lexer {
        class Token;
    }

    namespace ast {

class Module;

struct SymbolDependencyTracker
{
    struct SymGroup {
        std::string name;
        uz arity;
        int level = 0;
        ab<Declaration*> declarations;
        ab<SymGroup*> dependents;
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
            declarations.append(&decl);
        }

        void addDependent(SymGroup& group)
        {
            dependents.append(&group);
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
    ab<Box<SymGroup>> groups;

    SymbolDependencyTracker(Module& mod, Diagnostics& dgn);

    SymGroup* create(std::string name, uz arity);
    SymGroup* findOrCreate(stringv name, uz arity);

    void add(Declaration& decl);
    SymRes addDependency(Declaration& dependent,
                         stringv name,
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

Expression const* refType(Declaration const& decl);
Expression const* refType(Expression const& expr);
Expression const& removeReference(Declaration const& decl);
Declaration const* removeAllReferences(Declaration const& decl);
Expression const* removeAllReferences(Expression const& expr);

Scope const* staticAccessorScope(Expression const& expr);
Scope const* staticAccessorScope(Declaration const& decl);

struct AccessorScope
{
    Expression const* instance;
    Scope const* scope;
};

AccessorScope instanceAccessorScope(Expression const& expr);

TemplateDeclaration const* procTemplate(ProcedureDeclaration const& proc);
DataTypeDeclaration const* outerDataDeclaration(Declaration const& decl);
DataTypeDeclaration* outerDataDeclaration(Declaration& decl);
Declaration const* callingContextDeclaration(Declaration const& decl);
Declaration* callingContextDeclaration(Declaration& decl);
DataTypeDeclaration const* methodType(ProcedureDeclaration const& proc);
Expression const* dataType(Expression const& expr);
uz variationOrdinal(DataTypeDeclaration const& dt);

struct UnificationResult 
{
    SymRes resolution;
    Expression const* type;
};
UnificationResult unify(Context& ctx, Report::Subject gov, Slice<Expression const*> exprs);

bool matchEquivalent(Expression const& lhs, Expression const& rhs);
bool matchEquivalent(Slice<Expression const*> lhs, Slice<Expression const*> rhs);

ab<IdentifierExpression*> gatherMetaVariables(Expression& expr);
bool hasMetaVariable(Expression const& expr);

lexer::Token const& front(lexer::Token const& tok);
lexer::Token const& front(Expression   const& expr);
lexer::Token const& front(Statement    const& stmt);
lexer::Token const& front(Junction     const& junc);
lexer::Token const& front(Declaration  const& decl);

uz level(Expression const& expr);

    } // namespace ast

    namespace ascii {
        void write(DefaultOutStream& sink, ast::Expression const& expr);
        void write(DefaultOutStream& sink, ast::Statement  const& stmt);
        void write(DefaultOutStream& sink, ast::Junction   const& junc);
    }

} // namespace kyfoo
