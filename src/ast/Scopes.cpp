#include <kyfoo/ast/Scopes.hpp>

#include <cassert>

#include <kyfoo/Diagnostics.hpp>

#include <kyfoo/ast/Axioms.hpp>
#include <kyfoo/ast/Declarations.hpp>
#include <kyfoo/ast/Expressions.hpp>
#include <kyfoo/ast/Fabrication.hpp>
#include <kyfoo/ast/Module.hpp>
#include <kyfoo/ast/Semantics.hpp>
#include <kyfoo/ast/Context.hpp>

namespace kyfoo {
namespace ast {

//
// DeclarationScope

DeclarationScope::DeclarationScope(Kind kind,
                                   Module* module,
                                   DeclarationScope* parent,
                                   Declaration* decl)
    : myKind(kind)
    , myModule(module)
    , myParent(parent)
    , myDeclaration(decl)
{
    mySymbols.emplace_back(this, "");
}

DeclarationScope::DeclarationScope(Module& module)
    : DeclarationScope(Kind::Declaration, &module, nullptr, nullptr)
{
}

DeclarationScope::DeclarationScope(DeclarationScope* parent)
    : DeclarationScope(Kind::Declaration, &parent->module(), parent, nullptr)
{
}

DeclarationScope::DeclarationScope(DeclarationScope& parent, Declaration& decl)
    : DeclarationScope(Kind::Declaration, &parent.module(), &parent, &decl)
{
}

DeclarationScope::DeclarationScope(DeclarationScope const& rhs)
    : myKind(rhs.myKind)
    , myModule(rhs.myModule)
    , myDeclaration(rhs.myDeclaration)
    , myParent(rhs.myParent)
{
    // mySymbols, myProcedures, and myImports are to be filled out
    // by the semantic passes that build these indices
}

DeclarationScope& DeclarationScope::operator = (DeclarationScope const& rhs)
{
    DeclarationScope(rhs).swap(*this);
    return *this;
}

DeclarationScope::~DeclarationScope() = default;

void DeclarationScope::swap(DeclarationScope& rhs)
{
    using std::swap;
    swap(myKind, rhs.myKind);
    swap(myModule, rhs.myModule);
    swap(myDeclaration, rhs.myDeclaration);
    swap(myParent, rhs.myParent);
    swap(myDeclarations, rhs.myDeclarations);
    swap(mySymbols, rhs.mySymbols);
    swap(myImports, rhs.myImports);
}

void DeclarationScope::io(IStream& stream) const
{
    stream.next("declarations", myDeclarations);
}

IMPL_CLONE_NOBASE_BEGIN(DeclarationScope, DeclarationScope)
IMPL_CLONE_CHILD(myDeclarations)
IMPL_CLONE_END
IMPL_CLONE_REMAP_NOBASE_BEGIN(DeclarationScope)
IMPL_CLONE_REMAP(myModule)
IMPL_CLONE_REMAP(myDeclaration)
IMPL_CLONE_REMAP(myParent)
IMPL_CLONE_REMAP(myDeclarations)
IMPL_CLONE_REMAP_END

void DeclarationScope::resolveImports(Diagnostics& dgn)
{
    for ( auto& e : myDeclarations ) {
        if ( auto d = e->as<ImportDeclaration>() ) {
            module().import(dgn, d->identifier());
        }
    }
}

void DeclarationScope::resolveSymbols(Diagnostics& dgn)
{
    SymbolDependencyTracker tracker(module(), dgn);
    for ( auto const& d : myDeclarations )
        traceDependencies(tracker, *d);

    if ( dgn.errorCount() )
        return;

    tracker.sortPasses();

    ScopeResolver resolver(*this);

    // Resolve top-level declarations
    for ( auto const& symGroup : tracker.groups ) {
        for ( auto const& d : symGroup->declarations ) {
            if ( auto proc = d->as<ProcedureDeclaration>() )
                proc->resolvePrototypeSymbols(dgn);
            else
                d->symbol().resolveSymbols(dgn, resolver);

            if ( !addSymbol(dgn, d->symbol(), *d) )
                continue;

            if ( isMacroDeclaration(d->kind()) )
                d->resolveSymbols(dgn);
        }
    }

    // Resolve definitions
    for ( auto const& symGroup : tracker.groups ) {
        for ( auto const& d : symGroup->declarations ) {
            if ( isMacroDeclaration(d->kind()) )
                continue;

            if ( d->symbol().prototype().isConcrete() ) {
                if ( auto proc = d->as<ProcedureDeclaration>() ) {
                    if ( proc->symbol().prototype().isConcrete() )
                        proc->resolveSymbols(dgn);
                }
                else {
                    d->resolveSymbols(dgn);
                }
            }
        }
    }
}

/**
 * Locates a symbol with parameter list that has exact semantic match
 *
 * Equivalent symbols:
 * \code
 * mytype<n : integer>
 * mytype<m : integer>
 * \endcode
 *
 * Non-equivalent symbols:
 * \code
 * mytype<n : integer>
 * mytype<n : ascii>
 * \endcode
 */
LookupHit DeclarationScope::findEquivalent(Diagnostics& dgn, SymbolReference const& symbol) const
{
    auto symSpace = findSymbolSpace(dgn, symbol.name());
    if ( symSpace )
        return LookupHit(symSpace, symSpace->findEquivalent(dgn, symbol.pattern()));

    if ( myDeclaration && symbol.pattern().empty() )
        return LookupHit(symSpace, myDeclaration->symbol().prototype().findVariable(symbol.name()));

    return LookupHit();
}

/**
 * Locates a symbol with parameter list that could be instantiated by a value expression
 *
 * Overload example:
 * \code
 * mytype<n : integer>
 * mytype<32>
 * \endcode
 *
 * Non-overload example:
 * \code
 * mytype<n : integer>
 * mytype<m : integer>
 * \endcode
 *
 * \code
 * mytype<n : integer>
 * mytype<"str">
 * \endcode
 */
LookupHit DeclarationScope::findOverload(Diagnostics& dgn, SymbolReference const& sym) const
{
    LookupHit hit;
    auto symSpace = findSymbolSpace(dgn, sym.name());
    if ( symSpace ) {
        auto t = symSpace->findOverload(dgn, sym.pattern());
        hit.lookup(symSpace, t.instance ? t.instance : t.parent);
    }

    return hit;
}

void DeclarationScope::setDeclaration(Declaration* declaration)
{
    myDeclaration = declaration;
}

void DeclarationScope::append(std::unique_ptr<Declaration> declaration)
{
    myDeclarations.emplace_back(std::move(declaration));
    myDeclarations.back()->setScope(*this);
}

void DeclarationScope::import(Module& module)
{
    append(std::make_unique<ImportDeclaration>(Symbol(lexer::Token(lexer::TokenKind::Identifier, 0, 0, module.name()))));
}

void DeclarationScope::merge(DeclarationScope& rhs)
{
    myDeclarations.reserve(myDeclarations.size() + rhs.myDeclarations.size());
    for ( auto& e : rhs.myDeclarations ) {
        myDeclarations.emplace_back(std::move(e));
        myDeclarations.back()->setScope(*this);
    }

    rhs.myDeclarations.clear();
}

SymbolSpace* DeclarationScope::createSymbolSpace(Diagnostics&, std::string const& name)
{
    auto symLess = [](SymbolSpace const& s, std::string const& name) { return s.name() < name; };
    auto l = lower_bound(begin(mySymbols), end(mySymbols), name, symLess);
    if ( l != end(mySymbols) && l->name() == name )
        return &*l;

    l = mySymbols.insert(l, SymbolSpace(this, name));
    return &*l;
}

bool DeclarationScope::addSymbol(Diagnostics& dgn,
                                 Symbol const& sym,
                                 Declaration& decl)
{
    auto symSpace = createSymbolSpace(dgn, sym.identifier().lexeme());

    ScopeResolver resolver(*this);
    Context ctx(dgn, resolver);

    if ( auto other = symSpace->findEquivalent(dgn, sym.prototype().pattern()) ) {
        auto templDecl = decl.as<TemplateDeclaration>();
        if ( !templDecl ) {
            auto& err = dgn.error(module(), sym.identifier()) << "symbol is already defined";
            err.see(*other);
            return false;
        }

        auto otherTemplDecl = other->as<TemplateDeclaration>();
        if ( !otherTemplDecl ) {
            auto& err = dgn.error(module(), sym.identifier()) << "symbol was not first defined as a template";
            err.see(*other);
            return false;
        }

        otherTemplDecl->merge(*templDecl);
        return true;
    }

    symSpace->append(ctx, sym.prototype(), decl);
    return true;
}

SymbolSpace* DeclarationScope::findSymbolSpace(Diagnostics&, std::string const& name) const
{
    auto symLess = [](SymbolSpace const& s, std::string const& name) { return s.name() < name; };
    auto symSet = lower_bound(begin(mySymbols), end(mySymbols), name, symLess);
    if ( symSet != end(mySymbols) && symSet->name() == name )
        return &*symSet;

    return nullptr;
}

Module& DeclarationScope::module()
{
    return *myModule;
}

Module const& DeclarationScope::module() const
{
    return *myModule;
}

Declaration* DeclarationScope::declaration()
{
    return myDeclaration;
}

Declaration const* DeclarationScope::declaration() const
{
    return myDeclaration;
}

DeclarationScope* DeclarationScope::parent()
{
    return myParent;
}

DeclarationScope const* DeclarationScope::parent() const
{
    return myParent;
}

Slice<Declaration*> DeclarationScope::childDeclarations() const
{
    return myDeclarations;
}

//
// DataSumScope

DataSumScope::DataSumScope(DeclarationScope& parent,
                           DataSumDeclaration& declaration)
    : DeclarationScope(Kind::DataSum, &parent.module(), &parent, &declaration)
{
}

DataSumScope::DataSumScope(DataSumScope const& rhs)
    : DeclarationScope(rhs)
{
}

DataSumScope& DataSumScope::operator = (DataSumScope const& rhs)
{
    DataSumScope(rhs).swap(*this);
    return *this;
}

DataSumScope::~DataSumScope() = default;

void DataSumScope::swap(DataSumScope& rhs)
{
    DeclarationScope::swap(rhs);
}

void DataSumScope::io(IStream& stream) const
{
    DeclarationScope::io(stream);
}

IMPL_CLONE_BEGIN(DataSumScope, DeclarationScope, DeclarationScope)
IMPL_CLONE_END
IMPL_CLONE_REMAP_BEGIN(DataSumScope, DeclarationScope)
IMPL_CLONE_REMAP_END

void DataSumScope::resolveSymbols(Diagnostics& dgn)
{
    ScopeResolver resolver(*this);
    for ( auto const& d : myDeclarations ) {
        auto dsCtor = d->as<DataSumDeclaration::Constructor>();
        if ( !dsCtor )
            throw std::runtime_error("data sum must only contain constructors");

        dsCtor->symbol().resolveSymbols(dgn, resolver);
        parent()->addSymbol(dgn, d->symbol(), *d);
    }

    for ( auto& e : myDeclarations )
        e->resolveSymbols(dgn);
}

DataSumDeclaration* DataSumScope::declaration()
{
    return static_cast<DataSumDeclaration*>(myDeclaration);
}

//
// DataProductScope

DataProductScope::DataProductScope(DeclarationScope& parent,
                                   DataProductDeclaration& declaration)
    : DeclarationScope(Kind::DataProduct, &parent.module(), &parent, &declaration)
{
}

DataProductScope::DataProductScope(DataProductScope const& rhs)
    : DeclarationScope(rhs)
{
    // myFields is populated by the semantic pass
}

DataProductScope& DataProductScope::operator = (DataProductScope const& rhs)
{
    DataProductScope(rhs).swap(*this);
    return *this;
}

DataProductScope::~DataProductScope() = default;

void DataProductScope::swap(DataProductScope& rhs)
{
    DeclarationScope::swap(rhs);
    using std::swap;
    swap(myFields, rhs.myFields);
}

void DataProductScope::io(IStream& stream) const
{
    DeclarationScope::io(stream);
}

IMPL_CLONE_BEGIN(DataProductScope, DeclarationScope, DeclarationScope)
IMPL_CLONE_END
IMPL_CLONE_REMAP_BEGIN(DataProductScope, DeclarationScope)
IMPL_CLONE_REMAP(myFields)
IMPL_CLONE_REMAP_END

void DataProductScope::resolveSymbols(Diagnostics& dgn)
{
    for ( auto& d : myDeclarations )
        if ( auto v = d->as<DataProductDeclaration::Field>() )
            myFields.push_back(v);

    DeclarationScope::resolveSymbols(dgn);

    resolveConstructors(dgn);
    resolveDestructor(dgn);
}

void DataProductScope::resolveConstructors(Diagnostics&)
{
    // todo: only make default ctor when no other ctor/dtor defined
}

std::unique_ptr<ProcedureDeclaration> DataProductScope::createDefaultConstructor()
{
    return nullptr;
}

void DataProductScope::resolveDestructor(Diagnostics& dgn)
{
    auto makeTempl = [this, &dgn] {
        auto templ = std::make_unique<TemplateDeclaration>(
            Symbol(lexer::Token(lexer::TokenKind::Identifier,
                                declaration()->symbol().identifier().line(),
                                declaration()->symbol().identifier().column(),
                                "dtor")));
        templ->define(std::make_unique<TemplateScope>(*this, *templ));
        append(std::move(templ));
        addSymbol(dgn, myDeclarations.back()->symbol(), *myDeclarations.back());
    };

    auto symSpace = findSymbolSpace(dgn, "dtor");
    if ( !symSpace ) {
        makeTempl();
        symSpace = findSymbolSpace(dgn, "dtor");
    }

    auto decl = symSpace->findEquivalent(dgn, {});
    if ( !decl ) {
        makeTempl();
        decl = symSpace->findEquivalent(dgn, {});
    }

    auto templ = decl->as<TemplateDeclaration>();
    if ( !templ ) {
        dgn.error(module(), *decl) << "dtor must be declared as a procedure";
        return;
    }

    decl = templ->definition()->findOverload(dgn, SymbolReference("")).decl();
    if ( !decl ) {
        auto proc = createDefaultDestructor();
        auto p = proc.get();
        templ->definition()->append(std::move(proc));
        p->resolvePrototypeSymbols(dgn);
        templ->definition()->addSymbol(dgn, p->symbol(), *p);
        p->resolveSymbols(dgn);
        myDestructor = p;
        return;
    }

    auto proc = decl->as<ProcedureDeclaration>();
    if ( !proc ) {
        dgn.error(module(), *decl) << "dtor must be declared as a procedure";
        return;
    }

    myDestructor = proc;
}

std::unique_ptr<ProcedureDeclaration> DataProductScope::createDefaultDestructor()
{
    auto proc = std::make_unique<ProcedureDeclaration>(
        Symbol(lexer::Token(lexer::TokenKind::Identifier,
                            declaration()->symbol().identifier().line(),
                            declaration()->symbol().identifier().column(),
                            "")),
        nullptr);

    auto ps = std::make_unique<ProcedureScope>(*this, *proc);

    for ( auto f = myFields.rbegin(); f != myFields.rend(); ++f ) {
        auto dp = (*f)->constraint().declaration()->as<DataProductDeclaration>();
        if ( !dp )
            continue;

        if ( auto dtor = dp->definition()->destructor() )
            ps->append(createMemberCallExpression(*dtor, **f));
    }

    proc->define(std::move(ps));
    return proc;
}

DataProductDeclaration* DataProductScope::declaration()
{
    return static_cast<DataProductDeclaration*>(myDeclaration);
}

Slice<DataProductDeclaration::Field*> DataProductScope::fields()
{
    return myFields;
}

const Slice<DataProductDeclaration::Field*> DataProductScope::fields() const
{
    return myFields;
}

ProcedureDeclaration const* DataProductScope::destructor() const
{
    return myDestructor;
}

//
// ProcedureScope

ProcedureScope::ProcedureScope(DeclarationScope& parent,
                               ProcedureDeclaration& declaration)
    : DeclarationScope(Kind::Procedure, &parent.module(), &parent, &declaration)
{
}

ProcedureScope::ProcedureScope(ProcedureScope const& rhs)
    : DeclarationScope(rhs)
{
}

ProcedureScope& ProcedureScope::operator = (ProcedureScope const& rhs)
{
    ProcedureScope(rhs).swap(*this);
    return *this;
}

ProcedureScope::~ProcedureScope() = default;

void ProcedureScope::swap(ProcedureScope& rhs)
{
    DeclarationScope::swap(rhs);
    using std::swap;
    swap(myStatements, rhs.myStatements);
}

void ProcedureScope::io(IStream& stream) const
{
    DeclarationScope::io(stream);
    stream.openArray("statements");
    for ( auto const& e : myStatements )
        stream.next("", e.expression());
    stream.closeArray();
}

IMPL_CLONE(ProcedureScope)

void ProcedureScope::cloneChildren(DeclarationScope& c, clone_map_t& map) const
{
    ProcedureScope& ps = static_cast<ProcedureScope&>(c);
    for ( auto const& e : myStatements )
        ps.myStatements.emplace_back(e.clone(map));
}

void ProcedureScope::remapReferences(clone_map_t const& map)
{
    for ( auto& e : myStatements )
        e.remapReferences(map);
}

void ProcedureScope::resolveSymbols(Diagnostics& dgn)
{
    // Resolve declarations
    DeclarationScope::resolveSymbols(dgn);

    // Resolve expressions
    ScopeResolver resolver(*this);
    Context ctx(dgn, resolver);
    for ( auto& e : myStatements ) {
        ctx.changeStatement(&e);
        e.resolveSymbols(ctx);
    }
    ctx.changeStatement(nullptr);
}

ProcedureDeclaration* ProcedureScope::declaration()
{
    return static_cast<ProcedureDeclaration*>(myDeclaration);
}

void ProcedureScope::append(std::unique_ptr<Expression> expression)
{
    myStatements.emplace_back(std::move(expression));
}

Slice<Statement> const ProcedureScope::statements() const
{
    return myStatements;
}

//
// Statement

Statement::Statement(std::unique_ptr<Expression> expr)
    : myExpression(std::move(expr))
{
}

Statement::Statement(Statement const&)
{
}

Statement& Statement::operator = (Statement const& rhs)
{
    Statement(rhs).swap(*this);
    return *this;
}

Statement::Statement(Statement&& rhs)
    : myExpression(std::move(rhs.myExpression))
    , myUnnamedVariables(std::move(rhs.myUnnamedVariables))
{
}

Statement& Statement::operator = (Statement&& rhs)
{
    this->~Statement();
    new (this) Statement(std::move(rhs));
    return *this;
}

Statement::~Statement() = default;

void Statement::swap(Statement& rhs)
{
    using std::swap;
    swap(myExpression, rhs.myExpression);
    swap(myUnnamedVariables, rhs.myUnnamedVariables);
}

Statement Statement::clone(clone_map_t& map) const
{
    Statement ret(std::unique_ptr<Expression>(myExpression->clone(map)));
    ret.myUnnamedVariables = ast::clone(myUnnamedVariables, map);
    return std::move(ret);
}

void Statement::remapReferences(clone_map_t const& map)
{
    myExpression->remapReferences(map);
    for ( auto& e : myUnnamedVariables )
        e->remapReferences(map);
}

Expression const& Statement::expression() const
{
    return *myExpression;
}

Slice<VariableDeclaration*> const Statement::unnamedVariables() const
{
    return myUnnamedVariables;
}

void Statement::resolveSymbols(Context& ctx)
{
    ctx.resolveExpression(myExpression);
}

VariableDeclaration const* Statement::createUnnamed(ProcedureScope& scope, Declaration const& constraint)
{
    myUnnamedVariables.emplace_back(std::make_unique<VariableDeclaration>(scope, constraint));
    return myUnnamedVariables.back().get();
}

void Statement::appendUnnamed(ProcedureScope& scope, Expression const& expr)
{
    auto decl = resolveIndirections(expr.declaration());
    if ( !decl )
        throw std::runtime_error("unnamed instance must have a type");

    Declaration const* dt = decl;
    if ( auto proc = decl->as<ProcedureDeclaration>() ) {
        if ( isCtor(*proc) )
            dt = proc->parameters()[0]->dataType();
        else
            dt = proc->returnType()->declaration();
    }

    createUnnamed(scope, *dt);
}

//
// TemplateScope

TemplateScope::TemplateScope(DeclarationScope& parent,
                             TemplateDeclaration& declaration)
    : DeclarationScope(Kind::Template, &parent.module(), &parent, &declaration)
{
}

TemplateScope::TemplateScope(TemplateScope const& rhs)
    : DeclarationScope(rhs)
{
}

TemplateScope& TemplateScope::operator = (TemplateScope const& rhs)
{
    TemplateScope(rhs).swap(*this);
    return *this;
}

TemplateScope::~TemplateScope() = default;

void TemplateScope::swap(TemplateScope& rhs)
{
    DeclarationScope::swap(rhs);
}

void TemplateScope::io(IStream& stream) const
{
    DeclarationScope::io(stream);
}

IMPL_CLONE_BEGIN(TemplateScope, DeclarationScope, DeclarationScope)
IMPL_CLONE_END
IMPL_CLONE_REMAP_BEGIN(TemplateScope, DeclarationScope)
IMPL_CLONE_REMAP_END

void TemplateScope::resolveSymbols(Diagnostics& dgn)
{
    // Resolve declarations
    DeclarationScope::resolveSymbols(dgn);
}

TemplateDeclaration* TemplateScope::declaration()
{
    return static_cast<TemplateDeclaration*>(myDeclaration);
}

    } // namespace ast
} // namespace kyfoo
