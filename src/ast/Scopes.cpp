#include <kyfoo/ast/Scopes.hpp>

#include <cassert>
#include <functional>
#include <sstream>

#include <kyfoo/Diagnostics.hpp>
#include <kyfoo/Utilities.hpp>

#include <kyfoo/ast/Axioms.hpp>
#include <kyfoo/ast/ControlFlow.hpp>
#include <kyfoo/ast/Declarations.hpp>
#include <kyfoo/ast/Expressions.hpp>
#include <kyfoo/ast/Fabrication.hpp>
#include <kyfoo/ast/Module.hpp>
#include <kyfoo/ast/Semantics.hpp>
#include <kyfoo/ast/Context.hpp>
#include <kyfoo/ast/Overloading.hpp>
#include <kyfoo/ast/Visitors.hpp>

namespace kyfoo::ast {

//
// DeclarationScope

Scope::Scope(Kind kind,
             Module* module,
             Scope* parent)
    : myKind(kind)
    , myModule(module)
    , myParent(parent)
{
    mySymbols.append(this, "");
}

Scope::Scope(Module& module)
    : Scope(Kind::Base, &module, nullptr)
{
}

Scope::Scope(Scope* parent)
    : Scope(Kind::Base, &parent->module(), parent)
{
}

Scope::Scope(Scope const& rhs)
    : myKind(rhs.myKind)
    , myModule(rhs.myModule)
    , myDeclaration(rhs.myDeclaration)
    , myParent(rhs.myParent)
{
    // mySymbols, myProcedures, and myImports are to be filled out
    // by the semantic passes that build these indices
}

Scope& Scope::operator = (Scope const& rhs)
{
    Scope(rhs).swap(*this);
    return *this;
}

Scope::~Scope() = default;

void Scope::swap(Scope& rhs) noexcept
{
    using kyfoo::swap;
    swap(myKind, rhs.myKind);
    swap(myModule, rhs.myModule);
    swap(myDeclaration, rhs.myDeclaration);
    swap(myParent, rhs.myParent);
    swap(myDeclarations, rhs.myDeclarations);
    swap(myDefinitions, rhs.myDefinitions);
    swap(myLambdas, rhs.myLambdas);
    swap(mySymbols, rhs.mySymbols);
    swap(myImports, rhs.myImports);
}

IMPL_CLONE_NOBASE_BEGIN(Scope, Scope)
IMPL_CLONE_CHILD(myDeclarations)
IMPL_CLONE_CHILD(myDefinitions)
IMPL_CLONE_CHILD(myLambdas)
IMPL_CLONE_END
IMPL_CLONE_REMAP_NOBASE_BEGIN(Scope)
IMPL_CLONE_REMAP(myModule)
IMPL_CLONE_REMAP(myDeclaration)
IMPL_CLONE_REMAP(myParent)
IMPL_CLONE_REMAP(myDeclarations)
IMPL_CLONE_REMAP(myDefinitions)
IMPL_CLONE_REMAP(myLambdas)
IMPL_CLONE_REMAP_END

void Scope::resolveImports(Diagnostics& dgn)
{
    for ( auto& e : myDeclarations ) {
        if ( auto d = e->as<ImportDeclaration>() ) {
            module().import(dgn, d->symbol().token());
        }
    }

    for ( auto& defn : myDefinitions )
        defn->resolveImports(dgn);
}

SymRes Scope::resolveAttributes(Context& ctx)
{
    if ( myAttribRes )
        return *myAttribRes;

    myAttribRes.emplace(SymRes::Success);
    for ( auto& d : myDeclarations )
        *myAttribRes |= d->resolveAttributes(ctx);

    for ( auto& s : myDefinitions )
        *myAttribRes |= ctx.resolveScopeAttributes(*s);

    return *myAttribRes;
}

SymRes Scope::resolveDeclarations(Context& ctx)
{
    if ( myDeclRes )
        return *myDeclRes;

    myDeclRes.emplace(SymRes::Success);
    SymbolDependencyTracker tracker(module(), ctx.diagnostics());
    for ( auto& d : myDeclarations )
        *myDeclRes |= traceDependencies(tracker, *d);

    if ( myDeclRes->error() )
        return *myDeclRes;

    tracker.sortPasses();

    // Resolve top-level declarations
    for ( auto const& symGroup : tracker.groups ) {
        if ( symGroup->upstreamErrors )
            continue;

        for ( auto d : symGroup->declarations ) {
            auto res = ctx.resolveDeclaration(*d);
            if ( res.error() ) {
                for ( auto dep : symGroup->dependents )
                    dep->upstreamErrors = true;
            }
            *myDeclRes |= res;

            addSymbol(ctx.diagnostics(), d->symbol(), *d);
        }
    }

    if ( myDeclRes->error() )
        return *myDeclRes;

    if ( myDeclRes->error() )
        return *myDeclRes;

    for ( auto& decl : myDeclarations ) {
        if ( decl->symbol().prototype().isConcrete() )
            if ( auto defn = getDefinition(*decl) )
                *myDeclRes |= ctx.resolveScopeDeclarations(*defn);
    }

    if ( myDeclRes->error() )
        return* myDeclRes;

    if ( myDeclRes->error() )
        return *myDeclRes;

    // Resolve lambdas
    for ( auto& l : myLambdas )
        *myDeclRes |= ctx.resolveDeclaration(*l);

    return *myDeclRes;
}

SymRes Scope::resolveDefinitions(Context& ctx)
{
    if ( myDefnRes )
        return *myDefnRes;

    myDefnRes.emplace(SymRes::Success);

    // Resolve definitions
    for ( auto& defn : myDefinitions )
        if ( defn->declaration()->symbol().prototype().isConcrete() )
            *myDefnRes |= ctx.resolveScopeDefinitions(*defn);

    if ( myDefnRes->error() )
        return *myDefnRes;

    return *myDefnRes;
}

Lookup Scope::findEquivalent(SymbolReference const& symbol) const
{
    Lookup ret(symbol);
    auto symSpace = findSymbolSpace(symbol.name());
    if ( symSpace ) {
        ret.appendTrace(*symSpace);
        if ( auto decl = symSpace->findEquivalent(symbol.pattern()) )
            ret.resolveTo(*decl);
    }
    else if ( myDeclaration && !symbol.pattern() )
        if ( auto symVar = myDeclaration->symbol().prototype().findVariable(symbol.name()) )
            ret.resolveTo(*symVar);

    return ret;
}

SymbolVariable& Scope::createMetaVariable(lexer::Token const& tok)
{
    ascii::Formatter<ArrayBuffer<>> buf(tok.lexeme().card() + 2 + 5);
    buf('?')(myMetaVariables.card())('_')(tok.lexeme());
    myMetaVariableNames.append(mkString(buf));

    lexer::Token metaTok(lexer::TokenKind::MetaVariable,
                         myMetaVariableNames.back(),
                         tok.location());
    myMetaVariables.append(mk<SymbolVariable>(std::move(metaTok), *this, ab<Box<Expression>>()));
    return *myMetaVariables.back();
}

void Scope::setDeclaration(DefinableDeclaration& declaration)
{
    myDeclaration = &declaration;
    myParent = &myDeclaration->scope();
}

Lookup Scope::findOverload(Context& ctx, SymbolReference const& sym) const
{
    Lookup hit(sym);
    auto symSpace = findSymbolSpace(sym.name());
    if ( symSpace ) {
        hit.appendTrace(*symSpace);
        hit.resolveTo(symSpace->findViableOverloads(ctx, sym.pattern()));
    }

    return hit;
}

void Scope::append(Box<Declaration> declaration)
{
    myDeclarations.append(std::move(declaration));
    myDeclarations.back()->setScope(*this);
}

void Scope::append(Box<Scope> definition)
{
    myDefinitions.append(std::move(definition));
    myDefinitions.back()->myParent = this;
}

void Scope::appendLambda(Box<ProcedureDeclaration> proc,
                         Box<ProcedureScope> defn)
{
    myLambdas.append(std::move(proc));
    myLambdas.back()->setScope(*this);
    append(std::move(defn));
}

void Scope::import(Module& module)
{
    append(mk<ImportDeclaration>(Symbol(lexer::Token(lexer::TokenKind::Identifier, module.name(), lexer::SourceLocation()))));
}

void Scope::merge(Scope& rhs)
{
    myDeclarations.reserve(myDeclarations.card() + rhs.myDeclarations.card());
    for ( auto& e : rhs.myDeclarations ) {
        myDeclarations.append(std::move(e));
        myDeclarations.back()->setScope(*this);
    }

    rhs.myDeclarations.clear();
}

bool Scope::addSymbol(Diagnostics& dgn, Symbol const& sym, Declaration& decl)
{
    auto symSpace = createSymbolSpace(dgn, sym.token().lexeme());

    if ( auto other = symSpace->findEquivalent(sym.prototype().pattern()) ) {
        auto templDecl = decl.as<TemplateDeclaration>();
        if ( !templDecl ) {
            dgn.error(module(), diag::multiple_definition, sym.token())
                .see(*other);
            return false;
        }

        auto otherTemplDecl = other->as<TemplateDeclaration>();
        if ( !otherTemplDecl ) {
            dgn.error(module(), diag::expected_template, sym.token())
                .see(*other);
            return false;
        }

        otherTemplDecl->merge(*templDecl);
        return true;
    }

    symSpace->append(sym.prototype(), decl);
    return true;
}

SymbolSpace* Scope::createSymbolSpace(Diagnostics&, stringv name)
{
    auto symLess = [](SymbolSpace const& s, stringv name) { return s.name() < name; };
    auto l = std::lower_bound(begin(mySymbols), end(mySymbols), name, symLess);
    if ( l != end(mySymbols) && l->name() == name )
        return &*l;

    l = mySymbols.insert(l, SymbolSpace(this, mkString(name)));
    return &*l;
}

SymbolSpace* Scope::findSymbolSpace(stringv name) const
{
    auto symLess = [](SymbolSpace const& s, stringv name) { return s.name() < name; };
    auto symSet = std::lower_bound(begin(mySymbols), end(mySymbols), name, symLess);
    if ( symSet != end(mySymbols) && symSet->name() == name )
        return &*symSet;

    return nullptr;
}

Scope::Kind Scope::kind() const
{
    return myKind;
}

Module& Scope::module()
{
    return *myModule;
}

Module const& Scope::module() const
{
    return *myModule;
}

DefinableDeclaration* Scope::declaration()
{
    return myDeclaration;
}

DefinableDeclaration const* Scope::declaration() const
{
    return myDeclaration;
}

Scope* Scope::parent()
{
    return myParent;
}

Scope const* Scope::parent() const
{
    return myParent;
}

Slice<Declaration const*> Scope::childDeclarations() const
{
    return myDeclarations;
}

Slice<Scope const*> Scope::childDefinitions() const
{
    return myDefinitions;
}

Slice<ProcedureDeclaration const*> Scope::childLambdas() const
{
    return myLambdas;
}

//
// DataTypeScope

DataTypeScope::DataTypeScope(Scope& parent,
                             DataTypeDeclaration& declaration)
    : Scope(Kind::DataType, &parent.module(), &parent)
{
    declaration.define(*this);
}

DataTypeScope::DataTypeScope(DataTypeScope const& rhs)
    : Scope(rhs)
    , myFields(rhs.myFields)
{
}

DataTypeScope& DataTypeScope::operator = (DataTypeScope const& rhs)
{
    DataTypeScope(rhs).swap(*this);
    return *this;
}

DataTypeScope::~DataTypeScope() = default;

void DataTypeScope::swap(DataTypeScope& rhs) noexcept
{
    Scope::swap(rhs);
    using kyfoo::swap;
    swap(myFields, rhs.myFields);
}

IMPL_CLONE_BEGIN(DataTypeScope, Scope, Scope)
IMPL_CLONE_END
IMPL_CLONE_REMAP_BEGIN(DataTypeScope, Scope)
IMPL_CLONE_REMAP(myFields)
IMPL_CLONE_REMAP_END

SymRes DataTypeScope::resolveDeclarations(Context& ctx)
{
    if ( myDeclRes )
        return *myDeclRes;

    return Scope::resolveDeclarations(ctx);
}

SymRes DataTypeScope::resolveDefinitions(Context& ctx)
{
    return Scope::resolveDefinitions(ctx);
}

void DataTypeScope::appendField(Symbol symbol,
                                ab<Box<Expression>> constraints,
                                Box<Expression> init)
{
    auto field = mk<Field>(*declaration(),
                           std::move(symbol),
                           std::move(constraints),
                           std::move(init));
    myFields.append(field.get());
    Scope::append(std::move(field));
}

void DataTypeScope::appendVariation(Symbol sym)
{
    auto v = mk<DataTypeDeclaration>(std::move(sym), declaration());
    myVariations.append(v.get());
    Scope::append(std::move(v));
}

DataTypeDeclaration* DataTypeScope::declaration()
{
    return static_cast<DataTypeDeclaration*>(myDeclaration);
}

Slice<Field*> DataTypeScope::fields()
{
    return myFields;
}

Slice<Field const*> DataTypeScope::fields() const
{
    return myFields;
}

Slice<DataTypeDeclaration*> DataTypeScope::variations()
{
    return myVariations;
}

Slice<DataTypeDeclaration const*> DataTypeScope::variations() const
{
    return myVariations;
}

//
// ProcedureScope

ProcedureScope::ProcedureScope(Scope& parent,
                               ProcedureDeclaration& declaration)
    : ProcedureScope(parent, declaration, nullptr)
{
}

ProcedureScope::ProcedureScope(Scope& parent,
                               ProcedureDeclaration& declaration,
                               BasicBlock* mergeBlock)
    : ProcedureScope(parent, declaration, mergeBlock, lexer::Token(), lexer::Token())
{
}

ProcedureScope::ProcedureScope(Scope& parent,
                               ProcedureDeclaration& declaration,
                               BasicBlock* mergeBlock,
                               lexer::Token openToken,
                               lexer::Token label)
    : Scope(Kind::Procedure, &parent.module(), &parent)
    , myMergeBlock(mergeBlock)
    , myOpenToken(std::move(openToken))
    , myLabel(std::move(label))
{
    declaration.define(*this);
    createBasicBlock();
}

ProcedureScope::ProcedureScope(ProcedureScope& parent,
                               BasicBlock* mergeBlock,
                               lexer::Token openToken,
                               lexer::Token label)
    : Scope(Kind::Procedure, &parent.module(), &parent)
    , myMergeBlock(mergeBlock)
    , myOpenToken(std::move(openToken))
    , myLabel(std::move(label))
{
    myDeclaration = parent.declaration();
    createBasicBlock();
}

ProcedureScope::ProcedureScope(ProcedureScope const& rhs)
    : Scope(rhs)
    , myMergeBlock(rhs.myMergeBlock)
    , myOpenToken(rhs.myOpenToken)
    , myLabel(rhs.myLabel)
{
}

ProcedureScope& ProcedureScope::operator = (ProcedureScope const& rhs)
{
    ProcedureScope(rhs).swap(*this);
    return *this;
}

ProcedureScope::~ProcedureScope() = default;

void ProcedureScope::swap(ProcedureScope& rhs) noexcept
{
    Scope::swap(rhs);
    using kyfoo::swap;
    swap(myMergeBlock, rhs.myMergeBlock);
    swap(myOpenToken, rhs.myOpenToken);
    swap(myLabel, rhs.myLabel);
    swap(myBasicBlocks, rhs.myBasicBlocks);
    swap(myChildScopes, rhs.myChildScopes);
}

IMPL_CLONE_BEGIN(ProcedureScope, Scope, Scope)
IMPL_CLONE_CHILD(myBasicBlocks)
IMPL_CLONE_CHILD(myChildScopes)
IMPL_CLONE_END
IMPL_CLONE_REMAP_BEGIN(ProcedureScope, Scope)
IMPL_CLONE_REMAP(myMergeBlock)
IMPL_CLONE_REMAP(myBasicBlocks)
IMPL_CLONE_REMAP(myChildScopes)
IMPL_CLONE_REMAP_END

SymRes ProcedureScope::resolveDeclarations(Context& ctx)
{
    return Scope::resolveDeclarations(ctx);
}

SymRes ProcedureScope::resolveDefinitions(Context& ctx)
{
    if ( myDefnRes )
        return *myDefnRes;

    Scope::resolveDefinitions(ctx);

    for ( auto& bb : myBasicBlocks )
        *myDefnRes |= bb->resolveSymbols(ctx);

    for ( auto& s : myChildScopes )
        *myDefnRes |= ctx.resolveScope(*s);

    if ( !*myDefnRes )
        return *myDefnRes;

    if ( !isTop() )
        return *myDefnRes;

    *myDefnRes |= resolveReturn(ctx);
    if ( !*myDefnRes )
        return *myDefnRes;

    cacheDominators();
    *myDefnRes |= cacheVariableExtents(ctx);

    return *myDefnRes;
}

bool ProcedureScope::isJumpTarget() const
{
    return myOpenToken.kind() != lexer::TokenKind::Undefined;
}

bool ProcedureScope::isTop() const
{
    if ( !myParent )
        return true;

    if ( auto s = myParent->as<ProcedureScope>() )
        return s->declaration() != declaration();

    return true;
}

BasicBlock const* ProcedureScope::mergeBlock() const
{
    return myMergeBlock;
}

BasicBlock* ProcedureScope::mergeBlock()
{
    return myMergeBlock;
}

lexer::Token const& ProcedureScope::openToken() const
{
    return myOpenToken;
}

lexer::Token const& ProcedureScope::label() const
{
    return myLabel;
}

ProcedureDeclaration* ProcedureScope::declaration()
{
    return static_cast<ProcedureDeclaration*>(myDeclaration);
}

ProcedureDeclaration const* ProcedureScope::declaration() const
{
    return static_cast<ProcedureDeclaration const*>(myDeclaration);
}

Slice<ProcedureScope*> ProcedureScope::childScopes()
{
    return myChildScopes;
}

Slice<ProcedureScope const*> ProcedureScope::childScopes() const
{
    return myChildScopes;
}

Slice<BasicBlock*> ProcedureScope::basicBlocks()
{
    return myBasicBlocks;
}

Slice<BasicBlock const*> ProcedureScope::basicBlocks() const
{
    return myBasicBlocks;
}

BasicBlock* ProcedureScope::entryBlock()
{
    return myBasicBlocks.front().get();
}

BasicBlock const* ProcedureScope::entryBlock() const
{
    return myBasicBlocks.front().get();
}

Expression const* ProcedureScope::deduceReturnType(Context& ctx)
{
    ab<BasicBlock const*> returnBlocks;
    ab<Expression const*> returnTypes;
    ycomb([&returnBlocks, &returnTypes](auto rec, ProcedureScope& scope) -> void {
        for ( auto b : scope.basicBlocks() ) {
            if ( auto ret = b->junction()->as<ReturnJunction>() ) {
                returnBlocks.append(b);
                returnTypes.append(&ret->expression());
            }
        }

        for ( auto c : scope.childScopes() )
            rec(*c);
    })(*this);

    auto [ret, type] = unify(ctx, *declaration(), returnTypes);
    if ( !ret )
        return nullptr;

    return type;
}

void ProcedureScope::append(Box<Expression> expr)
{
    appendStatement(mk<ExpressionStatement>(std::move(expr)));
}

void ProcedureScope::append(Box<VariableDeclaration> var, Box<Expression> expr)
{
    auto p = var.get();
    Scope::append(std::move(var));
    appendStatement(mk<VariableStatement>(*p, std::move(expr)));
}

BasicBlock* ProcedureScope::createBasicBlock()
{
    myBasicBlocks.append(mk<BasicBlock>(this));
    return myBasicBlocks.back().get();
}

void ProcedureScope::popBasicBlock()
{
    myBasicBlocks.pop();
}

ProcedureScope* ProcedureScope::createChildScope(BasicBlock* mergeBlock,
                                                 lexer::Token openToken,
                                                 lexer::Token label)
{
    Box<ProcedureScope> child(new ProcedureScope(*this, mergeBlock, std::move(openToken), std::move(label)));
    myChildScopes.append(std::move(child));
    return myChildScopes.back().get();
}

void ProcedureScope::appendStatement(Box<Statement> stmt)
{
    if ( myBasicBlocks.back()->junction() )
        createBasicBlock();

    myBasicBlocks.back()->append(std::move(stmt));
}

SymRes ProcedureScope::resolveReturn(Context& ctx)
{
    auto const* returnType = declaration()->returnType();
    if ( !returnType )
        returnType = deduceReturnType(ctx);

    return ycomb([&](auto rec, ProcedureScope& proc) -> SymRes {
        SymRes ret;
        for ( auto bb : proc.basicBlocks() ) {
            if ( auto r = bb->junction()->as<ReturnJunction>() ) {
                auto iv = implicitViability(ctx, *returnType, r->expression());
                if ( !iv ) {
                    ctx.error(diag::no_conversion, *r);
                        //.directObject*returnType; // todo
                    ret = SymRes::Fail;
                }

                if ( iv.conversion() ) {
                    r->statement().changeExpression(createApply(createIdentifier(*iv.conversion()),
                                                                r->statement().takeExpression()));
                    ret |= ctx.resolveStatement(r->statement());
                }
            }
        }

        for ( auto s : proc.childScopes() )
            ret |= rec(*s);

        return ret;
    })(*this);
}

void ProcedureScope::cacheDominators()
{
    entryBlock()->myDominators.insert(entryBlock());

    FlatSet<BasicBlock*> all;
    ycomb([&all](auto rec, ProcedureScope& scope) -> void {
        for ( auto b : scope.basicBlocks() )
            all.insert(b);

        for ( auto c : scope.childScopes() )
            rec(*c);
    })(*this);

    auto allButEntry = all;
    allButEntry.zap(entryBlock());

    for ( auto b : allButEntry )
        b->myDominators = all;

    for ( bool change = true; change; ) {
        change = false;
        for ( auto b : allButEntry ) {
            auto oldSize = b->myDominators.card();
            for ( auto p : b->incoming() )
                b->myDominators.intersectWith(p->myDominators);

            b->myDominators.insert(b);
            if ( b->myDominators.card() != oldSize )
                change = true;
        }
    }

    for ( auto b : allButEntry ) {
        auto idom = b->myDominators;
        idom.zap(b);
        for ( auto d : b->myDominators ) {
            if ( d == b )
                continue;

            auto dd = d->myDominators;
            dd.zap(d);
            idom.diffWith(dd);
        }

        assert(idom.card() == 1);
        b->myImmediateDominator = idom[0];
    }
}

SymRes ProcedureScope::cacheVariableExtents(Context& ctx)
{
    if ( !myBasicBlocks )
        return SymRes::Success;

    return buildVariableExtents(ctx, *this, myExtents);
}

//
// TemplateScope

TemplateScope::TemplateScope(Scope& parent,
                             TemplateDeclaration& declaration)
    : Scope(Kind::Template, &parent.module(), &parent)
{
    declaration.define(*this);
}

TemplateScope::TemplateScope(TemplateScope const& rhs)
    : Scope(rhs)
{
}

TemplateScope& TemplateScope::operator = (TemplateScope const& rhs)
{
    TemplateScope(rhs).swap(*this);
    return *this;
}

TemplateScope::~TemplateScope() = default;

void TemplateScope::swap(TemplateScope& rhs) noexcept
{
    Scope::swap(rhs);
}

IMPL_CLONE_BEGIN(TemplateScope, Scope, Scope)
IMPL_CLONE_END
IMPL_CLONE_REMAP_BEGIN(TemplateScope, Scope)
IMPL_CLONE_REMAP_END

SymRes TemplateScope::resolveDeclarations(Context& ctx)
{
    return Scope::resolveDeclarations(ctx);
}

SymRes TemplateScope::resolveDefinitions(Context& ctx)
{
    return Scope::resolveDefinitions(ctx);
}

TemplateDeclaration* TemplateScope::declaration()
{
    return static_cast<TemplateDeclaration*>(myDeclaration);
}

} // namespace kyfoo::ast
