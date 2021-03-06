#include <kyfoo/ast/Expressions.hpp>

#include <algorithm>
#include <iterator>
#include <optional>

#include <kyfoo/Diagnostics.hpp>
#include <kyfoo/Utilities.hpp>

#include <kyfoo/ast/Axioms.hpp>
#include <kyfoo/ast/Context.hpp>
#include <kyfoo/ast/ControlFlow.hpp>
#include <kyfoo/ast/Declarations.hpp>
#include <kyfoo/ast/Fabrication.hpp>
#include <kyfoo/ast/Overloading.hpp>
#include <kyfoo/ast/Scopes.hpp>
#include <kyfoo/ast/Semantics.hpp>

#include <kyfoo/codegen/Codegen.hpp>

namespace kyfoo::ast {

namespace {
    std::optional<SymRes> resolveAsAccessorExpression(Context& ctx,
                                                      Expression const& lhs,
                                                      IdentifierExpression& rhs)
    {
        SymbolReference sym(rhs.token().lexeme());
        if ( auto symExpr = rhs.as<SymbolExpression>() ) {
            auto ret = symExpr->resolveSubExpressions(ctx);
            if ( !ret )
                return ret;

            sym = SymbolReference(symExpr->token().lexeme(), symExpr->expressions());
        }

        auto [lhsInstance, lhsScope] = instanceAccessorScope(lhs);
        if ( lhsScope ) {
            if ( auto hit = ctx.matchOverload(*lhsScope, Resolver::Narrow, sym) ) {
                auto rhsDecl = hit.single();
                if ( !rhsDecl ) {
                    ctx.error(diag::ambiguous, rhs);
                    // todo: enumerate ambiguities
                    return SymRes::Fail;
                }

                rhs.setDeclaration(*rhsDecl);
                if ( lhsInstance && rhsDecl->as<TemplateDeclaration>() )
                    return {};

                return SymRes::Success;
            }
        }

        return {};
    }
} // namespace

//
// Strata

Strata::Strata() = default;

Strata::~Strata() = default;

Expression const* Strata::getType(Expression const& expr)
{
    if ( auto u = expr.as<UniverseExpression>() )
        return &universe(u->level() + 1);

    if ( auto t = expr.as<TupleExpression>() ) {
        auto const exprs = t->expressions();
        if ( !exprs )
            return t;

        ab<Expression const*> types(exprs.card());
        for ( uz i = 0; i < types.card(); ++i )
            types[i] = exprs[i]->type();
        return &tuple(types);
    }

    return nullptr;
}

UniverseExpression const& Strata::universe(uz level)
{
    while ( myUniverses.card() <= level ) {
        auto u = new UniverseExpression(myUniverses.card());
        myUniverses.append(Box<UniverseExpression>(u));
    }

    return *myUniverses[level];
}

TupleExpression const& Strata::tuple(Slice<Expression const*> exprs)
{
    myTuples.append(mk<TupleExpression>(TupleKind::Open,
                                        ast::clone(exprs)));
    return *myTuples.back();
}

//
// Expression

Strata Expression::g_strata;

UniverseExpression const& Expression::universe(uz level)
{
    return g_strata.universe(level);
}

TupleExpression const& Expression::tuple(Slice<Expression const*> exprs)
{
    return g_strata.tuple(exprs);
}

Expression::Expression(Kind kind)
    : myKind(kind)
{
}

Expression::Expression(Kind kind, Expression const* type)
    : myKind(kind)
    , myType(type)
{
}

Expression::Expression(Expression const& rhs)
    : myKind(rhs.myKind)
    , myConstraints(ast::clone(rhs.myConstraints))
    , myType(rhs.myType)
{
}

Expression::~Expression() = default;

void Expression::swap(Expression& rhs) noexcept
{
    using kyfoo::swap;
    swap(myKind, rhs.myKind);
    swap(myConstraints, rhs.myConstraints);
    swap(myType, rhs.myType);
}

IMPL_CLONE_NOBASE_BEGIN(Expression, Expression)
IMPL_CLONE_CHILD(myConstraints)
IMPL_CLONE_END
IMPL_CLONE_REMAP_NOBASE_BEGIN(Expression)
IMPL_CLONE_REMAP(myConstraints)
IMPL_CLONE_REMAP(myType)
IMPL_CLONE_REMAP_END

void Expression::appendConstraint(Box<Expression> expr)
{
    myConstraints.append(std::move(expr));
}

void Expression::appendConstraints(ab<Box<Expression>> exprs)
{
    myConstraints.appendRange(MoveRange(exprs()));
}

Expression::Kind Expression::kind() const
{
    return myKind;
}

Expression const* Expression::type() const
{
    if ( myType )
        return myType;

    if ( auto type = g_strata.getType(*this) ) {
        myType = type;
        return myType;
    }

    return nullptr;
}

void Expression::setType(Expression const* type)
{
    myType = type;
}

void Expression::setType(Box<Expression> type)
{
    auto ptr = type.get();
    appendConstraint(std::move(type));
    setType(ptr);
}

void Expression::setType(Declaration const& decl)
{
    setType(createIdentifier(decl));
}

void Expression::clearType()
{
    myType = nullptr;
}

Slice<Expression*> Expression::constraints()
{
    return myConstraints;
}

Slice<Expression const*> Expression::constraints() const
{
    return myConstraints;
}

ab<Box<Expression>>&& Expression::takeConstraints()
{
    return std::move(myConstraints);
}

//
// LiteralExpression

LiteralExpression::LiteralExpression(lexer::Token token)
    : LiteralExpression(Expression::Kind::Literal, std::move(token))
{
}

LiteralExpression::LiteralExpression(Kind kind, lexer::Token token)
    : Expression(kind)
    , myToken(std::move(token))
{
}

LiteralExpression::LiteralExpression(LiteralExpression const& rhs) = default;

LiteralExpression& LiteralExpression::operator = (LiteralExpression const& rhs)
{
    LiteralExpression(rhs).swap(*this);
    return *this;
}

LiteralExpression::~LiteralExpression() = default;

void LiteralExpression::swap(LiteralExpression& rhs) noexcept
{
    Expression::swap(rhs);
    
    using kyfoo::swap;
    swap(myToken, rhs.myToken);
}

IMPL_CLONE_BEGIN(LiteralExpression, Expression, Expression)
IMPL_CLONE_END
IMPL_CLONE_REMAP_BEGIN(LiteralExpression, Expression)
IMPL_CLONE_REMAP_END

SymRes LiteralExpression::resolveSymbols(Context& ctx)
{
    if ( myType )
        return SymRes::Success;

    switch ( myToken.kind() ) {
    case lexer::TokenKind::Integer:
    {
        setType(*ctx.axioms().intrinsic(intrin::type::IntegerLiteralType));
        return SymRes::Success;
    }

    case lexer::TokenKind::Rational:
    {
        setType(*ctx.axioms().intrinsic(intrin::type::RationalLiteralType));
        return SymRes::Success;
    }

    case lexer::TokenKind::String:
    {
        ctx.module().interpretString(ctx.diagnostics(), myToken);
        setType(*ctx.axioms().intrinsic(intrin::type::StringLiteralType));
        return SymRes::Success;
    }

    default:
        ENFORCEU("unhandled literal-expression");
    }
}

lexer::Token const& LiteralExpression::token() const
{
    return myToken;
}

//
// IdentifierExpression

IdentifierExpression::IdentifierExpression(lexer::Token token)
    : IdentifierExpression(Kind::Identifier, std::move(token), nullptr)
{
}

IdentifierExpression::IdentifierExpression(lexer::Token token, Declaration const& decl)
    : IdentifierExpression(Kind::Identifier, std::move(token), &decl)
{
}

IdentifierExpression::IdentifierExpression(Kind kind, lexer::Token token, Declaration const* decl)
    : Expression(kind)
    , myToken(std::move(token))
{
    if ( decl )
        setDeclaration(*decl);
}

IdentifierExpression::IdentifierExpression(IdentifierExpression const& rhs) = default;

IdentifierExpression& IdentifierExpression::operator = (IdentifierExpression const& rhs)
{
    IdentifierExpression(rhs).swap(*this);
    return *this;
}

IdentifierExpression::~IdentifierExpression() = default;

void IdentifierExpression::swap(IdentifierExpression& rhs) noexcept
{
    Expression::swap(rhs);

    using kyfoo::swap;
    swap(myToken, rhs.myToken);
    swap(myDeclaration, rhs.myDeclaration);
}

IMPL_CLONE_BEGIN(IdentifierExpression, Expression, Expression)
IMPL_CLONE_END
IMPL_CLONE_REMAP_BEGIN(IdentifierExpression, Expression)
IMPL_CLONE_REMAP(myDeclaration)
IMPL_CLONE_REMAP_END

SymRes IdentifierExpression::resolveSymbols(Context& ctx)
{
    if ( myType )
        return SymRes::Success;

    auto ensureType = [this, &ctx]() {
        if ( !myType ) {
            ctx.error(diag::no_type, *this)
                .see(*myDeclaration);
            return SymRes::Fail;
        }

        return SymRes::Success;
    };

    if ( myDeclaration ) {
        if ( hasIndirection(myDeclaration->kind()) ) {
            auto expr = lookThrough(myDeclaration);
            if ( !expr )
                return SymRes::NeedsSubstitution;

            myType = expr->type();
            return SymRes::Success;
        }

        myType = getType(*myDeclaration);
        return ensureType();
    }

    if ( token().kind() == lexer::TokenKind::MetaVariable ) {
        if ( !myDeclaration ) {
            ctx.error(diag::unexpected_meta_variable, token());
            return SymRes::Fail;
        }

        auto n = lookThrough(myDeclaration);
        if ( !n )
            return SymRes::NeedsSubstitution;

        myType = n->type();
        return ensureType();
    }
    else if ( token().kind() == lexer::TokenKind::Identifier ) {
        // todo: remove by generalization
        if ( token().lexeme() == "null" ) {
            setType(*ctx.axioms().intrinsic(intrin::type::PointerNullLiteralType));
            return ensureType();
        }

        auto hit = ctx.matchOverload(token().lexeme());
        myDeclaration = hit.single();
        if ( !myDeclaration ) {
            ctx.error(diag::no_declaration, token());
            return SymRes::Fail;
        }

        if ( ctx.isTopLevel() ) {
            auto ret = tryLowerTemplateToProc(ctx);
            if ( !ret )
                return ret;
        }

        if ( !myType )
            myType = getType(*myDeclaration);

        if ( auto symVar = myDeclaration->as<SymbolVariable>() )
            if ( !symVar->boundExpression() )
                return SymRes::NeedsSubstitution;

        return ensureType();
    }

    ENFORCEU("unhandled identifier-expression");
}

lexer::Token const& IdentifierExpression::token() const
{
    return myToken;
}

Declaration const* IdentifierExpression::declaration() const
{
    return myDeclaration;
}

SymRes IdentifierExpression::tryLowerTemplateToProc(Context& ctx)
{
    if ( !myDeclaration )
        return SymRes::Success;

    auto templ = myDeclaration->as<TemplateDeclaration>();
    if ( !templ )
        return SymRes::Success;

    auto defn = templ->definition();
    if ( !defn ) {
        ctx.error(diag::no_definition, *this)
            .see(*templ);
        return SymRes::Fail;
    }

    auto proc = ctx.matchOverload(*defn, Resolver::Narrow, "").singleAs<ProcedureDeclaration>();
    if ( !proc ) {
        ctx.error(diag::expected_procedure, *this);
        return SymRes::Fail;
    }

    myDeclaration = proc;
    if ( !proc->type() ) {
        ctx.error(diag::no_type, *this) // todo: this is wrong subject
            .see(*proc);
        return SymRes::Fail;
    }

    myType = &proc->type()->to();
    return SymRes::Success;
}

void IdentifierExpression::setDeclaration(Declaration const& decl)
{
    if ( myDeclaration ) {
        switch ( resolveIndirections(myDeclaration)->kind() ) {
        case Declaration::Kind::DataType:
        case Declaration::Kind::Template:
            break;
        default:
            ENFORCEU("identifier resolved more than once");
        }
    }

    myDeclaration = &decl;
    myType = getType(*myDeclaration);
}

void IdentifierExpression::clearDeclaration()
{
    myDeclaration = nullptr;
}

void IdentifierExpression::setToken(lexer::Token const& token)
{
    myToken = token;
}

//
// SymbolExpression

SymbolExpression::SymbolExpression(lexer::Token token,
                                   ab<Box<Expression>> expressions)
    : IdentifierExpression(Expression::Kind::Symbol, std::move(token), nullptr)
    , myExpressions(std::move(expressions))
{
}

SymbolExpression::SymbolExpression(ab<Box<Expression>> expressions)
    : IdentifierExpression(Expression::Kind::Symbol, lexer::Token(), nullptr)
    , myExpressions(std::move(expressions))
{
}

SymbolExpression::SymbolExpression(lexer::Token open,
                                   lexer::Token close,
                                   ab<Box<Expression>> expressions)
    : IdentifierExpression(Expression::Kind::Symbol, lexer::Token(), nullptr)
    , myExpressions(std::move(expressions))
    , myOpenToken(std::move(open))
    , myCloseToken(std::move(close))
{
}

SymbolExpression::SymbolExpression(SymbolExpression const& rhs)
    : IdentifierExpression(rhs)
    , myOpenToken(rhs.myOpenToken)
    , myCloseToken(rhs.myCloseToken)
{
}

SymbolExpression& SymbolExpression::operator = (SymbolExpression const& rhs)
{
    SymbolExpression(rhs).swap(*this);
    return *this;
}

SymbolExpression::~SymbolExpression() = default;

void SymbolExpression::swap(SymbolExpression& rhs) noexcept
{
    IdentifierExpression::swap(rhs);

    using kyfoo::swap;
    swap(myExpressions, rhs.myExpressions);
    swap(myOpenToken, rhs.myOpenToken);
    swap(myCloseToken, rhs.myCloseToken);
}

IMPL_CLONE_BEGIN(SymbolExpression, IdentifierExpression, Expression)
IMPL_CLONE_CHILD(myExpressions)
IMPL_CLONE_END
IMPL_CLONE_REMAP_BEGIN(SymbolExpression, IdentifierExpression)
IMPL_CLONE_REMAP(myExpressions)
IMPL_CLONE_REMAP_END

SymRes SymbolExpression::resolveSymbols(Context& ctx)
{
    if ( declaration() )
        return SymRes::Success;

    if ( token().kind() == lexer::TokenKind::Undefined ) {
        if ( !myExpressions )
            return SymRes::Success;

        auto subject = myExpressions.front()->as<IdentifierExpression>();
        if ( !subject ) {
            ctx.error(diag::expected_symbol_tuple_identifier, *this);
            return SymRes::Fail;
        }

        auto subjectExpression = std::move(myExpressions.front());
        setToken(subject->token());
        myExpressions.remove(begin(myExpressions));

        if ( !myExpressions )
            return ctx.rewrite(std::move(subjectExpression));
    }

    auto ret = ctx.resolveExpressions(myExpressions);
    if ( !ret )
        return ret;

    SymbolReference sym(token().lexeme(), myExpressions);
    auto hit = ctx.matchOverload(sym);
    if ( !hit ) {
        ctx.error(diag::no_declaration, *this);
        return SymRes::Fail;
    }

    auto decl = hit.single();
    if ( !decl ) {
        ctx.error(diag::instantiate_error, *this)
            .see(std::move(hit));
        return SymRes::Fail;
    }

    setDeclaration(*decl);
    return SymRes::Success;
}

SymRes SymbolExpression::resolveSubExpressions(Context& ctx)
{
    return ctx.resolveExpressions(begin(myExpressions), end(myExpressions));
}

Slice<Expression*> SymbolExpression::expressions()
{
    return myExpressions;
}

Slice<Expression const*> SymbolExpression::expressions() const
{
    return myExpressions;
}

lexer::Token const& SymbolExpression::openToken() const
{
    return myOpenToken;
}

lexer::Token const& SymbolExpression::closeToken() const
{
    return myCloseToken;
}

ab<Box<Expression>>& SymbolExpression::internalExpressions()
{
    return myExpressions;
}

//
// DotExpression

DotExpression::DotExpression(bool modScope,
                             ab<Box<Expression>> exprs)
    : Expression(Kind::Dot)
    , myExpressions(std::move(exprs))
    , myModScope(modScope)
{
}

DotExpression::DotExpression(DotExpression const& rhs)
    : Expression(rhs)
    , myModScope(rhs.myModScope)
{
    // clone myExpressions, myTypeAsRef
}

DotExpression& DotExpression::operator = (DotExpression const& rhs)
{
    DotExpression(rhs).swap(*this);
    return *this;
}

DotExpression::~DotExpression() = default;

void DotExpression::swap(DotExpression& rhs) noexcept
{
    Expression::swap(rhs);
    using kyfoo::swap;
    swap(myExpressions, rhs.myExpressions);
    swap(myTypeAsRef, rhs.myTypeAsRef);
    swap(myModScope, rhs.myModScope);
}

IMPL_CLONE_BEGIN(DotExpression, Expression, Expression)
IMPL_CLONE_CHILD(myExpressions)
IMPL_CLONE_CHILD(myTypeAsRef)
IMPL_CLONE_END
IMPL_CLONE_REMAP_BEGIN(DotExpression, Expression)
IMPL_CLONE_REMAP(myExpressions)
IMPL_CLONE_REMAP(myTypeAsRef)
IMPL_CLONE_REMAP_END

SymRes DotExpression::resolveSymbols(Context& ctx)
{
    return resolveSymbols(ctx, myExpressions.card());
}

SymRes DotExpression::resolveSymbols(Context& ctx, uz subExpressionLimit)
{
    if ( myType )
        return SymRes::Success;

    Resolver resolver(*ctx.module().scope());
    if ( isModuleScope() )
        ctx.changeResolver(resolver);

    auto ret = ctx.resolveExpression(myExpressions.front());
    if ( !ret )
        return ret;

    auto isConstituent = [](Expression const& expr) {
        if ( auto lit = expr.as<LiteralExpression>() )
            return lit->token().kind() == lexer::TokenKind::Integer;

        return ast::as<Field>(expr) != nullptr;
    };

    bool isRef = false;
    for ( uz i = 1; i < subExpressionLimit; ++i ) {
        auto const* lhs = myExpressions[i - 1].get();
        if ( refType(*lhs->type()) )
            isRef = true;

        if ( myExpressions[i]->type() ) {
            if ( !isConstituent(*myExpressions[i]) )
                isRef = false;

            continue;
        }

        auto failedAsAccessor = false;
        if ( auto rhs = identify(*myExpressions[i]) ) {
            if ( auto o = resolveAsAccessorExpression(ctx, *lhs, *rhs) ) {
                ret |= *o;
                if ( !ret )
                    return ret;

                if ( !isConstituent(*rhs) )
                    isRef = false;

                continue;
            }

            failedAsAccessor = true;
        }
        else {
            ret = ctx.resolveExpression(myExpressions[i]);
        }

        if ( !ret )
            return ret;

        auto rhs = myExpressions[i].get();
        auto e = resolveIndirections(rhs);

        if ( auto lit = e->as<LiteralExpression>() ) {
            if ( lit->token().kind() != lexer::TokenKind::Integer ) {
                ctx.error(diag::expected_integer, *e);
                return SymRes::Fail;
            }

            int index = stoi(lit->token().lexeme());
            if ( index < 0 ) {
                ctx.error(diag::no_field, *e);
                return SymRes::Fail;
            }

            auto const* composite = resolveIndirections(lhs);
            bool binder = false;
            if ( auto b = getBinder(*composite) ) {
                composite = removeAllReferences(*b->type());
                binder = true;
            }

            if ( auto decl = getDeclaration(*composite) ) {
                auto dt = decl->as<DataTypeDeclaration>();
                if ( !dt ) {
                    ctx.error(diag::expected_composite, *e)
                        .see(*dt);
                    return SymRes::Fail;
                }

                auto defn = dt->definition();
                if ( !defn ) {
                    ctx.error(diag::no_definition, *composite)
                        .see(*dt);
                    return SymRes::Fail;
                }

                if ( uz(index) >= defn->fields().card() ) {
                    ctx.error(diag::no_field, *e)
                        .see(*dt);
                    return SymRes::Fail;
                }

                auto const tok = lit->token();
                myExpressions[i] = createIdentifier(mkToken(tok.lexeme(),
                                                            tok.location()),
                                                    *defn->fields()[index]);
            }
            else if ( auto tup = composite->as<TupleExpression>() ) {
                if ( uz(index) >= tup->elementsCount() ) {
                    ctx.error(diag::no_field, *e);
                    return SymRes::Fail;
                }

                if ( binder )
                    lit->setType(tup->elements()[index]);
                else
                    lit->setType(tup->elements()[index]->type());
            }
            else {
                ctx.error(diag::no_field, *e);
                return SymRes::Fail;
            }

            continue;
        }

        ab<Box<Expression>> lhsExprs(MoveRange(myExpressions(0, i)));
        myExpressions.remove(begin(myExpressions), begin(myExpressions) + i);
        subExpressionLimit -= i;
        i = 0;
        auto subj = std::move(myExpressions.front());
        auto arg = mk<DotExpression>(isModuleScope(), std::move(lhsExprs));
        auto app = mk<ApplyExpression>(createPtrList<Expression>(std::move(subj), std::move(arg)));
        myExpressions.front() = std::move(app);
        ret = ctx.resolveExpression(myExpressions.front());
        if ( !ret ) {
            if ( failedAsAccessor ) {
                if ( auto appExpr = myExpressions[i]->as<ApplyExpression>() ) 
                    ctx.error(diag::no_member, *appExpr->subject());
                        //.directObject(*appExpr->arguments()[0]); todo: direct object
                else
                    ctx.error(diag::no_member_or_application, *this);
            }
            
            return ret;
        }
    }

    if ( !myExpressions )
        return ctx.rewrite(createEmptyExpression());
    else if ( myExpressions.card() == 1 )
        return ctx.rewrite(std::move(myExpressions.front()));

    if ( subExpressionLimit == myExpressions.card() ) {
        auto t = myExpressions.back()->type();
        if ( isRef ) {
            myTypeAsRef = createRefType(front(*t).location(), clone(t));
            ret |= ctx.resolveExpression(myTypeAsRef);
            if ( !ret )
                return ret;

            t = myTypeAsRef.get();
        }

        setType(t);
    }

    return SymRes::Success;
}

Slice<Expression*> DotExpression::expressions()
{
    return myExpressions;
}

Slice<Expression const*> DotExpression::expressions() const
{
    return myExpressions;
}

Expression* DotExpression::top(uz index)
{
    if ( index >= myExpressions.card() )
        return nullptr;

    return myExpressions[myExpressions.card() - index - 1].get();
}

Expression const* DotExpression::top(uz index) const
{
    return const_cast<DotExpression*>(this)->top(index);
}

bool DotExpression::isModuleScope() const
{
    return myModScope;
}

Box<Expression> DotExpression::takeTop(uz index)
{
    auto const takeIndex = myExpressions.card() - 1 - index;
    auto m = begin(myExpressions) + takeIndex;
    auto ret = std::move(*m);
    myExpressions.remove(m);

    myType = nullptr;

    return ret;
}

//
// ApplyExpression

ApplyExpression::ApplyExpression(ab<Box<Expression>> expressions)
    : Expression(Expression::Kind::Apply)
    , myExpressions(std::move(expressions))
{
}

ApplyExpression::ApplyExpression(ApplyExpression const& rhs)
    : Expression(rhs)
{
}

ApplyExpression& ApplyExpression::operator = (ApplyExpression const& rhs)
{
    ApplyExpression(rhs).swap(*this);
    return *this;
}

ApplyExpression::~ApplyExpression() = default;

void ApplyExpression::swap(ApplyExpression& rhs) noexcept
{
    Expression::swap(rhs);

    using kyfoo::swap;
    swap(myExpressions, rhs.myExpressions);
}

IMPL_CLONE_BEGIN(ApplyExpression, Expression, Expression)
IMPL_CLONE_CHILD(myExpressions)
IMPL_CLONE_END
IMPL_CLONE_REMAP_BEGIN(ApplyExpression, Expression)
IMPL_CLONE_REMAP(myExpressions)
IMPL_CLONE_REMAP_END

SymRes ApplyExpression::resolveSymbols(Context& ctx)
{
    if ( myType )
        return SymRes::Success;

L_restart:
    auto tryLowerSingle = [this, &ctx]() -> SymRes {
        if ( !myExpressions )
            return ctx.rewrite(mk<TupleExpression>(TupleKind::Open, std::move(myExpressions)));
        else if ( myExpressions.card() == 1 )
            return ctx.rewrite(std::move(myExpressions.front()));

        return SymRes::Success;
    };

    auto ret = tryLowerSingle();
    if ( !ret )
        return ret;

    ret |= ctx.resolveExpressions(std::next(begin(myExpressions)), end(myExpressions));
    if ( !ret )
        return ret;

    flatten(std::next(begin(myExpressions)));
    ret |= tryLowerSingle(); // again
    if ( !ret )
        return ret;

    for ( auto e : arguments() ) {
        if ( auto id = identify(*e) ) {
            auto r = id->tryLowerTemplateToProc(ctx);
            if ( !r )
                return r;
        }
    }

    if ( auto lit = subject()->as<LiteralExpression>() )
        return ctx.rewrite(mk<TupleExpression>(TupleKind::Open, std::move(myExpressions)));

    if ( auto id = subject()->as<IdentifierExpression>() ) {
        if ( id->token().lexeme() && !id->declaration() ) {
            if ( !resolveSubjectAsUFCSMethod(ctx, *id) ) {
                auto hit = ctx.matchOverload(id->token().lexeme());
                if ( !hit )
                    return ctx.rewrite(mk<SymbolExpression>(std::move(myExpressions)));
            }
        }
    }

    if ( auto dot = subject()->as<DotExpression>() ) {
        ret |= dot->resolveSymbols(ctx, dot->expressions().card() - 1);
        if ( !ret )
            return ret;

        auto rhs = identify(*dot->top());
        if ( !rhs )
            goto L_notMethod;

        if ( auto o = resolveAsAccessorExpression(ctx, *dot->top(1), *rhs) ) {
            ret |= *o;
            if ( !ret )
                return ret;

            auto decl = getDeclaration(dot->top());
            if ( decl && (decl->kind() == Declaration::Kind::Procedure || decl->kind() == Declaration::Kind::Template) ) {
                auto thisDecl = getDeclaration(dot->top(1));
                if ( !thisDecl || !isDefinableDeclaration(thisDecl->kind()) ) {
                    myExpressions.insert(begin(myExpressions), dot->takeTop());
                    goto L_restart;
                }
            }
        }
        else {
            myExpressions.insert(begin(myExpressions), dot->takeTop());
            goto L_restart;
        }

        goto L_notMethod;
    }
    else {
L_notMethod:
        ret |= ctx.resolveExpression(myExpressions.front());
        if ( !ret )
            return ret;
    }

    auto const subjType = removeAllReferences(*myExpressions.front()->type());
    switch ( subjType->kind() ) {
    case Kind::Tuple:
        return elaborateTuple(ctx);

    case Kind::Arrow:
        myProc = getProcedure(*myExpressions.front());
        break;

    case Kind::Universe:
        ret |= lowerToStaticCall(ctx);
        break;

    default:
        ret |= lowerToApplicable(ctx);
    }

    if ( !ret )
        return ret;

    ArrowExpression const* arrow ;
    if ( myProc ) {
        arrow = myProc->type();
        if ( !arrow ) {
            ctx.error(diag::no_type, *this)
                .see(*myProc);
            return SymRes::Fail;
        }
    }
    else {
        arrow = subjType->as<ArrowExpression>();
        if ( !arrow ) {
            ctx.error(diag::no_conversion, *this)
                .see(ctx.resolver().scope(), *myExpressions.front());
            return SymRes::Fail;
        }
    }

    if ( !variance(ctx, arrow->from(), arguments()) ) {
        ctx.error(diag::no_conversion, *this)
            .expected(ctx.resolver().scope(), arrow->sliceFrom())
            .received(ctx.resolver().scope(), arguments());
        return SymRes::Fail;
    }

    myType = &arrow->to();

    return ret;
}

Declaration const* ApplyExpression::resolveSubjectAsUFCSMethod(Context& ctx, IdentifierExpression& id)
{
    // Look for method on first argument (UFCS)
    if ( !arguments() )
        return nullptr;

    auto selfExpr = arguments().front();
    auto selfType = selfExpr->type();
    auto scope = staticAccessorScope(*selfType);
    if ( !scope )
        return nullptr;

    auto hit = ctx.matchOverload(*scope, Resolver::Narrow, SymbolReference(id.token().lexeme()));
    if ( auto templ = hit.singleAs<TemplateDeclaration>() ) {
        id.setDeclaration(*resolveIndirections(templ));
        return id.declaration();
    }

    return nullptr;
}

SymRes ApplyExpression::lowerToApplicable(Context& ctx)
{
    auto const& subj = *subject();
    auto applicable = getDeclaration(removeAllReferences(*subj.type()));
    if ( !applicable ) {
        ctx.error(diag::expected_applicable, subj);
        return SymRes::Fail;
    }

    auto dt = applicable->as<DataTypeDeclaration>();
    if ( !dt ) {
        ctx.error(diag::expected_applicable, subj);
        return SymRes::Fail;
    }

    auto defn = dt->definition();
    if ( !defn ) {
        ctx.error(diag::no_definition, subj)
            .see(*dt);
        return SymRes::Fail;
    }

    auto hit = ctx.matchOverloadUsingImplicitConversions(*defn, Resolver::Narrow, "", myExpressions);
    if ( !hit ) {
        ctx.error(diag::no_conversion, subj)
            .see(*dt);
        return SymRes::Fail;
    }

    auto proc = hit.singleAs<ProcedureDeclaration>();
    if ( !proc ) {
        ctx.error(diag::expected_procedure, subj)
            .see(*hit.single())
            .see(*dt);
        return SymRes::Fail;
    }

    myProc = proc;
    myExpressions.insert(begin(myExpressions), createIdentifier(*myProc));

    return SymRes::Success;
}

SymRes ApplyExpression::lowerToStaticCall(Context& ctx)
{
    auto const& subj = *subject();
    auto decl = resolveIndirections(getDeclaration(subj));
    if ( !decl ) {
        ctx.error(diag::no_declaration, subj);
        return SymRes::Fail;
    }

    auto defDecl = getDefinableDeclaration(*decl);
    if ( !defDecl ) {
        ctx.error(diag::no_definition, subj)
            .see(*decl);
        return SymRes::Fail;
    }

    auto defn = defDecl->definition();
    if ( !defn ) {
        ctx.error(diag::no_definition, *defDecl);
        return SymRes::Fail;
    }

    auto hit = ctx.matchOverloadUsingImplicitConversions(*defn, Resolver::Narrow, "", mutableArgs());
    auto proc = hit.singleAs<ProcedureDeclaration>();
    if ( !proc ) {
        ctx.error(diag::no_invocation, *this)
            .see(std::move(hit));
        return SymRes::Fail;
    }

    myProc = proc;
    return SymRes::Success;
}

SymRes ApplyExpression::elaborateTuple(Context& ctx)
{
    auto subjectTypeRef = resolveIndirections(resolveIndirections(*myExpressions.front())->type());
    auto subjectType = removeAllReferences(*subjectTypeRef)->as<TupleExpression>();
    if ( !subjectType ) {
        ctx.error(diag::expected_tuple, *myExpressions.front());
        return SymRes::Fail;
    }

    if ( subjectType->kind() != TupleKind::Closed ) {
        ctx.error(diag::not_implemented, *subject());
        return SymRes::Fail;
    }

    if ( myExpressions.card() != 2 ) {
        ctx.error(diag::expected_arity, *subject());
        return SymRes::Fail;
    }

    if ( auto lit = myExpressions[1]->as<LiteralExpression>() ) {
        if ( lit->token().kind() != lexer::TokenKind::Integer ) {
            ctx.error(diag::expected_integer, *lit);
            return SymRes::Fail;
        }

        return ctx.rewrite(mk<DotExpression>(false, std::move(myExpressions)));
    }

    if ( subjectType->elementsCount() <= 1 ) {
        ctx.error(diag::not_implemented, *subject());
        return SymRes::Fail;
    }

    auto argType = myExpressions[1]->type();
    auto elementType = subjectType->expressions().front();

    if ( auto arrow = argType->as<ArrowExpression>() ) {
        Box<IdentifierExpression> refStorage;
        auto refElementType = elementType;
        if ( !refType(*refElementType) ) {
            refStorage = createIdentifier(*ctx.matchOverload(SymbolReference("ref", slice(elementType))).single());
            refElementType = refStorage.get();
        }

        if ( !variance(ctx, arrow->from(), *refElementType) ) {
            ctx.error(diag::no_conversion, *myExpressions[1])
                // .directObject(*myExpressions[0]) todo
                .expected(ctx.resolver().scope(), subjectType->expressions()(0, 1))
                .received(ctx.resolver().scope(), arrow->sliceFrom());
            return SymRes::Fail;
        }

        myType = elementType;
        return SymRes::Success;
    }

    auto indexType = createIdentifier(*ctx.axioms().intrinsic(intrin::type::size_t));
    auto via = implicitViability(ctx, *indexType, *argType);
    if ( !via ) {
        ctx.error(diag::no_conversion, *myExpressions[1]);
        return SymRes::Fail;
    }

    ctx.shimConversion(myExpressions[1], via);

    if ( subjectType->expressions().card() == 1 ) {
        auto type = subjectType->expressions().front();
        if ( refType(*subjectTypeRef) ) {
            Box<Expression> t = createRefType(front(*type).location(), clone(type));
            if ( auto r = ctx.resolveExpression(t); !r )
                return r;
            type = t.get();
            ctx.module().fabricate(std::move(t));
        }

        myType = type;
    }
    else {
        myType = &Expression::tuple(subjectType->expressions());
    }

    return SymRes::Success;
}

/**
 * Flatten apply-expressions that don't start with a procedure identifier
 */
void ApplyExpression::flatten()
{
    return flatten(begin(myExpressions));
}

void ApplyExpression::flatten(ab<Box<Expression>>::Iterator first)
{
    for ( auto i = first; i != end(myExpressions); ) {
        if ( auto tuple = (*i)->as<TupleExpression>() ) {
            if ( tuple->kind() == TupleKind::Open ) {
                auto index = std::distance(begin(myExpressions), i) + tuple->myExpressions.card();
                myExpressions.insertRange(i, MoveRange(tuple->myExpressions()));
                i = std::next(begin(myExpressions), index);
                goto L_removeItem;
            }
        }

        ++i;
        continue;

    L_removeItem:
        i = myExpressions.remove(i);
    }
}

Slice<Expression*> ApplyExpression::expressions()
{
    return myExpressions;
}

Slice<Expression const*> ApplyExpression::expressions() const
{
    return myExpressions;
}

Expression* ApplyExpression::subject()
{
    return myExpressions.front().get();
}

Expression const* ApplyExpression::subject() const
{
    return myExpressions.front().get();
}

Slice<Expression*> ApplyExpression::arguments()
{
    return myExpressions(1, $);
}

Slice<Expression const*> ApplyExpression::arguments() const
{
    return myExpressions(1, $);
}

ProcedureDeclaration const* ApplyExpression::procedure() const
{
    return myProc;
}

Slice<Box<Expression>> ApplyExpression::mutableArgs()
{
    return myExpressions(1, $);
}

//
// LambdaExpression

LambdaExpression::LambdaExpression(ProcedureDeclaration& proc)
    : Expression(Kind::Lambda)
    , myProc(&proc)
{
}

LambdaExpression::LambdaExpression(LambdaExpression const& rhs) = default;

LambdaExpression& LambdaExpression::operator = (LambdaExpression const& rhs)
{
    LambdaExpression(rhs).swap(*this);
    return *this;
}

LambdaExpression::~LambdaExpression() = default;

void LambdaExpression::swap(LambdaExpression& rhs) noexcept
{
    Expression::swap(rhs);
    using kyfoo::swap;
    swap(myProc, rhs.myProc);
}

IMPL_CLONE_BEGIN(LambdaExpression, Expression, Expression)
IMPL_CLONE_END
IMPL_CLONE_REMAP_BEGIN(LambdaExpression, Expression)
IMPL_CLONE_REMAP(myProc)
IMPL_CLONE_REMAP_END

SymRes LambdaExpression::resolveSymbols(Context& /*ctx*/)
{
    ENFORCE(myProc->definition(), "lambda missing body");

    myType = myProc->type();
    if ( !myType )
        return SymRes::Fail;

    return SymRes::Success;
}

ProcedureDeclaration const& LambdaExpression::procedure() const
{
    return *myProc;
}

ProcedureDeclaration& LambdaExpression::procedure()
{
    return *myProc;
}

//
// AssignExpression

AssignExpression::AssignExpression(Box<Expression> lhs,
                                   Box<Expression> rhs)
    : Expression(Kind::Assign)
    , myLeft(std::move(lhs))
    , myRight(std::move(rhs))
{
}

AssignExpression::AssignExpression(VariableDeclaration const& var,
                                   Box<Expression> expression)
    : Expression(Kind::Assign)
    , myLeft(createIdentifier(var))
    , myRight(std::move(expression))
{
}

AssignExpression::AssignExpression(AssignExpression const& rhs)
    : Expression(rhs)
{
}

AssignExpression& AssignExpression::operator = (AssignExpression const& rhs)
{
    AssignExpression(rhs).swap(*this);
    return *this;
}

AssignExpression::~AssignExpression() = default;

void AssignExpression::swap(AssignExpression& rhs) noexcept
{
    Expression::swap(rhs);
    using kyfoo::swap;
    swap(myLeft, rhs.myLeft);
    swap(myRight, rhs.myRight);
}

IMPL_CLONE_BEGIN(AssignExpression, Expression, Expression)
IMPL_CLONE_CHILD(myLeft)
IMPL_CLONE_CHILD(myRight)
IMPL_CLONE_END
IMPL_CLONE_REMAP_BEGIN(AssignExpression, Expression)
IMPL_CLONE_REMAP(myLeft)
IMPL_CLONE_REMAP(myRight)
IMPL_CLONE_REMAP_END

SymRes AssignExpression::resolveSymbols(Context& ctx)
{
    if ( myType )
        return SymRes::Success;

    ctx.error(diag::unexpected_assign_expression, *this);
    return SymRes::Fail;
}

Expression const& AssignExpression::left() const
{
    return *myLeft;
}

Expression& AssignExpression::left()
{
    return *myLeft;
}

Expression const& AssignExpression::right() const
{
    return *myRight;
}

Expression& AssignExpression::right()
{
    return *myRight;
}

Box<Expression> AssignExpression::takeLeft()
{
    return std::move(myLeft);
}

Box<Expression> AssignExpression::takeRight()
{
    return std::move(myRight);
}

//
// TupleExpression

TupleKind toTupleKind(lexer::TokenKind open, lexer::TokenKind close)
{
    if ( open == lexer::TokenKind::OpenParen ) {
        if ( close == lexer::TokenKind::CloseParen )
            return TupleKind::Open;
        else if ( close == lexer::TokenKind::CloseBracket )
            return TupleKind::OpenLeft;
    }
    else if ( open == lexer::TokenKind::OpenBracket ) {
        if ( close == lexer::TokenKind::CloseParen )
            return TupleKind::OpenRight;
        else if ( close == lexer::TokenKind::CloseBracket )
            return TupleKind::Closed;
    }

    ENFORCEU("invalid tuple expression syntax");
}

const char* to_string(TupleKind kind)
{
    switch (kind) {
#define X(a) case TupleKind::a: return #a;
        TUPLE_KINDS(X)
#undef X
    }

    ENFORCEU("invalid tuple kind");
}

const char* presentTupleOpen(TupleKind kind)
{
    switch (kind) {
    case TupleKind::Open:
    case TupleKind::OpenLeft:
        return "(";

    default:
        return "[";
    }
}

const char* presentTupleClose(TupleKind kind)
{
    switch (kind) {
    case TupleKind::Open:
    case TupleKind::OpenLeft:
        return ")";

    default:
        return "]";
    }
}
const char* presentTupleWeave(TupleKind)
{
    return ", ";
}

TupleExpression::TupleExpression(TupleKind kind,
                                 ab<Box<Expression>> expressions)
    : Expression(Expression::Kind::Tuple)
    , myKind(kind)
    , myExpressions(std::move(expressions))
{
}

TupleExpression::TupleExpression(lexer::Token const& open,
                                 lexer::Token const& close,
                                 ab<Box<Expression>> expressions)
    : Expression(Expression::Kind::Tuple)
    , myKind(toTupleKind(open.kind(), close.kind()))
    , myExpressions(std::move(expressions))
    , myOpenToken(open)
    , myCloseToken(close)
{
}

TupleExpression::TupleExpression(ab<Box<Expression>> expressions,
                                 Box<Expression> cardExpression)
    : Expression(Expression::Kind::Tuple)
    , myKind(TupleKind::Closed)
    , myExpressions(std::move(expressions))
    , myCardExpression(std::move(cardExpression))
{
}

TupleExpression::TupleExpression(TupleExpression const& rhs)
    : Expression(rhs)
    , myKind(rhs.myKind)
    , myCard(rhs.myCard)
    , myOpenToken(rhs.myOpenToken)
    , myCloseToken(rhs.myCloseToken)
{
}

TupleExpression& TupleExpression::operator = (TupleExpression const& rhs)
{
    TupleExpression(rhs).swap(*this);
    return *this;
}

void TupleExpression::swap(TupleExpression& rhs) noexcept
{
    Expression::swap(rhs);

    using kyfoo::swap;
    swap(myKind, rhs.myKind);
    swap(myExpressions, rhs.myExpressions);
    swap(myCardExpression, rhs.myCardExpression);
    swap(myCard, rhs.myCard);
    swap(myOpenToken, rhs.myOpenToken);
    swap(myCloseToken, rhs.myCloseToken);
}

TupleExpression::~TupleExpression() = default;

IMPL_CLONE_BEGIN(TupleExpression, Expression, Expression)
IMPL_CLONE_CHILD(myExpressions)
IMPL_CLONE_CHILD(myCardExpression)
IMPL_CLONE_END
IMPL_CLONE_REMAP_BEGIN(TupleExpression, Expression)
IMPL_CLONE_REMAP(myExpressions)
IMPL_CLONE_REMAP(myCardExpression)
IMPL_CLONE_REMAP_END

SymRes TupleExpression::resolveSymbols(Context& ctx)
{
    auto ret = ctx.resolveExpressions(myExpressions);
    if ( !ret )
        return ret;

    if ( myCardExpression ) {
        ret |= ctx.resolveExpression(myCardExpression);
        if ( !ret )
            return ret;

        auto lit = resolveIndirections(myCardExpression.get())->as<LiteralExpression>();
        if ( !lit || lit->token().kind() != lexer::TokenKind::Integer ) {
            ctx.error(diag::expected_integer, *myCardExpression);
            return SymRes::Fail;
        }

        auto n = stoi(lit->token().lexeme());
        if ( n < 0 ) {
            ctx.error(diag::unexpected_negative_integer, *myCardExpression);
            return SymRes::Fail;
        }

        myCard = static_cast<uz>(n);
    }
    else {
        myCard = 1;
    }

    if ( myKind == TupleKind::Open ) {
        if ( myExpressions.card() == 1 )
            return ctx.rewrite(std::move(myExpressions[0]));
    }

    flattenOpenTuples(myExpressions);

    // myType is generated lazily
    return SymRes::Success;
}

TupleKind TupleExpression::kind() const
{
    return myKind;
}

lexer::Token const& TupleExpression::openToken() const
{
    return myOpenToken;
}

lexer::Token const& TupleExpression::closeToken() const
{
    return myCloseToken;
}

Slice<Expression*> TupleExpression::expressions()
{
    return myExpressions;
}

Slice<Expression const*> TupleExpression::expressions() const
{
    return myExpressions;
}

ExpressionArray TupleExpression::elements() const
{
    return ExpressionArray(myExpressions, elementsCount());
}

uz TupleExpression::elementsCount() const
{
    return myCard;
}

/**
 * Flatten open-tuples
 */

std::optional<ab<Box<Expression>>::Iterator>
TupleExpression::tryExpandTuple(ab<Box<Expression>>& exprs,
                                ab<Box<Expression>>::Iterator i)
{
    if ( auto tup = (*i)->as<TupleExpression>() )
        if ( tup->kind() == TupleKind::Open )
            if ( tup->myCard == 1 || !tup->myCardExpression )
                return expandTuple(exprs, i);

    return {};
}

ab<Box<Expression>>::Iterator
TupleExpression::expandTuple(ab<Box<Expression>>& exprs,
                             ab<Box<Expression>>::Iterator i)
{
    auto& tup = static_cast<TupleExpression&>(**i);
    return exprs.remove(expandIntoList(tup, exprs, i));
}

ab<Box<Expression>>::Iterator
TupleExpression::expandIntoList(TupleExpression& tup,
                                ab<Box<Expression>>& exprs,
                                ab<Box<Expression>>::Iterator i)
{
    auto const index = std::distance(begin(exprs), i);
    auto const card = tup.myExpressions.card();
    exprs.insertRange(i, MoveRange(tup.myExpressions()));
    return std::next(begin(exprs), index + card);
}

void TupleExpression::flattenOpenTuples(ab<Box<Expression>>& exprs)
{
    for ( auto i = begin(exprs); i != end(exprs); ) {
        if ( auto o = tryExpandTuple(exprs, i) )
            i = *o;
        else
            ++i;
    }
}

//
// ArrowExpression

ArrowExpression::ArrowExpression(Box<Expression> from,
                                 Box<Expression> to)
    : Expression(Kind::Arrow)
    , myFrom(std::move(from))
    , myTo(std::move(to))
{
}

ArrowExpression::ArrowExpression(ArrowExpression const& rhs)
    : Expression(rhs)
{
}

ArrowExpression& ArrowExpression::operator = (ArrowExpression const& rhs)
{
    ArrowExpression(rhs).swap(*this);
    return *this;
}

ArrowExpression::~ArrowExpression() = default;

void ArrowExpression::swap(ArrowExpression& rhs) noexcept
{
    Expression::swap(rhs);
    using kyfoo::swap;
    swap(myFrom, rhs.myFrom);
    swap(myTo, rhs.myTo);
}

IMPL_CLONE_BEGIN(ArrowExpression, Expression, Expression)
IMPL_CLONE_CHILD(myFrom)
IMPL_CLONE_CHILD(myTo)
IMPL_CLONE_END
IMPL_CLONE_REMAP_BEGIN(ArrowExpression, Expression)
IMPL_CLONE_REMAP(myFrom)
IMPL_CLONE_REMAP(myTo)
IMPL_CLONE_REMAP_END

SymRes ArrowExpression::resolveSymbols(Context& ctx)
{
    auto ret = ctx.resolveExpression(myFrom);
    if ( !ret )
        return ret;

    ret = ctx.resolveExpression(myTo);
    if ( !ret )
        return ret;

    myType = &Expression::universe(level(*this));
    return ret;
}

Expression const& ArrowExpression::from() const
{
    return *myFrom;
}

Expression& ArrowExpression::from()
{
    return *myFrom;
}

Slice<Expression const*> ArrowExpression::sliceFrom() const
{
    return sliceBox(myFrom);
}

Expression const& ArrowExpression::to() const
{
    return *myTo;
}

Expression& ArrowExpression::to()
{
    return *myTo;
}

Slice<Expression const*> ArrowExpression::sliceTo() const
{
    return sliceBox(myTo);
}

Box<Expression> ArrowExpression::takeFrom()
{
    return std::move(myFrom);
}

Box<Expression> ArrowExpression::takeTo()
{
    return std::move(myTo);
}

//
// UniverseExpression

UniverseExpression::UniverseExpression(Natural level)
    : Expression(Kind::Universe)
    , myLevel(level)
{
}

UniverseExpression::UniverseExpression(UniverseExpression const& rhs) = default;

UniverseExpression& UniverseExpression::operator = (UniverseExpression const& rhs)
{
    UniverseExpression(rhs).swap(*this);
    return *this;
}

UniverseExpression::~UniverseExpression() = default;

void UniverseExpression::swap(UniverseExpression& rhs) noexcept
{
    Expression::swap(rhs);
    using kyfoo::swap;
    swap(myLevel, rhs.myLevel);
}

IMPL_CLONE_BEGIN(UniverseExpression, Expression, Expression)
IMPL_CLONE_END
IMPL_CLONE_REMAP_BEGIN(UniverseExpression, Expression)
IMPL_CLONE_REMAP_END

SymRes UniverseExpression::resolveSymbols(Context&)
{
    // myType is generated lazily
    return SymRes::Success;
}

UniverseExpression::Natural UniverseExpression::level() const
{
    return myLevel;
}

//
// Utilities

lexer::SourceLocation getSourceLocation(Expression const& expr)
{
    return front(expr).location();
}

Expression const* createInferredType(Expression& expr, Declaration const& decl)
{
    auto e = createIdentifier(decl);
    auto ret = e.get();
    expr.appendConstraint(std::move(e));
    return ret;
}

bool hasDeclaration(Expression const& expr)
{
    switch (expr.kind()) {
    case Expression::Kind::Identifier:
    case Expression::Kind::Symbol:
    case Expression::Kind::Dot:
        return true;

    default:
        return false;
    }
}

IdentifierExpression* identify(Expression& expr)
{
    if ( auto id = expr.as<IdentifierExpression>() )
        return id;

    if ( auto sym = expr.as<SymbolExpression>() )
        return sym;

    if ( auto dot = expr.as<DotExpression>() )
        return identify(*dot->expressions().back());

    return nullptr;
}

IdentifierExpression const* identify(Expression const& expr)
{
    return identify(const_cast<Expression&>(expr));
}

Declaration const* getDeclaration(Expression const& expr)
{
    if ( auto id = identify(expr) )
        return id->declaration();

    return nullptr;
}

Declaration const* getDeclaration(Expression const* expr)
{
    if ( expr )
        return getDeclaration(*expr);

    return nullptr;
}

ab<Box<Expression>> flattenConstraints(Box<Expression> expr)
{
    auto ret = createPtrList<Expression>(std::move(expr));
    for ( uz i = 0; i != ret.card(); ++i ) {
        ret.appendRange(MoveRange(ret[i]->myConstraints()));
        ret[i]->myConstraints.clear();
    }

    return ret;
}

bool isUnit(Expression const& expr)
{
    auto tup = expr.as<TupleExpression>();
    return tup && tup->kind() == TupleKind::Open && !tup->expressions();
}

ProcedureDeclaration const* getProcedure(Expression const& expr)
{
    if ( auto decl = getDeclaration(expr) )
        return decl->as<ProcedureDeclaration>();

    if ( auto l = expr.as<LambdaExpression>() )
        return &l->procedure();

    return nullptr;
}

} // namespace kyfoo::ast
