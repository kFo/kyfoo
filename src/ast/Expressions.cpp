#include <kyfoo/ast/Expressions.hpp>

#include <algorithm>
#include <iterator>

#include <kyfoo/Diagnostics.hpp>

#include <kyfoo/ast/Axioms.hpp>
#include <kyfoo/ast/Context.hpp>
#include <kyfoo/ast/ControlFlow.hpp>
#include <kyfoo/ast/Declarations.hpp>
#include <kyfoo/ast/Fabrication.hpp>
#include <kyfoo/ast/Scopes.hpp>
#include <kyfoo/ast/Semantics.hpp>

#include <kyfoo/codegen/Codegen.hpp>

namespace kyfoo::ast {

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
        if ( exprs.empty() )
            return &universe(1);

        std::vector<Expression const*> types(exprs.size());
        for ( uz i = 0; i < types.size(); ++i )
            types[i] = exprs[i]->type();
        return &tuple(types);
    }

    return nullptr;
}

UniverseExpression const& Strata::universe(uz level)
{
    while ( myUniverses.size() <= level ) {
        auto u = new UniverseExpression(myUniverses.size());
        myUniverses.emplace_back(Box<UniverseExpression>(u));
    }

    return *myUniverses[level];
}

TupleExpression const& Strata::tuple(Slice<Expression const*> exprs)
{
    myTuples.emplace_back(mk<TupleExpression>(TupleKind::Open,
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

void Expression::swap(Expression& rhs)
{
    using std::swap;
    swap(myKind, rhs.myKind);
    swap(myConstraints, rhs.myConstraints);
    swap(myType, rhs.myType);
}

void Expression::io(IStream& /*stream*/) const
{
    // todo
}

void Expression::cloneChildren(Expression& c, clone_map_t& map) const
{
    IMPL_CLONE_CHILD(myConstraints)
}

IMPL_CLONE_REMAP_NOBASE_BEGIN(Expression)
IMPL_CLONE_REMAP(myConstraints)
IMPL_CLONE_REMAP(myType)
IMPL_CLONE_REMAP_END

void Expression::addConstraint(Box<Expression> expr)
{
    myConstraints.emplace_back(std::move(expr));
}

void Expression::addConstraints(std::vector<Box<Expression>>&& exprs)
{
    move(begin(exprs), end(exprs), back_inserter(myConstraints));
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
    addConstraint(std::move(type));
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

std::vector<Box<Expression>>&& Expression::takeConstraints()
{
    return std::move(myConstraints);
}

//
// LiteralExpression

LiteralExpression::LiteralExpression(lexer::Token const& token)
    : LiteralExpression(Expression::Kind::Literal, token)
{
}

LiteralExpression::LiteralExpression(Kind kind, lexer::Token const& token)
    : Expression(kind)
    , myToken(token)
{
}

LiteralExpression::LiteralExpression(LiteralExpression const& rhs)
    : Expression(rhs)
    , myToken(rhs.myToken)
{
}

LiteralExpression& LiteralExpression::operator = (LiteralExpression const& rhs)
{
    LiteralExpression(rhs).swap(*this);
    return *this;
}

LiteralExpression::~LiteralExpression() = default;

void LiteralExpression::swap(LiteralExpression& rhs)
{
    Expression::swap(rhs);
    
    using std::swap;
    swap(myToken, rhs.myToken);
}

void LiteralExpression::io(IStream& stream) const
{
    stream.next("literal", myToken);
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
        setType(*ctx.axioms().intrinsic(IntegerLiteralType));
        return SymRes::Success;
    }

    case lexer::TokenKind::Rational:
    {
        setType(*ctx.axioms().intrinsic(RationalLiteralType));
        return SymRes::Success;
    }

    case lexer::TokenKind::String:
    {
        ctx.module().interpretString(ctx.diagnostics(), myToken);
        setType(*ctx.axioms().intrinsic(Sliceu8));
        return SymRes::Success;
    }
    }

    throw std::runtime_error("unhandled literal-expression");
}

lexer::Token const& LiteralExpression::token() const
{
    return myToken;
}

//
// IdentifierExpression

IdentifierExpression::IdentifierExpression(lexer::Token const& token)
    : IdentifierExpression(Kind::Identifier, token, nullptr)
{
}

IdentifierExpression::IdentifierExpression(lexer::Token const& token, Declaration const& decl)
    : IdentifierExpression(Kind::Identifier, token, &decl)
{
}

IdentifierExpression::IdentifierExpression(Kind kind, lexer::Token const& token, Declaration const* decl)
    : Expression(kind)
    , myToken(token)
{
    if ( decl )
        setDeclaration(*decl);
}

IdentifierExpression::IdentifierExpression(IdentifierExpression const& rhs)
    : Expression(rhs)
    , myToken(rhs.myToken)
    , myDeclaration(rhs.myDeclaration)
{
}

IdentifierExpression& IdentifierExpression::operator = (IdentifierExpression const& rhs)
{
    IdentifierExpression(rhs).swap(*this);
    return *this;
}

IdentifierExpression::~IdentifierExpression() = default;

void IdentifierExpression::swap(IdentifierExpression& rhs)
{
    Expression::swap(rhs);

    using std::swap;
    swap(myToken, rhs.myToken);
    swap(myDeclaration, rhs.myDeclaration);
}

void IdentifierExpression::io(IStream& stream) const
{
    Expression::io(stream);
    // todo
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
            auto& err = ctx.error(*this) << "cannot be typed";
            err.see(*myDeclaration);
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
            ctx.error(token()) << "meta-variable not expected in this context";
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
            setType(*ctx.axioms().intrinsic(PointerNullLiteralType));
            return ensureType();
        }

        auto hit = ctx.matchOverload(token().lexeme());
        myDeclaration = hit.decl();
        if ( !myDeclaration ) {
            ctx.error(token()) << "undeclared identifier";
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

    throw std::runtime_error("unhandled identifier-expression");
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
        auto& err = ctx.error(*this) << "missing definition";
        err.see(*templ);
        return SymRes::Fail;
    }

    Resolver resolver(*defn, Resolver::Narrow);
    Context templateCtx(ctx.module(), ctx.diagnostics(), resolver);
    auto proc = templateCtx.matchOverload("").as<ProcedureDeclaration>();
    if ( !proc ) {
        ctx.error(*this) << "does not refer to any procedure";
        return SymRes::Fail;
    }

    myDeclaration = proc;
    myType = &proc->type()->to();
    return SymRes::Success;
}

void IdentifierExpression::setDeclaration(Declaration const& decl)
{
    if ( myDeclaration ) {
        switch ( resolveIndirections(myDeclaration)->kind() ) {
        case DeclKind::DataProduct:
        case DeclKind::Template:
            break;
        default:
            throw std::runtime_error("identifier resolved more than once");
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

    throw std::runtime_error("invalid tuple expression syntax");
}

const char* to_string(TupleKind kind)
{
    switch (kind) {
#define X(a) case TupleKind::a: return #a;
        TUPLE_KINDS(X)
#undef X
    }

    throw std::runtime_error("invalid tuple kind");
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
                                 std::vector<Box<Expression>>&& expressions)
    : Expression(Expression::Kind::Tuple)
    , myKind(kind)
    , myExpressions(std::move(expressions))
{
}

TupleExpression::TupleExpression(lexer::Token const& open,
                                 lexer::Token const& close,
                                 std::vector<Box<Expression>>&& expressions)
    : Expression(Expression::Kind::Tuple)
    , myKind(toTupleKind(open.kind(), close.kind()))
    , myExpressions(std::move(expressions))
    , myOpenToken(open)
    , myCloseToken(close)
{
}

TupleExpression::TupleExpression(std::vector<Box<Expression>>&& expressions,
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
{
}

TupleExpression& TupleExpression::operator = (TupleExpression const& rhs)
{
    TupleExpression(rhs).swap(*this);
    return *this;
}

void TupleExpression::swap(TupleExpression& rhs)
{
    Expression::swap(rhs);

    using std::swap;
    swap(myKind, rhs.myKind);
    swap(myExpressions, rhs.myExpressions);
}

TupleExpression::~TupleExpression() = default;

void TupleExpression::io(IStream& stream) const
{
    stream.openArray(to_string(myKind));
    for ( auto const& e : myExpressions )
        e->io(stream);
    stream.closeArray();
}

IMPL_CLONE_BEGIN(TupleExpression, Expression, Expression)
IMPL_CLONE_CHILD(myExpressions)
IMPL_CLONE_END
IMPL_CLONE_REMAP_BEGIN(TupleExpression, Expression)
IMPL_CLONE_REMAP(myExpressions)
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
            ctx.error(*myCardExpression) << "array cardinality must be an integer";
            return SymRes::Fail;
        }

        auto n = stoi(lit->token().lexeme());
        if ( n < 0 ) {
            ctx.error(*myCardExpression) << "array cardinality cannot be negative";
            return SymRes::Fail;
        }

        myCard = static_cast<uz>(n);
    }
    else {
        myCard = 1;
    }

    if ( myKind == TupleKind::Open ) {
        if ( myExpressions.size() == 1 )
            return ctx.rewrite(std::move(myExpressions[0]));
    }

    flattenOpenTuples();

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
void TupleExpression::flattenOpenTuples()
{
    for ( auto i = begin(myExpressions); i != end(myExpressions); ) {
        if ( (*i)->kind() == Expression::Kind::Tuple ) {
            auto tuple = static_cast<TupleExpression*>(i->get());
            if ( tuple->kind() == TupleKind::Open && tuple->myCard == 1 ) {
                auto index = distance(begin(myExpressions), i) + tuple->myExpressions.size();
                move(begin(tuple->myExpressions), end(tuple->myExpressions),
                     std::inserter(myExpressions, i));
                i = next(begin(myExpressions), index);
                goto L_removeItem;
            }
        }

        ++i;
        continue;

    L_removeItem:
        i = myExpressions.erase(i);
    }
}

//
// ApplyExpression

ApplyExpression::ApplyExpression(std::vector<Box<Expression>>&& expressions)
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

void ApplyExpression::swap(ApplyExpression& rhs)
{
    Expression::swap(rhs);

    using std::swap;
    swap(myExpressions, rhs.myExpressions);
}

void ApplyExpression::io(IStream& stream) const
{
    stream.next("expressions", myExpressions);
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
        if ( myExpressions.empty() )
            return ctx.rewrite(mk<TupleExpression>(TupleKind::Open, std::move(myExpressions)));
        else if ( myExpressions.size() == 1 )
            return ctx.rewrite(std::move(myExpressions.front()));

        return SymRes::Success;
    };

    auto ret = tryLowerSingle();
    if ( !ret )
        return ret;

    ret |= ctx.resolveExpressions(next(begin(myExpressions)), end(myExpressions));
    if ( !ret )
        return ret;

    flatten(next(begin(myExpressions)));
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
        if ( !id->token().lexeme().empty() ) {
            auto hit = ctx.matchOverload(id->token().lexeme());
            if ( !hit )
                return ctx.rewrite(mk<SymbolExpression>(std::move(myExpressions)));
        }
    }

    ret |= ctx.resolveExpression(myExpressions.front());
    if ( !ret )
        return ret;

    if ( auto dot = subject()->as<DotExpression>() ) {
        auto decl = getDeclaration(dot->top());
        if ( decl && (decl->kind() == DeclKind::Procedure || decl->kind() == DeclKind::Template) ) {
            auto thisDecl = getDeclaration(dot->top(1));
            if ( !thisDecl || !isDefinableDeclaration(thisDecl->kind()) ) {
                myExpressions.emplace(begin(myExpressions), dot->takeTop());
                goto L_restart;
            }
        }
    }

    auto const subjType = myExpressions.front()->type();
    switch ( subjType->kind() ) {
    case Kind::Tuple:
        return elaborateTuple(ctx);

    case Kind::Arrow:
        myProc = getProcedure(*myExpressions.front());
        break;

    case Kind::Universe:
        if ( subjType->as<UniverseExpression>()->level() == 0 )
            ret |= lowerToStaticCall(ctx);
        else
            ret |= lowerToConstruction(ctx);
        break;

    default:
        ret |= lowerToApplicable(ctx);
    }

    if ( !ret ) {
        ctx.error(*myExpressions.front()) << "is not applicable";
        return ret;
    }

    auto arrow = subjType->as<ArrowExpression>();
    if ( myProc )
        arrow = myProc->type();

    if ( !arrow )
        throw std::runtime_error("proc is not typed");

    if ( !variance(ctx, arrow->from(), arguments()) ) {
        (ctx.error(*this) << "types do not agree")
            .expectedTypes(ctx.resolver().scope(), arrow->sliceFrom())
            .received(ctx.resolver().scope(), arguments());
        return SymRes::Fail;
    }

    myType = &arrow->to();

    if ( !ctx.isTopLevel() ) {
        return ctx.rewrite([&ctx](Box<Expression>& expr) {
            // todo: explicit proc scope knowledge
            auto& procScope = static_cast<ProcedureScope&>(ctx.resolver().scope());
            return ctx.statement().appendUnnamedExpression(procScope, std::move(expr));
        });
    }

    return ret;
}

SymRes ApplyExpression::lowerToApplicable(Context& ctx)
{
    auto const& subj = *subject();
    auto applicable = resolveIndirections(getDeclaration(subj.type()));
    if ( !applicable ) {
        ctx.error(subj) << "does not have an applicable type";
        return SymRes::Fail;
    }

    while ( auto ds = applicable->as<DataSumDeclaration>() ) {
        if ( isReference(*ds) ) {
            applicable = resolveIndirections(getDeclaration(ds->symbol().prototype().pattern().front()));
            if ( !applicable ) {
                ctx.error(subj) << "does not refer to an applicable type";
                return SymRes::Fail;
            }
        }
    }

    auto dp = applicable->as<DataProductDeclaration>();
    if ( !dp ) {
        ctx.error(subj) << "only data-product types can be applied";
        return SymRes::Fail;
    }

    auto defn = dp->definition();
    if ( !defn ) {
        (ctx.error(subj) << "missing definition")
            .see(*dp);
        return SymRes::Fail;
    }

    Resolver resolver(*defn, Resolver::Narrow);
    Context dpCtx(ctx.module(), ctx.diagnostics(), resolver);
    auto hit = dpCtx.matchOverload(SymbolReference("", expressions()));
    if ( !hit ) {
        (ctx.error(subj) << "no suitable apply overload found")
            .see(*dp);
        return SymRes::Fail;
    }

    auto proc = hit.as<ProcedureDeclaration>();
    if ( !proc ) {
        (ctx.error(subj) << "is not a procedure")
            .see(*hit.decl())
            .see(*dp);
        return SymRes::Fail;
    }

    myProc = proc;
    myExpressions.emplace(begin(myExpressions), createIdentifier(*myProc));

    return SymRes::Success;
}

SymRes ApplyExpression::lowerToStaticCall(Context& ctx)
{
    auto const& subj = *subject();
    auto decl = resolveIndirections(getDeclaration(subj));
    if ( !decl ) {
        ctx.error(subj) << "does not refer to a declaration";
        return SymRes::Fail;
    }

    auto templ = decl->as<TemplateDeclaration>();
    if ( !templ ) {
        (ctx.error(subj) << "does not refer to a template-declaration")
            .see(*decl);
        return SymRes::Fail;
    }

    auto defn = templ->definition();
    if ( !defn ) {
        ctx.error(*templ) << "missing definition";
        return SymRes::Fail;
    }

    Resolver resolver(*defn, Resolver::Narrow);
    Context templCtx(ctx.module(), ctx.diagnostics(), resolver);
    auto hit = templCtx.matchOverload(SymbolReference("", arguments()));
    auto proc = hit.as<ProcedureDeclaration>();
    if ( !proc ) {
        auto& err = ctx.error(*this) << "does not match any procedure overload";
        if ( hit.symSpace() ) {
            for ( auto const& p : hit.symSpace()->prototypes() )
                err.see(*p.proto.decl);
        }
        return SymRes::Fail;
    }

    myProc = proc;
    return SymRes::Success;
}

SymRes ApplyExpression::lowerToConstruction(Context& ctx)
{
    auto const& subj = *subject();
    auto decl = resolveIndirections(getDeclaration(subj));
    if ( !decl ) {
        ctx.error(subj) << "does not refer to a declaration";
        return SymRes::Fail;
    }

    if ( auto dp = decl->as<DataProductDeclaration>() ) {
        auto defn = dp->definition();
        if ( !defn ) {
            (ctx.error(subj) << "missing data-product definition")
                .see(*dp);
            return SymRes::Fail;
        }

        Resolver resolver(*defn, Resolver::Narrow);
        Context dpCtx(ctx.module(), ctx.diagnostics(), resolver);
        auto hit = dpCtx.matchOverload("ctor");
        if ( !hit ) {
            (ctx.error(subj) << "data-product is not constructible")
                .see(*dp);
            return SymRes::Fail;
        }

        auto ctorTemplDecl = hit.as<TemplateDeclaration>();
        if ( !ctorTemplDecl )
            throw std::runtime_error("ctor is not a template");

        Resolver templScope(*ctorTemplDecl->definition(), Resolver::Narrow);
        dpCtx.changeResolver(templScope);
        hit = dpCtx.matchOverload(SymbolReference("", arguments()));
        auto proc = hit.as<ProcedureDeclaration>();
        if ( !proc )
            throw std::runtime_error("ctor is not a procedure");

        myProc = proc;
        return SymRes::Success;
    }

    if ( auto dsCtor = decl->as<DataSumDeclaration::Constructor>() ) {
        ctx.error(*myExpressions.front()) << "ds ctor not implemented";
        return SymRes::Fail;
    }

    return SymRes::Success;
}

SymRes ApplyExpression::elaborateTuple(Context& ctx)
{
    auto subjectType = resolveIndirections(*myExpressions.front())->type()->as<TupleExpression>();
    if ( !subjectType ) {
        ctx.error(*myExpressions.front()) << "is not a tuple";
        return SymRes::Fail;
    }

    if ( subjectType->kind() != TupleKind::Closed ) {
        ctx.error(*subject()) << "non-closed tuple application not implemented";
        return SymRes::Fail;
    }

    if ( myExpressions.size() != 2 ) {
        ctx.error(*subject()) << "too many arguments to tuple";
        return SymRes::Fail;
    }

    if ( auto lit = myExpressions[1]->as<LiteralExpression>() ) {
        if ( lit->token().kind() != lexer::TokenKind::Integer ) {
            ctx.error(*lit) << "expected integer for index";
            return SymRes::Fail;
        }

        return ctx.rewrite(mk<DotExpression>(false, std::move(myExpressions)));
    }

    if ( subjectType->elementsCount() <= 1 ) {
        ctx.error(*subject()) << "index only implemented for arrays";
        return SymRes::Fail;
    }

    auto argType = myExpressions[1]->type();
    auto elementType = subjectType->expressions().front();

    if ( auto arrow = argType->as<ArrowExpression>() ) {
        Box<IdentifierExpression> refStorage;
        auto refElementType = elementType;
        if ( !isReference(*refElementType) ) {
            refStorage = createIdentifier(*ctx.matchOverload(SymbolReference("ref", slice(elementType))).decl());
            refElementType = refStorage.get();
        }

        if ( !variance(ctx, arrow->from(), *refElementType) ) {
            (ctx.error(*myExpressions[1]) << "cannot be applied to " << *myExpressions[0])
                .expectedTypes(ctx.resolver().scope(), subjectType->expressions()(0, 1))
                .receivedTypes(ctx.resolver().scope(), arrow->sliceFrom());
            return SymRes::Fail;
        }

        myType = elementType;
        return SymRes::Success;
    }

    auto indexType = createIdentifier(*ctx.axioms().intrinsic(size_t));
    if ( !variance(ctx, *indexType, *argType) ) {
        ctx.error(*myExpressions[1]) << "cannot be converted to size_t";
        return SymRes::Fail;
    }

    if ( subjectType->expressions().size() == 1 )
        myType = subjectType->expressions().front();
    else
        myType = &Expression::tuple(subjectType->expressions());

    return SymRes::Success;
}

/**
 * Flatten apply-expressions that don't start with a procedure identifier
 */
void ApplyExpression::flatten()
{
    return flatten(begin(myExpressions));
}

void ApplyExpression::flatten(std::vector<Box<Expression>>::iterator first)
{
    for ( auto i = first; i != end(myExpressions); ) {
        if ( auto tuple = (*i)->as<TupleExpression>() ) {
            if ( tuple->kind() == TupleKind::Open ) {
                auto index = distance(begin(myExpressions), i) + tuple->myExpressions.size();
                move(begin(tuple->myExpressions), end(tuple->myExpressions),
                     std::inserter(myExpressions, i));
                i = next(begin(myExpressions), index);
                goto L_removeItem;
            }
        }

        ++i;
        continue;

    L_removeItem:
        i = myExpressions.erase(i);
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
    return slice(myExpressions, 1);
}

Slice<Expression const*> ApplyExpression::arguments() const
{
    return slice(myExpressions, 1);
}

ProcedureDeclaration const* ApplyExpression::procedure() const
{
    return myProc;
}

//
// SymbolExpression

SymbolExpression::SymbolExpression(lexer::Token const& token,
                                   std::vector<Box<Expression>>&& expressions)
    : IdentifierExpression(Expression::Kind::Symbol, token, nullptr)
    , myExpressions(std::move(expressions))
{
}

SymbolExpression::SymbolExpression(std::vector<Box<Expression>>&& expressions)
    : IdentifierExpression(Expression::Kind::Symbol, lexer::Token(), nullptr)
    , myExpressions(std::move(expressions))
{
}

SymbolExpression::SymbolExpression(lexer::Token const& open,
                                   lexer::Token const& close,
                                   std::vector<Box<Expression>>&& expressions)
    : IdentifierExpression(Expression::Kind::Symbol, lexer::Token(), nullptr)
    , myExpressions(std::move(expressions))
    , myOpenToken(open)
    , myCloseToken(close)
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

void SymbolExpression::swap(SymbolExpression& rhs)
{
    IdentifierExpression::swap(rhs);

    using std::swap;
    swap(myExpressions, rhs.myExpressions);
    swap(myOpenToken, rhs.myOpenToken);
    swap(myCloseToken, rhs.myCloseToken);
}

void SymbolExpression::io(IStream& stream) const
{
    IdentifierExpression::io(stream);
    stream.next("expressions", myExpressions);
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
        if ( myExpressions.empty() )
            return SymRes::Success;

        auto subject = myExpressions.front()->as<IdentifierExpression>();
        if ( !subject ) {
            ctx.error(*this) << "symbol tuples must start with an identifier";
            return SymRes::Fail;
        }

        auto subjectExpression = std::move(myExpressions.front());
        setToken(subject->token());
        move(next(begin(myExpressions)), end(myExpressions),
             begin(myExpressions));
        myExpressions.pop_back();

        if ( myExpressions.empty() )
            return ctx.rewrite(std::move(subjectExpression));
    }

    auto ret = ctx.resolveExpressions(myExpressions);
    if ( !ret )
        return ret;

    SymbolReference sym(token().lexeme(), myExpressions);
    auto hit = ctx.matchOverload(sym);
    if ( !hit ) {
        ctx.error(*this) << "undeclared symbol identifier";
        return SymRes::Fail;
    }

    setDeclaration(*hit.decl());
    return SymRes::Success;
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

std::vector<Box<Expression>>& SymbolExpression::internalExpressions()
{
    return myExpressions;
}

//
// DotExpression

DotExpression::DotExpression(bool modScope,
                             std::vector<Box<Expression>>&& exprs)
    : Expression(Kind::Dot)
    , myExpressions(std::move(exprs))
    , myModScope(modScope)
{
}

DotExpression::DotExpression(DotExpression const& rhs)
    : Expression(rhs)
{
    // myFirst, mySecond cloned
}

DotExpression& DotExpression::operator = (DotExpression const& rhs)
{
    DotExpression(rhs).swap(*this);
    return *this;
}

DotExpression::~DotExpression()
{
}

void DotExpression::swap(DotExpression& rhs)
{
    Expression::swap(rhs);
    using std::swap;
    swap(myExpressions, rhs.myExpressions);
}

void DotExpression::io(IStream& stream) const
{
    Expression::io(stream);
    stream.next("expression", myExpressions);
}

IMPL_CLONE_BEGIN(DotExpression, Expression, Expression)
IMPL_CLONE_CHILD(myExpressions)
IMPL_CLONE_END
IMPL_CLONE_REMAP_BEGIN(DotExpression, Expression)
IMPL_CLONE_REMAP(myExpressions)
IMPL_CLONE_REMAP_END

SymRes DotExpression::resolveSymbols(Context& ctx)
{
    if ( myType )
        return SymRes::Success;

    Resolver resolver(*ctx.module().scope());
    if ( isModuleScope() )
        ctx.changeResolver(resolver);

    auto ret = ctx.resolveExpression(myExpressions.front());
    if ( !ret )
        return ret;

    auto updateContext = [&ctx, &resolver](Expression const& expr)
    {
        auto decl = resolveIndirections(getDeclaration(expr));
        if ( !decl )
            return;

        if ( auto scope = memberScope(*decl) ) {
            resolver = Resolver(*scope);
            ctx.changeResolver(resolver);
        }
    };

    updateContext(*myExpressions.front());

    for ( uz i = 1; i < myExpressions.size(); ++i ) {
        ret = ctx.resolveExpression(myExpressions[i]);
        if ( !ret )
            return ret;

        auto e = resolveIndirections(myExpressions[i].get());

        if ( auto lit = e->as<LiteralExpression>() ) {
            if ( lit->token().kind() != lexer::TokenKind::Integer ) {
                ctx.error(*e) << "cannot be an accessor";
                return SymRes::Fail;
            }

            int index = stoi(lit->token().lexeme());
            if ( index < 0 ) {
                ctx.error(*e) << "field accessor cannot be negative";
                return SymRes::Fail;
            }

            auto const* composite = resolveIndirections(myExpressions[i - 1].get());
            bool binder = false;
            if ( auto decl = getDeclaration(*composite) ) {
                if ( auto b = getBinder(*decl) ) {
                    composite = resolveIndirections(b->type());
                    binder = true;
                }
            }

            if ( auto decl = getDeclaration(*composite) ) {
                auto dp = decl->as<DataProductDeclaration>();
                if ( !dp ) {
                    (ctx.error(*e) << "field accessor must refer to composite type")
                        .see(*dp);
                    return SymRes::Fail;
                }

                auto defn = dp->definition();
                if ( !defn ) {
                    (ctx.error(*composite) << "is not defined")
                        .see(*dp);
                    return SymRes::Fail;
                }

                if ( index >= defn->fields().size() ) {
                    (ctx.error(*e) << "field accessor out of bounds")
                        .see(*dp);
                    return SymRes::Fail;
                }

                auto const tok = lit->token();
                myExpressions[i] = createIdentifier(makeToken(tok.lexeme(),
                                                              tok.line(),
                                                              tok.column()),
                                                    *defn->fields()[index]);
                e = myExpressions[i].get();
            }
            else if ( auto tup = composite->as<TupleExpression>() ) {
                if ( index >= tup->elementsCount() ) {
                    ctx.error(*e) << "field index out of bounds";
                    return SymRes::Fail;
                }

                if ( binder )
                    lit->setType(tup->elements()[index]);
                else
                    lit->setType(tup->elements()[index]->type());
            }
            else {
                ctx.error(*e) << "cannot be indexed";
                return SymRes::Fail;
            }
        }

        updateContext(*myExpressions[i]);
    }

    if ( myExpressions.empty() )
        return ctx.rewrite(createEmptyExpression());
    else if ( myExpressions.size() == 1 )
        return ctx.rewrite(std::move(myExpressions.front()));

    setType(myExpressions.back()->type());

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

Expression const* DotExpression::top(uz index) const
{
    if ( index >= myExpressions.size() )
        return nullptr;

    return myExpressions[myExpressions.size() - index - 1].get();
}

bool DotExpression::isModuleScope() const
{
    return myModScope;
}

Box<Expression> DotExpression::takeTop(uz index)
{
    auto const takeIndex = myExpressions.size() - 1 - index;
    auto m = begin(myExpressions) + takeIndex;
    auto ret = std::move(*m);
    move(next(m), end(myExpressions), m);
    myExpressions.pop_back();

    myType = nullptr;

    return ret;
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

void AssignExpression::swap(AssignExpression& rhs)
{
    Expression::swap(rhs);
    using std::swap;
    swap(myLeft, rhs.myLeft);
    swap(myRight, rhs.myRight);
}

void AssignExpression::io(IStream& stream) const
{
    stream.next("left", myLeft);
    stream.next("right", myRight);
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

    auto ret = ctx.resolveExpression(myLeft);
    if ( !ret )
        return ret;

    ret = ctx.resolveExpression(myRight);
    if ( !ret )
        return ret;

    VarianceResult result = Invariant;
    if ( isReference(*myLeft->type()) )
        result = variance(ctx, getDeclaration(myLeft->type())->symbol().prototype().pattern(), *myRight->type());
    else
        result = variance(ctx, *myLeft, *myRight);

    if ( !result ) {
        ctx.error(*this) << "assignment expression type does not match variable type";
        return SymRes::Fail;
    }

    myType = myLeft->type();
    return SymRes::Success;
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

//
// LambdaExpression

LambdaExpression::LambdaExpression(lexer::Token const& yieldToken,
                                   ProcedureDeclaration* proc)
    : Expression(Kind::Lambda)
    , myYieldToken(yieldToken)
    , myProc(proc)
{
}

LambdaExpression::LambdaExpression(LambdaExpression const& rhs)
    : Expression(rhs)
{
}

LambdaExpression& LambdaExpression::operator = (LambdaExpression const& rhs)
{
    LambdaExpression(rhs).swap(*this);
    return *this;
}

LambdaExpression::~LambdaExpression() = default;

void LambdaExpression::swap(LambdaExpression& rhs)
{
    Expression::swap(rhs);
    using std::swap;
    swap(myProc, rhs.myProc);
}

void LambdaExpression::io(IStream&) const
{
    // todo
}

IMPL_CLONE_BEGIN(LambdaExpression, Expression, Expression)
IMPL_CLONE_END
IMPL_CLONE_REMAP_BEGIN(LambdaExpression, Expression)
IMPL_CLONE_REMAP(myProc)
IMPL_CLONE_REMAP_END

SymRes LambdaExpression::resolveSymbols(Context& /*ctx*/)
{
    if ( !myProc->definition() )
        throw std::runtime_error("lambda missing body");

    myType = myProc->type();
    if ( !myType )
        return SymRes::Fail;

    return SymRes::Success;
}

lexer::Token const& LambdaExpression::yieldToken() const
{
    return myYieldToken;
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

void ArrowExpression::swap(ArrowExpression& rhs)
{
    Expression::swap(rhs);
    using std::swap;
    swap(myFrom, rhs.myFrom);
    swap(myTo, rhs.myTo);
}

void ArrowExpression::io(IStream& stream) const
{
    stream.next("from", myFrom);
    stream.next("to", myTo);
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

//
// UniverseExpression

UniverseExpression::UniverseExpression(natural_t level)
    : Expression(Kind::Universe)
    , myLevel(level)
{
}

UniverseExpression::UniverseExpression(UniverseExpression const& rhs)
    : Expression(rhs)
    , myLevel(rhs.myLevel)
{
}

UniverseExpression& UniverseExpression::operator = (UniverseExpression const& rhs)
{
    UniverseExpression(rhs).swap(*this);
    return *this;
}

UniverseExpression::~UniverseExpression() = default;

void UniverseExpression::swap(UniverseExpression& rhs)
{
    Expression::swap(rhs);
    using std::swap;
    swap(myLevel, rhs.myLevel);
}

void UniverseExpression::io(IStream& stream) const
{
    Expression::io(stream);
    stream.next("level", myLevel);
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

UniverseExpression::natural_t UniverseExpression::level() const
{
    return myLevel;
}

//
// Utilities

std::ostream& operator << (std::ostream& stream, Expression const& expr)
{
    return print(stream, expr);
}

std::ostream& operator << (std::ostream& stream, Slice<Expression const*> exprs)
{
    if ( exprs )
        print(stream, *exprs.front());

    for ( auto e : exprs(1, $) ) {
        stream << ", ";
        print(stream, *e);
    }

    return stream;
}

std::ostream& operator << (std::ostream& stream, Slice<Expression*> exprs)
{
    return operator << (stream, static_cast<Slice<Expression const*>>(exprs));
}

std::ostream& operator << (std::ostream& stream, get_types&& types)
{
    if ( types.exprs )
        print(stream, *types.exprs.front()->type());

    for ( auto e : types.exprs(1, $) ) {
        stream << ", ";
        print(stream, *e->type());
    }

    return stream;
}

Expression const* createInferredType(Expression& expr, Declaration const& decl)
{
    auto e = createIdentifier(decl);
    auto ret = e.get();
    expr.addConstraint(std::move(e));
    return ret;
}

bool hasDeclaration(Expression const& expr)
{
    switch (expr.kind()) {
    case Expression::Kind::Identifier:
    case Expression::Kind::Symbol:
    case Expression::Kind::Dot:
        return true;
    }

    return false;
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

std::vector<Box<Expression>> flattenConstraints(Box<Expression> expr)
{
    auto ret = createPtrList<Expression>(std::move(expr));
    for ( uz i = 0; i != ret.size(); ++i ) {
        std::move(begin(ret[i]->myConstraints), end(ret[i]->myConstraints),
                  std::back_inserter(ret));
        ret[i]->myConstraints.resize(0);
    }

    return ret;
}

DeclRef getRef(Expression const& expr_)
{
    auto expr = resolveIndirections(&expr_);
    if ( auto dot = expr->as<DotExpression>() ) {
        uz i = 0;
        auto e = resolveIndirections(dot->top(i));
        for ( ; e; e = resolveIndirections(dot->top(++i)) ) {
            if ( auto decl = getDeclaration(e) ) {
                if ( auto var = decl->as<VariableDeclaration>() )
                    return {var, dot->type()};

                if ( auto field = decl->as<DataProductDeclaration::Field>() )
                    continue;
            }

            if ( auto lit = e->as<LiteralExpression>() ) {
                if ( lit->token().kind() == lexer::TokenKind::Integer )
                    continue;
            }

            break;
        }

        return {nullptr, nullptr};
    }

    if ( auto decl = getDeclaration(expr) ) {
        if ( auto var = decl->as<VariableDeclaration>() )
            return {var, var->type()};
    }

    if ( auto app = expr->as<ApplyExpression>() ) {
        if ( app->subject()->type()->kind() == Expression::Kind::Tuple )
            if ( auto decl = getDeclaration(app->subject()) )
                if ( auto var = decl->as<VariableDeclaration>() )
                    return {var, app->type()};

        return {nullptr, nullptr};
    }

    return {nullptr, nullptr};
}

Expression const* getRefType(Expression const& expr)
{
    return getRef(expr).type;
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
