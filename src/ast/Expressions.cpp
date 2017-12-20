#include <kyfoo/ast/Expressions.hpp>

#include <algorithm>
#include <iterator>

#include <kyfoo/Diagnostics.hpp>

#include <kyfoo/ast/Axioms.hpp>
#include <kyfoo/ast/Context.hpp>
#include <kyfoo/ast/ControlFlow.hpp>
#include <kyfoo/ast/Declarations.hpp>
#include <kyfoo/ast/Scopes.hpp>
#include <kyfoo/ast/Semantics.hpp>

#include <kyfoo/codegen/Codegen.hpp>

namespace kyfoo {
    namespace ast {

//
// Expression

Expression::Expression(Kind kind)
    : myKind(kind)
{
}

Expression::Expression(Kind kind, Declaration const* decl)
    : myKind(kind)
    , myDeclaration(decl)
{
}

Expression::Expression(Expression const& rhs)
    : myKind(rhs.myKind)
    , myConstraints(ast::clone(rhs.myConstraints))
    , myDeclaration(rhs.myDeclaration)
{
}

Expression::~Expression() = default;

void Expression::swap(Expression& rhs)
{
    using std::swap;
    swap(myKind, rhs.myKind);
    swap(myConstraints, rhs.myConstraints);
    swap(myDeclaration, rhs.myDeclaration);
}

void Expression::cloneChildren(Expression& c, clone_map_t& map) const
{
    IMPL_CLONE_CHILD(myConstraints)
}

IMPL_CLONE_REMAP_NOBASE_BEGIN(Expression)
IMPL_CLONE_REMAP(myConstraints)
IMPL_CLONE_REMAP(myDeclaration)
IMPL_CLONE_REMAP_END

void Expression::addConstraint(std::unique_ptr<Expression> expr)
{
    myConstraints.emplace_back(std::move(expr));
}

Expression::Kind Expression::kind() const
{
    return myKind;
}

Declaration const* Expression::declaration() const
{
    return myDeclaration;
}

void Expression::setDeclaration(Declaration const& decl)
{
    myDeclaration = &decl;
}

void Expression::clearDeclaration()
{
    myDeclaration = nullptr;
}

Slice<Expression*> Expression::constraints()
{
    return myConstraints;
}

const Slice<Expression*> Expression::constraints() const
{
    return myConstraints;
}

//
// PrimaryExpression

PrimaryExpression::PrimaryExpression(lexer::Token const& token)
    : PrimaryExpression(Expression::Kind::Primary, token)
{
}

PrimaryExpression::PrimaryExpression(Kind kind, lexer::Token const& token)
    : Expression(kind)
    , myToken(token)
{
}

PrimaryExpression::PrimaryExpression(PrimaryExpression const& rhs)
    : Expression(rhs)
    , myToken(rhs.myToken)
{
}

PrimaryExpression& PrimaryExpression::operator = (PrimaryExpression const& rhs)
{
    PrimaryExpression(rhs).swap(*this);
    return *this;
}

PrimaryExpression::~PrimaryExpression() = default;

void PrimaryExpression::swap(PrimaryExpression& rhs)
{
    Expression::swap(rhs);
    
    using std::swap;
    swap(myToken, rhs.myToken);
}

void PrimaryExpression::io(IStream& stream) const
{
    stream.next("primary", myToken);
}

IMPL_CLONE_BEGIN(PrimaryExpression, Expression, Expression)
IMPL_CLONE_END
IMPL_CLONE_REMAP_BEGIN(PrimaryExpression, Expression)
IMPL_CLONE_REMAP_END

SymRes PrimaryExpression::resolveSymbols(Context& ctx)
{
    if ( myDeclaration ) {
        if ( auto symVar = myDeclaration->as<SymbolVariable>() )
            if ( !symVar->boundExpression() )
                return SymRes::NeedsSubstitution;

        return SymRes::Success;
    }

    switch ( myToken.kind() ) {
    case lexer::TokenKind::Integer:
    {
        myDeclaration = ctx.axioms().intrinsic(IntegerLiteralType);
        return SymRes::Success;
    }

    case lexer::TokenKind::Rational:
    {
        myDeclaration = ctx.axioms().intrinsic(RationalLiteralType);
        return SymRes::Success;
    }

    case lexer::TokenKind::String:
    {
        ctx.module().interpretString(ctx.diagnostics(), myToken);
        myDeclaration = ctx.axioms().intrinsic(StringLiteralType);
        return SymRes::Success;
    }

    case lexer::TokenKind::MetaVariable:
    {
        if ( !myDeclaration ) {
            ctx.error(myToken) << "meta variable not expected in this context";
            return SymRes::Fail;
        }

        if ( auto symVar = myDeclaration->as<SymbolVariable>() ) {
            if ( symVar->boundExpression() )
                return SymRes::Success;
        }
        else {
            ctx.error(myToken) << "must be bound to a symbol-variable";
            return SymRes::Fail;
        }

        return SymRes::NeedsSubstitution;
    }

    case lexer::TokenKind::Identifier:
    {
        // todo: removeme
        if ( myToken.lexeme() == "null" ) {
            myDeclaration = ctx.axioms().intrinsic(PointerNullLiteralType);
            return SymRes::Success;
        }

        auto hit = ctx.matchOverload(SymbolReference(myToken.lexeme()));
        if ( !hit ) {
            ctx.error(myToken) << "undeclared identifier";
            return SymRes::Fail;
        }

        if ( hit.as<TemplateDeclaration>() ) {
            ctx.error(myToken) << "does not refer to any template invocation";
            return SymRes::Fail;
        }
        else if ( hit.as<ProcedureDeclaration>() ) {
            throw std::runtime_error("primary-expression cannot refer to a procedure");
        }

        myDeclaration = hit.decl();

        if ( auto symVar = myDeclaration->as<SymbolVariable>() )
            if ( !symVar->boundExpression() )
                return SymRes::NeedsSubstitution;

        return SymRes::Success;
    }

    default:
        throw std::runtime_error("unhandled primary expression");
    }
}

lexer::Token const& PrimaryExpression::token() const
{
    return myToken;
}

void PrimaryExpression::setMetaVariable(Declaration const* decl)
{
    if ( myDeclaration )
        throw std::runtime_error("meta variable can only be bound once");

    myDeclaration = decl;
}

//
// ReferenceExpression

ReferenceExpression::ReferenceExpression(std::unique_ptr<Expression> expression)
    : Expression(Expression::Kind::Reference)
    , myExpression(std::move(expression))
{
}

ReferenceExpression::ReferenceExpression(ReferenceExpression const& rhs)
    : Expression(rhs)
{
}

ReferenceExpression& ReferenceExpression::operator = (ReferenceExpression const& rhs)
{
    ReferenceExpression(rhs).swap(*this);
    return *this;
}

ReferenceExpression::~ReferenceExpression() = default;

void ReferenceExpression::swap(ReferenceExpression& rhs)
{
    Expression::swap(rhs);
    using std::swap;
    swap(myExpression, rhs.myExpression);
}

void ReferenceExpression::io(IStream& stream) const
{
    stream.next("expr", myExpression);
}

IMPL_CLONE_BEGIN(ReferenceExpression, Expression, Expression)
IMPL_CLONE_CHILD(myExpression)
IMPL_CLONE_END
IMPL_CLONE_REMAP_BEGIN(ReferenceExpression, Expression)
IMPL_CLONE_REMAP(myExpression)
IMPL_CLONE_REMAP_END

SymRes ReferenceExpression::resolveSymbols(Context& ctx)
{
    if ( myDeclaration )
        return SymRes::Success;

    auto ret = ctx.resolveExpression(myExpression);
    if ( !ret )
        return ret;

    if ( !isMemoryDeclaration(myExpression->declaration()->kind()) ) {
        ctx.error(*this) << "reference-expressions can only apply to expressions with storage";
        return SymRes::Fail;
    }

    myDeclaration = myExpression->declaration();
    return myDeclaration ? SymRes::Success : SymRes::Fail;
}

Expression const& ReferenceExpression::expression() const
{
    return *myExpression;
}

Expression& ReferenceExpression::expression()
{
    return *myExpression;
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
                                 std::vector<std::unique_ptr<Expression>>&& expressions)
    : Expression(Expression::Kind::Tuple)
    , myKind(kind)
    , myExpressions(std::move(expressions))
{
}

TupleExpression::TupleExpression(lexer::Token const& open,
                                 lexer::Token const& close,
                                 std::vector<std::unique_ptr<Expression>>&& expressions)
    : Expression(Expression::Kind::Tuple)
    , myKind(toTupleKind(open.kind(), close.kind()))
    , myExpressions(std::move(expressions))
    , myOpenToken(open)
    , myCloseToken(close)
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
    flattenOpenTuples();

    if ( myKind == TupleKind::Open ) {
        if ( myExpressions.empty() ) {
            myDeclaration = ctx.axioms().intrinsic(EmptyLiteralType);
            return SymRes::Success;
        }
        else if ( myExpressions.size() == 1 ) {
            return ctx.rewrite(std::move(myExpressions[0]));
        }
    }

    auto ret = ctx.resolveExpressions(myExpressions);
    if ( !ret )
        return ret;

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

Slice<Expression*> TupleExpression::expressions() const
{
    return myExpressions;
}

Slice<Expression*> TupleExpression::expressions()
{
    return myExpressions;
}

/**
 * Flatten open-tuples
 */
void TupleExpression::flattenOpenTuples()
{
    for ( auto i = begin(myExpressions); i != end(myExpressions); ) {
        if ( (*i)->kind() == Expression::Kind::Tuple ) {
            auto tuple = static_cast<TupleExpression*>(i->get());
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

//
// ApplyExpression

ApplyExpression::ApplyExpression(std::vector<std::unique_ptr<Expression>>&& expressions)
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
    if ( myDeclaration )
        return SymRes::Success;

    flatten(next(begin(myExpressions)));

    if ( myExpressions.empty() )
        return ctx.rewrite(std::make_unique<TupleExpression>(TupleKind::Open, std::move(myExpressions)));
    else if ( myExpressions.size() == 1 )
        return ctx.rewrite(std::move(myExpressions.front()));

    auto ret = ctx.resolveExpressions(next(begin(myExpressions)), end(myExpressions));
    if ( !ret )
        return ret;

    auto subject = myExpressions.front()->as<PrimaryExpression>();
    if ( subject ) {
        if ( subject->token().kind() != lexer::TokenKind::Identifier )
            return ctx.rewrite(std::make_unique<TupleExpression>(TupleKind::Open, std::move(myExpressions)));

        auto hit = ctx.matchOverload(SymbolReference(subject->token().lexeme()));
        if ( !hit )
            return ctx.rewrite(std::make_unique<SymbolExpression>(std::move(myExpressions)));

        subject->setDeclaration(*hit.decl());
    }

    ret = ctx.resolveExpression(myExpressions.front());
    if ( !ret )
        return ret;

    LookupHit hit;
    for ( auto callable = myExpressions.front()->declaration(); ; ) {
        if ( auto d = callable->as<DataSumDeclaration>() ) {
            if ( myExpressions.size() == 1 )
                return ctx.rewrite(std::move(myExpressions.front()));

            auto& err = ctx.error(*myExpressions.front()) << "cannot be the subject of an apply-expression";
            err.see(*d);
            return SymRes::Fail;
        }

        if ( auto d = callable->as<DataProductDeclaration>() ) {
            if ( auto defn = d->definition() ) {
                hit = defn->findOverload(ctx.module(), ctx.diagnostics(), SymbolReference("ctor"));
                if ( hit ) {
                    if ( auto decl = hit.as<TemplateDeclaration>() ) {
                        hit = decl->definition()->findOverload(ctx.module(), ctx.diagnostics(), SymbolReference("", slice(myExpressions, 1)));
                        if ( auto proc = hit.as<ProcedureDeclaration>() ) {
                            if ( !myExpressions.front()->as<DotExpression>() ) {
                                auto procScope = ctx.resolver().scope().as<ProcedureScope>();
                                auto var = ctx.statement().createUnnamed(const_cast<ProcedureScope&>(*procScope), *proc->thisType());

                                std::vector<std::unique_ptr<Expression>> exprs;
                                exprs.emplace_back(std::make_unique<PrimaryExpression>(lexer::Token()));
                                exprs.back()->setDeclaration(*var);
                                exprs.emplace_back(std::move(myExpressions.front()));

                                myExpressions.front() = std::make_unique<DotExpression>(false, std::move(exprs));
                                myExpressions.front()->setDeclaration(*proc);
                            }
                        }
                    }
                }
            }
        }

        if ( auto d = callable->as<TemplateDeclaration>() ) {
            if ( auto defn = d->definition() )
                hit = defn->findOverload(ctx.module(), ctx.diagnostics(), SymbolReference("", slice(myExpressions, 1)));
        }

        if ( auto d = callable->as<DataSumDeclaration::Constructor>() ) {
            ctx.error(*myExpressions.front()) << "ds ctor not implemented";
            return SymRes::Fail;
        }

        if ( auto d = callable->as<DataProductDeclaration::Field  >() ) { callable = d->constraint().declaration(); continue; }
        if ( auto d = callable->as<SymbolDeclaration              >() ) { callable = d->expression()->declaration(); continue; }
        if ( auto d = callable->as<ProcedureDeclaration           >() ) { callable = d->returnType()->declaration(); continue; }
        if ( auto d = callable->as<ProcedureParameter             >() ) { callable = d->dataType(); continue; }
        if ( auto d = callable->as<VariableDeclaration            >() ) { callable = d->dataType(); continue; }
        if ( auto d = callable->as<SymbolVariable                 >() ) { callable = d->boundExpression()->declaration(); continue; }

        if ( auto d = callable->as<ImportDeclaration              >() ) {
            ctx.error(*myExpressions.front()) << "cannot use import declaration in apply-expression";
            return SymRes::Fail;
        }

        break;
    }

    auto tryLowerSingle = [this, &ctx](Declaration const* decl) -> SymRes {
        myDeclaration = decl;

        if ( auto proc = decl->as<ProcedureDeclaration>() ) {
            Declaration const* dt = decl;
            if ( isCtor(*proc) )
                dt = proc->parameters()[0]->dataType();
            else
                dt = proc->returnType()->declaration();

            if ( dt == ctx.axioms().intrinsic(EmptyLiteralType) )
                return SymRes::Success;

            return ctx.rewrite([&ctx, dt](std::unique_ptr<Expression>& expr) {
                auto procScope = ctx.resolver().scope().as<ProcedureScope>();
                // todo: remove const_cast
                auto var = ctx.statement().createUnnamed(const_cast<ProcedureScope&>(*procScope), *dt);
                return std::make_unique<VarExpression>(*var, std::move(expr));
            });
        }

        if ( myExpressions.size() == 1 ) {
            myExpressions.front()->setDeclaration(*myDeclaration);
            return ctx.rewrite(std::move(myExpressions.front()));
        }

        return SymRes::Success;
    };

    if ( !hit ) {
        // Look for hit on symbol
        LookupHit symHit;
        if ( subject ) {
            SymbolReference sym(subject->token().lexeme(), slice(myExpressions, 1));
            if ( symHit = ctx.matchOverload(sym) )
                return tryLowerSingle(symHit.decl());
        }

        // Doesn't match either proc or sym
        auto& err = ctx.error(*this) << "does not match any procedure overload or symbol declaration";
        if ( hit.symSpace() ) {
            for ( auto const& p : hit.symSpace()->prototypes() )
                err.see(*p.proto.decl);

            for ( auto const& p : symHit.symSpace()->prototypes() )
                err.see(*p.proto.decl);
        }
        return SymRes::Fail;
    }

    return tryLowerSingle(hit.decl());
}

/**
 * Flatten apply-expressions that don't start with a procedure identifier
 */
void ApplyExpression::flatten()
{
    return flatten(begin(myExpressions));
}

void ApplyExpression::flatten(std::vector<std::unique_ptr<Expression>>::iterator first)
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

Slice<Expression*> ApplyExpression::expressions() const
{
    return myExpressions;
}

ProcedureDeclaration const* ApplyExpression::declaration() const
{
    return static_cast<ProcedureDeclaration const*>(myDeclaration);
}

//
// SymbolExpression

SymbolExpression::SymbolExpression(lexer::Token const& identifier,
                                   std::vector<std::unique_ptr<Expression>>&& expressions)
    : Expression(Expression::Kind::Symbol)
    , myIdentifier(identifier)
    , myExpressions(std::move(expressions))
{
}

SymbolExpression::SymbolExpression(std::vector<std::unique_ptr<Expression>>&& expressions)
    : Expression(Expression::Kind::Symbol)
    , myExpressions(std::move(expressions))
{
}

SymbolExpression::SymbolExpression(lexer::Token const& open,
                                   lexer::Token const& close,
                                   std::vector<std::unique_ptr<Expression>>&& expressions)
    : Expression(Expression::Kind::Symbol)
    , myExpressions(std::move(expressions))
    , myOpenToken(open)
    , myCloseToken(close)
{
}

SymbolExpression::SymbolExpression(SymbolExpression const& rhs)
    : Expression(rhs)
    , myIdentifier(rhs.myIdentifier)
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
    Expression::swap(rhs);

    using std::swap;
    swap(myIdentifier, rhs.myIdentifier);
    swap(myExpressions, rhs.myExpressions);
    swap(myOpenToken, rhs.myOpenToken);
    swap(myCloseToken, rhs.myCloseToken);
}

void SymbolExpression::io(IStream& stream) const
{
    stream.next("identifier", myIdentifier);
    stream.next("expressions", myExpressions);
}

IMPL_CLONE_BEGIN(SymbolExpression, Expression, Expression)
IMPL_CLONE_CHILD(myExpressions)
IMPL_CLONE_END
IMPL_CLONE_REMAP_BEGIN(SymbolExpression, Expression)
IMPL_CLONE_REMAP(myExpressions)
IMPL_CLONE_REMAP_END

SymRes SymbolExpression::resolveSymbols(Context& ctx)
{
    if ( myDeclaration )
        return SymRes::Success;

    if ( myIdentifier.kind() == lexer::TokenKind::Undefined ) {
        if ( myExpressions.empty() )
            return SymRes::Success;

        auto subject = myExpressions.front()->as<PrimaryExpression>();
        if ( !subject ) {
            ctx.error(*this) << "symbol tuples must start with an identifier";
            return SymRes::Fail;
        }

        auto subjectExpression = std::move(myExpressions.front());
        myIdentifier = subject->token();
        move(next(begin(myExpressions)), end(myExpressions),
             begin(myExpressions));
        myExpressions.pop_back();

        if ( myExpressions.empty() )
            return ctx.rewrite(std::move(subjectExpression));
    }

    auto ret = ctx.resolveExpressions(myExpressions);
    if ( !ret )
        return ret;

    SymbolReference sym(myIdentifier.lexeme(), myExpressions);
    auto hit = ctx.matchOverload(sym);
    if ( !hit ) {
        ctx.error(*this) << "undeclared symbol identifier";
        return SymRes::Fail;
    }

    myDeclaration = hit.decl();
    return SymRes::Success;
}

lexer::Token const& SymbolExpression::identifier() const
{
    return myIdentifier;
}

Slice<Expression*> SymbolExpression::expressions()
{
    return myExpressions;
}

Slice<Expression*> SymbolExpression::expressions() const
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

std::vector<std::unique_ptr<Expression>>& SymbolExpression::internalExpressions()
{
    return myExpressions;
}

//
// DotExpression

DotExpression::DotExpression(bool global,
                             std::vector<std::unique_ptr<Expression>>&& exprs)
    : Expression(Kind::Dot)
    , myExpressions(std::move(exprs))
    , myGlobal(global)
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
    ScopeResolver resolver(*ctx.module().scope());
    if ( isModuleScope() )
        ctx.changeResolver(resolver);

    auto ret = ctx.resolveExpression(myExpressions.front());
    if ( !ret )
        return ret;

    {
        auto scope = memberScope(*resolveIndirections(myExpressions.front()->declaration()));
        if ( !scope ) {
            ctx.error(*myExpressions.front()) << "does not have any accessible members";
            return SymRes::Fail;
        }

        resolver = ScopeResolver(*scope);
        ctx.changeResolver(resolver);
    }

    for ( std::size_t i = 1; i < myExpressions.size() - 1; ++i ) {
        auto& e = myExpressions[i];
        ret = ctx.resolveExpression(e);
        if ( !ret )
            return ret;

        auto scope = memberScope(*resolveIndirections(e->declaration()));
        if ( !scope ) {
            ctx.error(*e) << "does not have any accessible members";
            return SymRes::Fail;
        }

        resolver = ScopeResolver(*scope);
    }

    ctx.resolveExpression(myExpressions.back());
    if ( myExpressions.back()->declaration() )
        myDeclaration = myExpressions.back()->declaration();

    return myDeclaration ? SymRes::Success : SymRes::Fail;
}

Slice<Expression*> DotExpression::expressions()
{
    return myExpressions;
}

Slice<Expression*> DotExpression::expressions() const
{
    return myExpressions;
}

Expression& DotExpression::top()
{
    return *myExpressions.back();
}

Expression const& DotExpression::top() const
{
    return *myExpressions.back();
}

bool DotExpression::isModuleScope() const
{
    return myGlobal;
}

//
// VarExpression

VarExpression::VarExpression(std::unique_ptr<PrimaryExpression> id,
                             std::unique_ptr<Expression> expression)
    : Expression(Kind::Var)
    , myIdentity(std::move(id))
    , myExpression(std::move(expression))
{
}

VarExpression::VarExpression(VariableDeclaration const& var,
                             std::unique_ptr<Expression> expression)
    : Expression(Kind::Var)
    , myIdentity(std::make_unique<PrimaryExpression>(lexer::Token()))
    , myExpression(std::move(expression))
{
    myIdentity->setDeclaration(var);
}

VarExpression::VarExpression(VarExpression const& rhs)
    : Expression(rhs)
{
}

VarExpression& VarExpression::operator = (VarExpression const& rhs)
{
    VarExpression(rhs).swap(*this);
    return *this;
}

VarExpression::~VarExpression() = default;

void VarExpression::swap(VarExpression& rhs)
{
    Expression::swap(rhs);
    using std::swap;
    swap(myIdentity, rhs.myIdentity);
    swap(myExpression, rhs.myExpression);
}

void VarExpression::io(IStream& stream) const
{
    stream.next("identity", myIdentity);
    stream.next("expression", myExpression);
}

IMPL_CLONE_BEGIN(VarExpression, Expression, Expression)
IMPL_CLONE_CHILD(myIdentity)
IMPL_CLONE_CHILD(myExpression)
IMPL_CLONE_END
IMPL_CLONE_REMAP_BEGIN(VarExpression, Expression)
IMPL_CLONE_REMAP(myIdentity)
IMPL_CLONE_REMAP(myExpression)
IMPL_CLONE_REMAP_END

SymRes VarExpression::resolveSymbols(Context& ctx)
{
    if ( myDeclaration )
        return SymRes::Success;

    auto ret = ctx.resolveExpression(reinterpret_cast<std::unique_ptr<Expression>&>(myIdentity));
    if ( !ret )
        return ret;

    ret = ctx.resolveExpression(myExpression);
    if ( !ret )
        return ret;

    myDeclaration = myIdentity->declaration();
    return SymRes::Success;
}

PrimaryExpression const& VarExpression::identity() const
{
    return *myIdentity;
}

PrimaryExpression& VarExpression::identity()
{
    return *myIdentity;
}

Expression const& VarExpression::expression() const
{
    return *myExpression;
}

Expression& VarExpression::expression()
{
    return *myExpression;
}

//
// LambdaExpression

LambdaExpression::LambdaExpression(std::unique_ptr<Expression> params,
                                   std::unique_ptr<Expression> returnType,
                                   std::unique_ptr<Expression> body)
    : Expression(Kind::Lambda)
    , myParams(std::move(params))
    , myReturnType(std::move(returnType))
    , myBody(std::move(body))
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
    swap(myParams, rhs.myParams);
    swap(myReturnType, rhs.myReturnType);
    swap(myBody, rhs.myBody);
}

void LambdaExpression::io(IStream& stream) const
{
    stream.next("param", myParams);
    stream.next("returnType", myReturnType);
    stream.next("body", myBody);
}

IMPL_CLONE_BEGIN(LambdaExpression, Expression, Expression)
IMPL_CLONE_CHILD(myParams)
IMPL_CLONE_CHILD(myReturnType)
IMPL_CLONE_CHILD(myBody)
IMPL_CLONE_END
IMPL_CLONE_REMAP_BEGIN(LambdaExpression, Expression)
IMPL_CLONE_REMAP(myParams)
IMPL_CLONE_REMAP(myReturnType)
IMPL_CLONE_REMAP(myBody)
IMPL_CLONE_REMAP_END

SymRes LambdaExpression::resolveSymbols(Context& ctx)
{
    if ( myDeclaration )
        return SymRes::Success;

    auto ret = ctx.resolveExpression(myParams);
    if ( !ret )
        return ret;

    ret = ctx.resolveExpression(myBody);
    if ( !ret )
        return ret;

    if ( !myReturnType ) {
        ctx.error(*myReturnType) << "return type deduction not implemented";
        return SymRes::Fail;
    }
    ret = ctx.resolveExpression(myReturnType);
    if ( !ret )
        return ret;

    // todo: create and resolve to ProcedureDeclaration
    return SymRes::Fail;
}

Expression const& LambdaExpression::parameters() const
{
    return *myParams;
}

Expression const& LambdaExpression::returnType() const
{
    return *myReturnType;
}

Expression const& LambdaExpression::body() const
{
    return *myBody;
}

Expression& LambdaExpression::parameters()
{
    return *myParams;
}

Expression& LambdaExpression::returnType()
{
    return *myReturnType;
}

Expression& LambdaExpression::body()
{
    return *myBody;
}

//
// Utilities

bool allResolved(Slice<Expression*> const& exprs)
{
    for ( auto const& e : exprs )
        if ( !e->declaration() )
            return false;

    return true;
}

    } // namespace ast
} // namespace kyfoo
