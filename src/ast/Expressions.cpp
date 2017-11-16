#include <kyfoo/ast/Expressions.hpp>

#include <algorithm>
#include <iterator>

#include <kyfoo/Diagnostics.hpp>

#include <kyfoo/ast/Axioms.hpp>
#include <kyfoo/ast/Context.hpp>
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

void PrimaryExpression::resolveSymbols(Context& ctx)
{
    if ( myDeclaration )
        return;

    switch ( myToken.kind() ) {
    case lexer::TokenKind::Integer:
    {
        myDeclaration = ctx.axioms().intrinsic(IntegerLiteralType);
        return;
    }

    case lexer::TokenKind::Rational:
    {
        myDeclaration = ctx.axioms().intrinsic(RationalLiteralType);
        return;
    }

    case lexer::TokenKind::String:
    {
        ctx.module().interpretString(ctx.diagnostics(), myToken);
        myDeclaration = ctx.axioms().intrinsic(StringLiteralType);
        return;
    }

    case lexer::TokenKind::FreeVariable:
    {
        if ( !myDeclaration ) {
            ctx.error(myToken) << "free variable not expected in this context";
            return;
        }

        return;
    }

    case lexer::TokenKind::Identifier:
    {
        // todo: removeme
        if ( myToken.lexeme() == "null" ) {
            myDeclaration = ctx.axioms().intrinsic(PointerNullLiteralType);
            return;
        }

        auto hit = ctx.matchOverload(SymbolReference(myToken.lexeme()));
        if ( !hit ) {
            ctx.error(myToken) << "undeclared identifier";
            return;
        }

        if ( auto templ = hit.as<TemplateDeclaration>() ) {
            ctx.error(myToken) << "does not refer to any template invocation";
            return;
        }
        else if ( auto proc = hit.as<ProcedureDeclaration>() ) {
            throw std::runtime_error("primary-expression cannot refer to a procedure");
        }

        myDeclaration = hit.decl();
        return;
    }

    default:
        throw std::runtime_error("unhandled primary expression");
    }
}

lexer::Token const& PrimaryExpression::token() const
{
    return myToken;
}

void PrimaryExpression::setFreeVariable(Declaration const* decl)
{
    if ( myDeclaration )
        throw std::runtime_error("free variable can only be bound once");

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

void ReferenceExpression::resolveSymbols(Context& ctx)
{
    if ( myDeclaration )
        return;

    ctx.resolveExpression(myExpression);
    if ( ctx.errorCount() )
        return;

    if ( !isMemoryDeclaration(myExpression->declaration()->kind()) ) {
        ctx.error(*this) << "reference-expressions can only apply to expressions with storage";
        return;
    }

    myDeclaration = myExpression->declaration();
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

void TupleExpression::resolveSymbols(Context& ctx)
{
    if ( myDeclaration )
        return;

    ctx.resolveExpressions(myExpressions);

    if ( myKind == TupleKind::Open ) {
        if ( myExpressions.empty() ) {
            myDeclaration = ctx.axioms().intrinsic(EmptyLiteralType);
            return;
        }
        else if ( myExpressions.size() == 1 ) {
            return ctx.rewrite(std::move(myExpressions[0]));
        }
    }

    flattenOpenTuples();
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

void ApplyExpression::resolveSymbols(Context& ctx)
{
    if ( myDeclaration )
        return;

    ctx.resolveExpressions(next(begin(myExpressions)), end(myExpressions));
    flatten(next(begin(myExpressions)));

    if ( !allResolved(slice(myExpressions, 1)) ) {
        ctx.error(*this) << "compilation stopped due to prior unresolved symbols";
        return;
    }

    auto subject = myExpressions.front()->as<PrimaryExpression>();
    if ( subject ) {
        if ( subject->token().kind() != lexer::TokenKind::Identifier )
            return ctx.rewrite(std::make_unique<TupleExpression>(TupleKind::Open, std::move(myExpressions)));

        auto hit = ctx.matchOverload(SymbolReference(subject->token().lexeme()));
        if ( !hit )
            return ctx.rewrite(std::make_unique<SymbolExpression>(std::move(myExpressions)));

        subject->setDeclaration(*hit.decl());
    }

    ctx.resolveExpression(myExpressions.front());
    if ( !myExpressions.front()->declaration() ) {
        ctx.error(*myExpressions.front()) << "unresolved expressions";
        return;
    }

    if ( !subject && myExpressions.size() == 1)
        return ctx.rewrite(std::move(myExpressions.front()));

    LookupHit hit;
    for ( auto callable = myExpressions.front()->declaration(); ; ) {
        if ( auto d = callable->as<DataSumDeclaration>() ) {
            if ( myExpressions.size() == 1 )
                return ctx.rewrite(std::move(myExpressions.front()));

            auto& err = ctx.error(*myExpressions.front()) << "cannot be the subject of an apply-expression";
            err.see(*d);
            return;
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
            return;
        }

        if ( auto d = callable->as<DataProductDeclaration::Field  >() ) { callable = d->constraint().declaration(); continue; }
        if ( auto d = callable->as<SymbolDeclaration              >() ) { callable = d->expression()->declaration(); continue; }
        if ( auto d = callable->as<ProcedureDeclaration           >() ) { callable = d->returnType()->declaration(); continue; }
        if ( auto d = callable->as<ProcedureParameter             >() ) { callable = d->dataType(); continue; }
        if ( auto d = callable->as<VariableDeclaration            >() ) { callable = d->dataType(); continue; }
        if ( auto d = callable->as<SymbolVariable                 >() ) { callable = d->boundExpression()->declaration(); continue; }

        if ( auto d = callable->as<ImportDeclaration              >() ) {
            ctx.error(*myExpressions.front()) << "cannot use import declaration in apply-expression";
            return;
        }

        break;
    }

    auto tryLowerSingle = [this, &ctx](Declaration const* decl) {
        myDeclaration = decl;

        if ( auto proc = decl->as<ProcedureDeclaration>() ) {
            Declaration const* dt = decl;
            if ( isCtor(*proc) )
                dt = proc->parameters()[0]->dataType();
            else
                dt = proc->returnType()->declaration();

            if ( dt == ctx.axioms().intrinsic(EmptyLiteralType) )
                return;

            return ctx.rewrite([&ctx, dt](std::unique_ptr<Expression>& expr) {
                auto procScope = ctx.resolver().scope().as<ProcedureScope>();
                // todo: remove const_cast
                auto var = ctx.statement().createUnnamed(const_cast<ProcedureScope&>(*procScope), *dt);
                return std::make_unique<VarExpression>(*var, std::move(expr));
            });
        }

        if ( myExpressions.size() == 1 ) {
            myExpressions.front()->setDeclaration(*myDeclaration);
            ctx.rewrite(std::move(myExpressions.front()));
        }
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
        return;
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

void SymbolExpression::resolveSymbols(Context& ctx)
{
    if ( myDeclaration )
        return;

    if ( myIdentifier.kind() == lexer::TokenKind::Undefined ) {
        if ( myExpressions.empty() )
            return;

        auto subject = myExpressions.front()->as<PrimaryExpression>();
        if ( !subject ) {
            ctx.error(*this) << "symbol tuples must start with an identifier";
            return;
        }

        auto subjectExpression = std::move(myExpressions.front());
        myIdentifier = subject->token();
        move(next(begin(myExpressions)), end(myExpressions),
             begin(myExpressions));
        myExpressions.pop_back();

        if ( myExpressions.empty() )
            return ctx.rewrite(std::move(subjectExpression));
    }

    ctx.resolveExpressions(myExpressions);
    if ( !allResolved(myExpressions) )
        return;

    SymbolReference sym(myIdentifier.lexeme(), myExpressions);
    auto hit = ctx.matchOverload(sym);
    if ( !hit ) {
        ctx.error(*this) << "undeclared symbol identifier";
        return;
    }

    myDeclaration = hit.decl();
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

void DotExpression::resolveSymbols(Context& ctx)
{
    IResolver* originalResolver = &ctx.resolver();
    ScopeResolver resolver(*ctx.module().scope());
    if ( isModuleScope() )
        ctx.changeResolver(resolver);

    ctx.resolveExpression(myExpressions.front());
    if ( !myExpressions.front()->declaration() )
        return;

    {
        auto scope = memberScope(*resolveIndirections(myExpressions.front()->declaration()));
        if ( !scope ) {
            ctx.error(*myExpressions.front()) << "does not have any accessible members";
            return;
        }

        resolver = ScopeResolver(*scope);
        ctx.changeResolver(resolver);
    }

    for ( std::size_t i = 1; i < myExpressions.size() - 1; ++i ) {
        auto& e = myExpressions[i];
        ctx.resolveExpression(e);
        if ( !e->declaration() )
            return;

        auto scope = memberScope(*resolveIndirections(e->declaration()));
        if ( !scope ) {
            ctx.error(*e) << "does not have any accessible members";
            return;
        }

        resolver = ScopeResolver(*scope);
    }

    ctx.resolveExpression(myExpressions.back());
    if ( myExpressions.back()->declaration() )
        myDeclaration = myExpressions.back()->declaration();

    ctx.changeResolver(*originalResolver);
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

void VarExpression::resolveSymbols(Context& ctx)
{
    if ( myDeclaration )
        return;

    ctx.resolveExpression(reinterpret_cast<std::unique_ptr<Expression>&>(myIdentity));
    ctx.resolveExpression(myExpression);

    myDeclaration = myIdentity->declaration();
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

void LambdaExpression::resolveSymbols(Context& ctx)
{
    if ( myDeclaration )
        return;

    ctx.resolveExpression(myParams);
    ctx.resolveExpression(myBody);

    if ( !myReturnType ) {
        ctx.error(*myReturnType) << "return type deduction not implemented";
        return;
    }
    ctx.resolveExpression(myReturnType);

    // todo: create and resolve to ProcedureDeclaration
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
// BranchExpression

BranchExpression::BranchExpression(lexer::Token const& token,
                                   std::unique_ptr<Expression> condition)
    : Expression(Kind::Branch)
    , myToken(token)
    , myCondition(std::move(condition))
{
}

BranchExpression::BranchExpression(BranchExpression const& rhs)
    : Expression(rhs)
    , myToken(rhs.myToken)
{
}

BranchExpression& BranchExpression::operator = (BranchExpression const& rhs)
{
    BranchExpression(rhs).swap(*this);
    return *this;
}

BranchExpression::~BranchExpression() = default;

void BranchExpression::swap(BranchExpression& rhs)
{
    Expression::swap(rhs);
    using std::swap;
    swap(myToken, rhs.myToken);
    swap(myCondition, rhs.myCondition);
}

void BranchExpression::io(IStream& stream) const
{
    stream.next("condition", myCondition);
}

IMPL_CLONE_BEGIN(BranchExpression, Expression, Expression)
IMPL_CLONE_CHILD(myCondition)
IMPL_CLONE_END
IMPL_CLONE_REMAP_BEGIN(BranchExpression, Expression)
IMPL_CLONE_REMAP(myCondition)
IMPL_CLONE_REMAP_END

void BranchExpression::resolveSymbols(Context& ctx)
{
    if ( !condition() ) {
        ctx.error(*this) << "is missing a condition";
        return;
    }

    for ( auto b = this; b; b = b->next() ) {
        if ( b->condition() ) {
            ctx.resolveExpression(b->myCondition);
        }
        else if ( b->next() ) {
            ctx.error(*b) << "alternative branch must occur at the end of a branch sequence";
            return;
        }

        if ( !scope() ) {
            ctx.error(*b) << "is missing a branch body";
            return;
        }

        b->scope()->resolveSymbols(ctx.module(), ctx.diagnostics());
    }
}

lexer::Token const& BranchExpression::token() const
{
    return myToken;
}

Expression const* BranchExpression::condition() const
{
    return myCondition.get();
}

Expression* BranchExpression::condition()
{
    return myCondition.get();
}

ProcedureScope const* BranchExpression::scope() const
{
    return myScope.get();
}

ProcedureScope* BranchExpression::scope()
{
    return myScope.get();
}

void BranchExpression::setScope(std::unique_ptr<ProcedureScope> scope)
{
    myScope = std::move(scope);
}

BranchExpression const* BranchExpression::next() const
{
    return myNext.get();
}

BranchExpression* BranchExpression::next()
{
    return myNext.get();
}

void BranchExpression::setNext(std::unique_ptr<BranchExpression> branchExpr)
{
    myNext = std::move(branchExpr);
}

//
// ReturnExpression

ReturnExpression::ReturnExpression(lexer::Token const& token,
                                   std::unique_ptr<Expression> expression)
    : Expression(Kind::Return)
    , myToken(token)
    , myExpression(std::move(expression))
{
}

ReturnExpression::ReturnExpression(ReturnExpression const& rhs)
    : Expression(rhs)
{
}

ReturnExpression& ReturnExpression::operator = (ReturnExpression const& rhs)
{
    ReturnExpression(rhs).swap(*this);
    return *this;
}

ReturnExpression::~ReturnExpression() = default;

void ReturnExpression::swap(ReturnExpression& rhs)
{
    Expression::swap(rhs);
    using std::swap;
    swap(myExpression, rhs.myExpression);
}

void ReturnExpression::io(IStream& stream) const
{
    stream.next("expr", myExpression);
}

IMPL_CLONE_BEGIN(ReturnExpression, Expression, Expression)
IMPL_CLONE_CHILD(myExpression)
IMPL_CLONE_END
IMPL_CLONE_REMAP_BEGIN(ReturnExpression, Expression)
IMPL_CLONE_REMAP(myExpression)
IMPL_CLONE_REMAP_END

void ReturnExpression::resolveSymbols(Context& ctx)
{
    if ( myExpression )
        ctx.resolveExpression(myExpression);
}

lexer::Token const& ReturnExpression::token() const
{
    return myToken;
}

Expression const* ReturnExpression::expression() const
{
    return myExpression.get();
}

Expression* ReturnExpression::expression()
{
    return myExpression.get();
}

//
// Utilities

template <typename Dispatcher>
struct FrontExpression
{
    using result_t = lexer::Token const&;

    Dispatcher& dispatch;

    FrontExpression(Dispatcher& dispatch)
        : dispatch(dispatch)
    {
    }

    result_t exprPrimary(PrimaryExpression const& p)
    {
        return p.token();
    }

    result_t exprReference(ReferenceExpression const& r)
    {
        return dispatch(r.expression());
    }

    result_t exprTuple(TupleExpression const& t)
    {
        if ( t.expressions().empty() )
            return t.openToken();

        return dispatch(*t.expressions()[0]);
    }

    result_t exprApply(ApplyExpression const& a)
    {
        return dispatch(*a.expressions()[0]);
    }

    result_t exprSymbol(SymbolExpression const& s)
    {
        if ( s.identifier().kind() != lexer::TokenKind::Undefined )
            return s.identifier();

        if ( s.expressions().empty() )
            return s.openToken();

        return dispatch(*s.expressions()[0]);
    }

    result_t exprDot(DotExpression const& d)
    {
        return dispatch(*d.expressions()[0]);
    }

    result_t exprVar(VarExpression const& v)
    {
        return exprPrimary(v.identity());
    }

    result_t exprLambda(LambdaExpression const& l)
    {
        return dispatch(l.parameters());
    }

    result_t exprBranch(BranchExpression const& b)
    {
        return dispatch(*b.condition());
    }

    result_t exprReturn(ReturnExpression const& r)
    {
        if ( r.expression() )
            return dispatch(*r.expression());
        
        return r.token();
    }
};

lexer::Token const& front(Expression const& expr)
{
    ShallowApply<FrontExpression> op;
    return op(expr);
}

template <typename Dispatcher>
struct PrintOperator
{
    using result_t = std::ostream&;

    Dispatcher& dispatch;
    result_t stream;
    int nest = 0;

    PrintOperator(Dispatcher& dispatch, result_t stream)
        : dispatch(dispatch)
        , stream(stream)
    {
    }

    result_t printConstraints(Expression const& expr)
    {
        for ( auto c : expr.constraints() ) {
            stream << ": ";
            dispatch(*c);
        }

        return stream;
    }

    result_t exprPrimary(PrimaryExpression const& p)
    {
        stream << p.token().lexeme();
        return printConstraints(p);
    }

    result_t exprReference(ReferenceExpression const& r)
    {
        stream << "=";
        return dispatch(r.expression());
    }

    result_t exprTuple(TupleExpression const& t)
    {
        stream << presentTupleOpen(t.kind());

        if ( !t.expressions().empty() ) {
            dispatch(*t.expressions()[0]);

            for ( auto const& e : t.expressions()(1, t.expressions().size()) ) {
                stream << presentTupleWeave(t.kind());
                dispatch(*e);
            }
        }

        stream << presentTupleClose(t.kind());
        return printConstraints(t);
    }

    result_t exprApply(ApplyExpression const& a)
    {
        if ( nest )
            stream << "(";

        ++nest;
        auto first = true;
        for ( auto const& e : a.expressions() ) {
            if ( !first )
                stream << " ";
            else
                first = false;

            dispatch(*e);
        }
        --nest;
        if ( nest )
            stream << ")";

        return printConstraints(a);
    }

    result_t exprSymbol(SymbolExpression const& s)
    {
        auto const& id = s.identifier().lexeme();
        if ( !id.empty() )
            stream << id;

        if ( !s.expressions().empty() ) {
            stream << '<';
            dispatch(*s.expressions()[0]);

            for ( auto const& e : s.expressions()(1, s.expressions().size()) ) {
                stream << ", ";
                dispatch(*e);
            }

            return stream << '>';
        }

        if ( id.empty() )
            stream << "<>";

        return printConstraints(s);
    }

    result_t exprDot(DotExpression const& d)
    {
        if ( d.isModuleScope() )
            stream << ".";

        auto l = begin(d.expressions());
        auto r = end(d.expressions());
        if ( l != r )
            dispatch(*(*l));

        ++l;
        for ( ; l != r; ++l ) {
            stream << ".";
            dispatch(*(*l));
        }

        return printConstraints(d);
    }

    result_t exprVar(VarExpression const& v)
    {
        if ( v.identity().token().kind() != lexer::TokenKind::Undefined ) {
            exprPrimary(v.identity());
            stream << " = ";
        }

        return dispatch(v.expression());
    }

    result_t exprLambda(LambdaExpression const& l)
    {
        dispatch(l.parameters());
        stream << " => ";
        return dispatch(l.body());
    }

    result_t exprBranch(BranchExpression const& b)
    {
        stream << ":? ";
        return dispatch(*b.condition());
    }

    result_t exprReturn(ReturnExpression const& r)
    {
        stream << "return ";
        if ( r.expression() )
            return dispatch(*r.expression());

        return stream;
    }
};

std::ostream& print(std::ostream& stream, Expression const& expr)
{
    ShallowApply<PrintOperator> op(stream);
    return op(expr);
}

bool allResolved(Slice<Expression*> const& exprs)
{
    for ( auto const& e : exprs )
        if ( !e->declaration() )
            return false;

    return true;
}

template <typename Dispatcher>
struct ClearDeclaration
{
    using result_t = void;

    Dispatcher& dispatch;

    ClearDeclaration(Dispatcher& dispatch)
        : dispatch(dispatch)
    {
    }

    result_t dispatchConstraints(Expression& expr)
    {
        for ( auto c : expr.constraints() )
            dispatch(*c);
    }

    result_t exprPrimary(PrimaryExpression& p)
    {
        if ( p.token().kind() == lexer::TokenKind::Identifier )
            p.clearDeclaration();

        dispatchConstraints(p);
    }

    result_t exprReference(ReferenceExpression& r)
    {
        r.clearDeclaration();
        dispatch(r.expression());
        dispatchConstraints(r);
    }

    result_t exprTuple(TupleExpression& t)
    {
        t.clearDeclaration();
        for ( auto const& e : t.expressions() )
            dispatch(*e);

        dispatchConstraints(t);
    }

    result_t exprApply(ApplyExpression& a)
    {
        a.clearDeclaration();
        for ( auto const& e : a.expressions() )
            dispatch(*e);

        dispatchConstraints(a);
    }

    result_t exprSymbol(SymbolExpression& s)
    {
        s.clearDeclaration();
        for ( auto const& e : s.expressions() )
            dispatch(*e);

        dispatchConstraints(s);
    }

    result_t exprDot(DotExpression& d)
    {
        d.clearDeclaration();
        for ( auto& e : d.expressions() )
            dispatch(*e);

        dispatchConstraints(d);
    }

    result_t exprVar(VarExpression& v)
    {
        v.clearDeclaration();
        if ( v.identity().token().kind() != lexer::TokenKind::Undefined )
            exprPrimary(v.identity());

        dispatch(v.expression());
        dispatchConstraints(v);
    }

    result_t exprLambda(LambdaExpression& l)
    {
        l.clearDeclaration();
        dispatch(l.parameters());
        dispatch(l.body());
        dispatchConstraints(l);
    }

    result_t exprBranch(BranchExpression& b)
    {
        b.clearDeclaration();
        for ( auto p = &b; p; p = p->next() ) {
            if ( p->condition() )
                dispatch(*p->condition());

            for ( auto& stmt : p->scope()->statements() )
                dispatch(stmt.expression());
        }
    }

    result_t exprReturn(ReturnExpression& r)
    {
        r.clearDeclaration();
        if ( r.expression() )
            r.expression()->clearDeclaration();
    }
};

void clearDeclarations(Expression& expr)
{
    ShallowApply<ClearDeclaration> op;
    op(expr);
}

    } // namespace ast
} // namespace kyfoo
