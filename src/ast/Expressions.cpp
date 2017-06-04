#include <kyfoo/ast/Expressions.hpp>

#include <algorithm>
#include <iterator>

#include <kyfoo/Diagnostics.hpp>

#include <kyfoo/ast/Declarations.hpp>
#include <kyfoo/ast/Semantics.hpp>

namespace kyfoo {
    namespace ast {

PrimaryExpression const* front(Expression const* expr)
{
    if ( auto p = expr->as<PrimaryExpression>() )
        return p;

    if ( auto t = expr->as<TupleExpression>() )
        return front(t->expressions()[0]);

    if ( auto c = expr->as<ConstraintExpression>() )
        return front(c->subject());

    throw std::runtime_error("failed to find front of expression");
}

template <typename T>
std::vector<std::unique_ptr<T>> clone(std::vector<std::unique_ptr<T>> const& rhs)
{
    std::vector<std::unique_ptr<T>> ret;
    ret.reserve(rhs.size());
    for ( auto const& e : rhs )
        ret.emplace_back(e->clone());

    return ret;
}

struct EnforceResolution
{
    Context& ctx;

    EnforceResolution(Context& ctx)
        : ctx(ctx)
    {
    }

    void operator () (Expression const* expr) const
    {
        if ( auto p = expr->as<PrimaryExpression>() ) {
            if ( p->token().kind() == lexer::TokenKind::Identifier && !p->declaration() )
                ctx.error(p->token()) << "does not identify a declaration";
        }
    }
};

template <typename T>
void preorder(Expression const* expr, T& op)
{
    op(expr);

    if ( auto t = expr->as<TupleExpression>() ) {
        for ( auto const& e : t->expressions() )
            preorder(e, op);
    }
    else if ( auto a = expr->as<ApplyExpression>() ) {
        for ( auto const& e : a->expressions() )
            preorder(e, op);
    }
    else if ( auto s = expr->as<SymbolExpression>() ) {
        for ( auto const& e : s->expressions() )
            preorder(e, op);
    }
    else if ( auto c = expr->as<ConstraintExpression>() ) {
        preorder(c->subject(), op);
        preorder(c->constraint(), op);
    }
}

//
// Context

Context::Context(Diagnostics& dgn, IResolver& resolver)
    : myDiagnostics(&dgn)
    , myResolver(&resolver)
{
}

Context::~Context() = default;

Error& Context::error(lexer::Token const& token)
{
    return myDiagnostics->error(myResolver->module(), token);
}

std::size_t Context::errorCount() const
{
    return myDiagnostics->errorCount();
}

Declaration const* Context::lookup(SymbolReference const& sym) const
{
    return myResolver->lookup(sym);
}

Declaration const* Context::match(SymbolReference const& sym) const
{
    return myResolver->match(sym);
}

Declaration const* Context::matchProcedure(SymbolReference const& sym) const
{
    return myResolver->matchProcedure(sym);
}

void Context::rewrite(std::unique_ptr<Expression> expr)
{
    myRewrite = std::move(expr);
}

void Context::resolveExpression(std::unique_ptr<Expression>& expression)
{
    myRewrite.reset();
    expression->resolveSymbols(*this);
    if ( myRewrite )
        expression = std::move(myRewrite);
}

void Context::resolveExpressions(std::vector<std::unique_ptr<Expression>>& expressions)
{
    myRewrite.reset();

    for ( auto i = begin(expressions); i != end(expressions); ++i ) {
        (*i)->resolveSymbols(*this);
        if ( myRewrite )
            *i = std::move(std::move(myRewrite));
    }
}

//
// Expression

Expression::Expression(Kind kind)
    : myKind(kind)
{
}

Expression::Expression(Expression const& rhs)
    : myKind(rhs.myKind)
{
}

Expression::~Expression() = default;

void Expression::swap(Expression& rhs)
{
    using std::swap;
    swap(myKind, rhs.myKind);
}

Expression::Kind Expression::kind() const
{
    return myKind;
}

//
// PrimaryExpression

PrimaryExpression::PrimaryExpression(lexer::Token const& token)
    : base_t(Expression::Kind::Primary)
    , myToken(token)
{
}

PrimaryExpression::PrimaryExpression(PrimaryExpression const& rhs)
    : base_t(rhs)
    , myToken(rhs.myToken)
    , myDeclaration(rhs.myDeclaration)
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

void PrimaryExpression::resolveSymbols(Context& ctx)
{
    if ( myToken.kind() != lexer::TokenKind::Identifier )
        return;

    auto decl = ctx.lookup(Symbol(myToken));
    if ( decl )
        myDeclaration = decl;

    // Not an error to miss its basic symbol lookup
}

lexer::Token const& PrimaryExpression::token() const
{
    return myToken;
}

Declaration const* PrimaryExpression::declaration() const
{
    return myDeclaration;
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

TupleExpression::TupleExpression(TupleKind kind,
                                 std::vector<std::unique_ptr<Expression>>&& expressions)
    : base_t(Expression::Kind::Tuple)
    , myKind(kind)
    , myExpressions(std::move(expressions))
{
}

TupleExpression::TupleExpression(TupleExpression const& rhs)
    : base_t(rhs)
    , myKind(rhs.myKind)
    , myExpressions(ast::clone(rhs.myExpressions))
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

void TupleExpression::resolveSymbols(Context& ctx)
{
    ctx.resolveExpressions(myExpressions);
}

TupleKind TupleExpression::kind() const
{
    return myKind;
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
                move(begin(tuple->myExpressions), end(tuple->myExpressions),
                     std::inserter(myExpressions, i));
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
    : base_t(Expression::Kind::Apply)
    , myExpressions(std::move(expressions))
{
}

ApplyExpression::ApplyExpression(ApplyExpression const& rhs)
    : base_t(rhs)
    , myExpressions(ast::clone(rhs.myExpressions))
    , myDeclaration(rhs.myDeclaration)
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
    swap(myDeclaration, rhs.myDeclaration);
}

void ApplyExpression::io(IStream& stream) const
{
    stream.next("expressions", myExpressions);
}

void ApplyExpression::resolveSymbols(Context& ctx)
{
    ctx.resolveExpressions(myExpressions);

    auto subject = myExpressions.front()->as<PrimaryExpression>();
    if ( !subject ) {
        auto p = front(subject);
        ctx.error(p->token()) << "symbol tuples must start with an identifier";
        return;
    }

    auto args = slice(myExpressions, 1);
    SymbolReference sym(subject->token().lexeme(), args);

    // Look for hit on symbol
    auto decl = ctx.match(sym);
    if ( decl ) {
        // Transmute apply-expression into symbol-expression
        auto id = subject->token();
        auto expr = std::move(myExpressions);
        rotate(begin(expr), next(begin(expr)), end(expr));
        expr.pop_back();
        ctx.rewrite(std::make_unique<SymbolExpression>(id, std::move(expr)));
        return;
    }

    // Search procedure overloads by arguments
    auto procDecl = ctx.matchProcedure(sym);
    if ( !procDecl ) {
        auto p = front(subject);
        ctx.error(p->token()) << "does not match any procedure overloads";
        // todo: references to potential overloads
        return;
    }
}

/**
 * Flatten apply-expressions that don't start with a procedure identifier
 */
void ApplyExpression::flatten()
{
    for ( auto i = begin(myExpressions); i != end(myExpressions); ) {
        if ( auto apply = (*i)->as<ApplyExpression>() ) {
            if ( auto p = apply->expressions()[0]->as<PrimaryExpression>() ) {
                if ( auto d = p->declaration() )
                    if ( d->as<ProcedureDeclaration>() )
                        goto L_next;
            }

            move(begin(apply->myExpressions), end(apply->myExpressions),
                 std::inserter(myExpressions, i));
            goto L_removeItem;
        }

    L_next:
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

//
// SymbolExpression

SymbolExpression::SymbolExpression(lexer::Token const& identifier,
                                   std::vector<std::unique_ptr<Expression>>&& expressions)
    : base_t(Expression::Kind::Symbol)
    , myIdentifier(identifier)
    , myExpressions(std::move(expressions))
{
}

SymbolExpression::SymbolExpression(std::vector<std::unique_ptr<Expression>>&& expressions)
    : base_t(Expression::Kind::Symbol)
    , myExpressions(std::move(expressions))
{
}

SymbolExpression::SymbolExpression(SymbolExpression const& rhs)
    : base_t(rhs)
    , myExpressions(ast::clone(rhs.myExpressions))
    , myDeclaration(rhs.myDeclaration)
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
    swap(myExpressions, rhs.myExpressions);
    swap(myDeclaration, rhs.myDeclaration);
}

void SymbolExpression::io(IStream& stream) const
{
    stream.next("expressions", myExpressions);
}

void SymbolExpression::resolveSymbols(Context& ctx)
{
    if ( myIdentifier.kind() == lexer::TokenKind::Undefined ) {
        if ( myExpressions.empty() )
            return;

        auto subject = myExpressions.front()->as<PrimaryExpression>();
        if ( !subject ) {
            auto p = front(subject);
            ctx.error(p->token()) << "symbol tuples must start with an identifier";
            return;
        }

        myIdentifier = subject->token();
        rotate(begin(myExpressions), next(begin(myExpressions)), end(myExpressions));
        myExpressions.pop_back();
    }

    ctx.resolveExpressions(myExpressions);

    {
        auto const startCount = ctx.errorCount();
        EnforceResolution op(ctx);
        preorder(this, op);

        if ( ctx.errorCount() - startCount )
            return;
    }

    SymbolReference sym(myIdentifier.lexeme(), myExpressions);
    auto decl = ctx.match(sym);
    if ( !decl ) {
        ctx.error(myIdentifier) << "undeclared symbol identifier";
        return;
    }
}

Slice<Expression*> SymbolExpression::expressions()
{
    return myExpressions;
}

Slice<Expression*> SymbolExpression::expressions() const
{
    return myExpressions;
}

std::vector<std::unique_ptr<Expression>>& SymbolExpression::internalExpressions()
{
    return myExpressions;
}

//
// ConstraintExpression

ConstraintExpression::ConstraintExpression(std::unique_ptr<Expression> subject,
                                           std::unique_ptr<Expression> constraint)
    : base_t(Expression::Kind::Constraint)
    , mySubject(std::move(subject))
    , myConstraint(std::move(constraint))
{
    if ( !mySubject )
        throw std::runtime_error("constrain expression must have a subject");

    if ( !myConstraint )
        throw std::runtime_error("constrain expression must have a constraint");
}

ConstraintExpression::ConstraintExpression(ConstraintExpression const& rhs)
    : base_t(rhs)
    , mySubject(rhs.mySubject->clone())
    , myConstraint(rhs.myConstraint->clone())
{
}

ConstraintExpression& ConstraintExpression::operator = (ConstraintExpression const& rhs)
{
    ConstraintExpression(rhs).swap(*this);
    return *this;
}

void ConstraintExpression::swap(ConstraintExpression& rhs)
{
    Expression::swap(rhs);

    using std::swap;
    swap(mySubject, rhs.mySubject);
    swap(myConstraint, rhs.myConstraint);
}

ConstraintExpression::~ConstraintExpression() = default;

void ConstraintExpression::io(IStream& stream) const
{
    stream.next("subject", mySubject);
    stream.next("constraint", myConstraint);
}

void ConstraintExpression::resolveSymbols(Context& ctx)
{
    ctx.resolveExpression(mySubject);
    ctx.resolveExpression(myConstraint);
}

Expression const* ConstraintExpression::subject() const
{
    return mySubject.get();
}

Expression const* ConstraintExpression::constraint() const
{
    return myConstraint.get();
}

    } // namespace ast
} // namespace kyfoo
