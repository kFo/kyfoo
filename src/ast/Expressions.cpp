#include <kyfoo/ast/Expressions.hpp>

#include <algorithm>
#include <iterator>

#include <kyfoo/Diagnostics.hpp>

#include <kyfoo/ast/Axioms.hpp>
#include <kyfoo/ast/Context.hpp>
#include <kyfoo/ast/Declarations.hpp>
#include <kyfoo/ast/Scopes.hpp>
#include <kyfoo/ast/Semantics.hpp>

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

void Expression::setDeclaration(Declaration& decl)
{
    myDeclaration = &decl;
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

        auto hit = ctx.matchCovariant(SymbolReference(myToken.lexeme()));
        if ( !hit ) {
            if ( !hit.symSpace() )
                ctx.error(myToken) << "undeclared identifier";
            return;
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

ReferenceExpression::ReferenceExpression(lexer::Token const& token)
    : PrimaryExpression(Expression::Kind::Reference, token)
{
}

ReferenceExpression::ReferenceExpression(ReferenceExpression const& rhs)
    : PrimaryExpression(rhs)
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
    PrimaryExpression::swap(rhs);
}

void ReferenceExpression::io(IStream& stream) const
{
    PrimaryExpression::io(stream);
}

IMPL_CLONE_BEGIN(ReferenceExpression, PrimaryExpression, Expression)
IMPL_CLONE_END
IMPL_CLONE_REMAP_BEGIN(ReferenceExpression, Expression)
IMPL_CLONE_REMAP_END

void ReferenceExpression::resolveSymbols(Context& ctx)
{
    if ( myDeclaration )
        return;

    auto hit = ctx.matchCovariant(SymbolReference(token().lexeme()));
    if ( !hit ) {
        if ( !hit.symSpace() )
            ctx.error(token()) << "undeclared identifier";
        return;
    }

    myDeclaration = hit.decl();
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
    auto subject = myExpressions.front()->as<PrimaryExpression>();
    if ( !subject ) {
        // explicit procedure lookup
        ctx.resolveExpressions(myExpressions);
        flatten();

        if ( !allResolved(myExpressions) ) {
            ctx.error(*this) << "compilation stopped due to prior unresolved symbols";
            return;
        }
    }
    else {
        // implicit procedure lookup
        if ( !isIdentifier(subject->token().kind()) ) {
            ctx.error(*this) << "procedure application must begin with an identifier";
            return;
        }

        if ( subject->token().kind() == lexer::TokenKind::FreeVariable ) {
            // defer symbol lookup until concrete expression is instantiated
            return;
        }

        ctx.resolveExpressions(next(begin(myExpressions)), end(myExpressions));
        flatten(next(begin(myExpressions)));

        if ( !allResolved(slice(myExpressions, 1)) ) {
            ctx.error(*this) << "compilation stopped due to prior unresolved symbols";
            return;
        }

        SymbolReference sym(subject->token().lexeme(), slice(myExpressions, 1));

        // Look for hit on symbol
        auto hit = ctx.matchCovariant(sym);
        if ( hit ) {
            myDeclaration = hit.decl();
            return;
        }
        else {
            ctx.resolveExpression(myExpressions.front());
            if ( !myExpressions.front()->declaration() ) {
                auto& err = ctx.error(*this) << "does not match any symbol declarations or procedure overloads";
                // todo: references to potential overloads
                // todo: chained symbol sets
                // todo: lookup failures are not returning symbol sets
                if ( hit.symSpace() ) {
                    for ( auto const& p : hit.symSpace()->prototypes() )
                        err.see(*p.proto.decl);
                }
                return;
            }
        }
    }

    // Treat subject as callable
    LookupHit hit;
    for ( auto callable = myExpressions.front()->declaration(); ; ) {
        if ( auto d = callable->as<DataSumDeclaration             >() ) hit = const_cast<DataSumDeclaration*    >(d)->definition()->findCovariant(ctx.diagnostics(), SymbolReference("", slice(myExpressions, 1)));
        if ( auto d = callable->as<DataProductDeclaration         >() ) hit = const_cast<DataProductDeclaration*>(d)->definition()->findCovariant(ctx.diagnostics(), SymbolReference("", slice(myExpressions, 1)));
        if ( auto d = callable->as<TemplateDeclaration            >() ) hit = const_cast<TemplateDeclaration*   >(d)->definition()->findCovariant(ctx.diagnostics(), SymbolReference("", slice(myExpressions, 1)));

        if ( auto d = callable->as<DataSumDeclaration::Constructor>() ) { ctx.error(*myExpressions.front()) << "ds ctor not implemented"; return; }
        if ( auto d = callable->as<DataProductDeclaration::Field  >() ) { callable = d->constraint().declaration(); continue; }
        if ( auto d = callable->as<SymbolDeclaration              >() ) { callable = d->expression()->declaration(); continue; }
        if ( auto d = callable->as<ProcedureDeclaration           >() ) { callable = d->returnType()->declaration(); continue; }
        if ( auto d = callable->as<ProcedureParameter             >() ) { callable = d->dataType(); continue; }
        if ( auto d = callable->as<VariableDeclaration            >() ) { callable = d->dataType(); continue; }
        if ( auto d = callable->as<SymbolVariable                 >() ) { callable = d->boundExpression()->declaration(); continue; }
        
        if ( auto d = callable->as<ImportDeclaration              >() ) { ctx.error(*myExpressions.front()) << "cannot use import declaration in apply-expression"; return; }

        break;
    }

    if ( !hit ) {
        auto& err = ctx.error(*this) << "does not match any symbol declarations or procedure overloads";
        // todo: references to potential overloads
        // todo: chained symbol sets
        // todo: lookup failures are not returning symbol sets
        if ( hit.symSpace() ) {
            for ( auto const& p : hit.symSpace()->prototypes() )
                err.see(*p.proto.decl);
        }
        return;
    }

    myDeclaration = hit.decl();
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

    SymbolReference sym(myIdentifier.lexeme().c_str(), myExpressions);
    auto hit = ctx.matchCovariant(sym);
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
        return exprPrimary(r);
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
        return exprPrimary(r);
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
        auto first = true;
        for ( auto const& e : a.expressions() ) {
            if ( !first )
                stream << " ";
            else
                first = false;

            auto const group = e->kind() == Expression::Kind::Apply;
            if ( group )
                stream << "(";

            dispatch(*e);

            if ( group )
                stream << ")";
        }

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

    } // namespace ast
} // namespace kyfoo
