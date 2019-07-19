#include <kyfoo/lexer/Scanner.hpp>

#include <cassert>

#include <map>

#include <kyfoo/String.hpp>

namespace kyfoo::lexer {

namespace
{
    bool isSpace(char c)
    {
        switch (c)
        {
        case ' ':
        case '\t':
            return true;
        }

        return false;
    }

    bool isLineComment(char a, char b)
    {
        return a == '/' && b == '/';
    }

    bool isMultiLineCommentStart(char a, char b)
    {
        return a == '/' && b == '*';
    }

    bool isMultiLineCommentEnd(char a, char b)
    {
        return isMultiLineCommentStart(b, a);
    }

    bool isLineBreak(char c)
    {
        switch (c)
        {
        case '\r':
        case '\n':
            return true;
        }

        return false;
    }

    bool isLetter(char c)
    {
        if ( 'a' <= c && c <= 'z' )
            return true;

        if ( 'A' <= c && c <= 'Z' )
            return true;

        return false;
    }

    bool isNumber(char c)
    {
        if ( '0' <= c && c <= '9' )
            return true;

        return false;
    }

    bool isIdentifierStart(char c)
    {
        switch ( c ) {
        case '_':
        case '+':
        case '*':
        case '\\':
            return true;
        default:
            return isLetter(c);
        }
    }

    bool isIdentifierMid(char c)
    {
        return isIdentifierStart(c) || isNumber(c);
    }

    TokenKind identifierKind(stringv lexeme)
    {
        static std::map<stringv, TokenKind> map;
        static struct _init_map { _init_map(std::map<stringv, TokenKind>& map) {
            for ( auto i = int(TokenKind::_keywordStart) + 1; i != int(TokenKind::_keywordEnd); ++i )
                map[to_string(TokenKind(i))] = TokenKind(i);
        } } init_map(map);

        auto e = map.find(lexeme);
        if ( e != map.end() )
            return e->second;
        
        return TokenKind::Identifier;
    }

    bool precedesVacuum(TokenKind kind)
    {
        switch (kind) {
        case TokenKind::Identifier:
        case TokenKind::MetaVariable:
        case TokenKind::Integer:
        case TokenKind::Rational:
        case TokenKind::String:
        case TokenKind::CloseParen:
        case TokenKind::CloseBracket:
        case TokenKind::CloseAngle:
        case TokenKind::CloseBrace:
            return true;

        default:
            return false;
        }
    }
} // namespace

Scanner::Scanner(DefaultTokenFactory& tokenFactory, Slice<char const> stream)
    : myTokenFactory(tokenFactory)
    , myTok(stream)
    , myState(InternalScanState{ 0 })
{
}

Scanner::Scanner(DefaultTokenFactory& tokenFactory, std::deque<Token const*> buffer)
    : myTokenFactory(tokenFactory)
    , myState(InternalScanState{ 0 })
    , myBuffer(std::move(buffer))
{
}

Token const& Scanner::next()
{
    if ( !mySavePoints.empty() ) {
        if ( myState.readIndex + 1 >= myBuffer.size() )
            peek(myState.readIndex + 1);
    }

    if ( !myBuffer.empty() ) {
        auto const& ret = *myBuffer[myState.readIndex];
        if ( mySavePoints.empty() ) {
            assert(myState.readIndex == 0);
            myBuffer.pop_front();
        }
        else {
            ++myState.readIndex;
        }

        return ret;
    }

    return readNext();
}

Token const& Scanner::peek(uz lookAhead)
{
    auto const peekTarget = myState.readIndex + lookAhead;
    if ( peekTarget < myBuffer.size() )
        return *myBuffer[peekTarget];
    
    do {
        auto const& next = readNext();
        if ( next.kind() == TokenKind::EndOfInput )
            return next;

        myBuffer.emplace_back(&next);
    } while ( peekTarget >= myBuffer.size() );

    return *myBuffer[peekTarget];
}

void Scanner::beginScan()
{
    mySavePoints.push_back(myState);
}

void Scanner::endScan()
{
    mySavePoints.pop_back();

    if ( mySavePoints.empty() ) {
        myBuffer.erase(begin(myBuffer), begin(myBuffer) + myState.readIndex);
        myState.readIndex = 0;
    }
}

void Scanner::rollbackScan()
{
    myState = mySavePoints.back();
    mySavePoints.pop_back();
}

bool Scanner::eof() const
{
    return !myTok && myBuffer.empty();
}

bool Scanner::hasError() const
{
    return myError;
}

Scanner::operator bool() const
{
    return !eof() && !myError;
}

Token const& Scanner::indent(SourceLocation loc, IndentWidth indent)
{
    auto token = [this, &loc](TokenKind kind) -> Token const& {
        myLastTokenKind = kind;
        return myTokenFactory.mkToken(kind, loc);
    };

    IndentWidth current = 0;
    if ( !myIndents.empty() )
        current = myIndents.back();

    if ( indent == current )
        return token(TokenKind::IndentEQ);

    if ( indent > current ) {
        myIndents.push_back(indent);
        return token(TokenKind::IndentGT);
    }

    myIndents.pop_back();

    while ( !myIndents.empty() && myIndents.back() != indent ) {
        if ( myIndents.back() < indent ) {
            myError = true;
            return token(TokenKind::IndentError);
        }
        
        myBuffer.emplace_back(&myTokenFactory.mkToken(TokenKind::IndentLT, loc));
        myIndents.pop_back();
    }

    if ( myIndents.empty() && indent != 0 ) {
        myError = true;
        return token(TokenKind::IndentError);
    }

    return token(TokenKind::IndentLT);
}

void Scanner::bumpLine()
{
    ++myLoc.line;
    myLoc.column = 1;
}

void Scanner::addNest()
{
    ++myNestings;
}

void Scanner::removeNest()
{
    --myNestings;
}

int Scanner::nestings() const
{
    return myNestings;
}

Token const& Scanner::readNext()
{
    if ( myCurrentTokenKind != TokenKind::Undefined ) {
        // myCurrentTokenKind indicates the next token is already lexed.
        // This happens when virtual tokens are emitted in place of
        // already lexed tokens.

        myLastTokenKind = myCurrentTokenKind;
        myCurrentTokenKind = TokenKind::Undefined;
        auto loc = myLoc;
        myLoc.column += trunc<decltype(myLoc.column)>(myTok.window().card());
        return myTokenFactory.mkToken(myLastTokenKind, myTok.take(), loc);
    }

    bool emitVacuum = false;
    auto token = [&, this](TokenKind kind) -> Token const& {
        myLastTokenKind = kind;
        if ( emitVacuum ) {
            myCurrentTokenKind = kind;
            return myTokenFactory.mkToken(TokenKind::Vacuum, myLoc);
        }

        auto loc = myLoc;
        myLoc.column += trunc<decltype(myLoc.column)>(myTok.window().card());
        return myTokenFactory.mkToken(kind, myTok.take(), loc);
    };

    IndentWidth spaces = 0;
    bool freshline = false;

    if ( false ) {
    L_ignoreToken:
        if ( !isLineBreak(myTok.current()) )
            myLoc.column += trunc<decltype(myLoc.column)>(myTok.window().card());
        myTok.bump();
    }

    if ( !myTok ) {
        if ( !myIndents.empty() )
            return indent(myLoc, 0);

        // todo: cache eoi
        return myTokenFactory.mkToken(TokenKind::EndOfInput, myLoc);
    }

    auto takeLineBreaks = [this] {
        switch ( myTok.current() ) {
        case '\r': if ( myTok.peek() == '\n' ) myTok.grow();
        case '\n': bumpLine();
        L_anotherLineBreak:
            switch (myTok.peek()) {
            case '\r': if ( myTok.peek<2>() == '\n' ) myTok.grow();
            case '\n':
                myTok.grow();
                bumpLine();
                goto L_anotherLineBreak;
            }

            return true;
        }

        return false;
    };

    if ( takeLineBreaks() ) {
        spaces = 0;
        freshline = true;
        goto L_ignoreToken;
    }

    if ( isSpace(myTok.current()) ) {
        while ( isSpace(myTok.peek()) )
            myTok.grow();

        spaces += myTok.window().card();
        goto L_ignoreToken;
    }

    if ( isLineComment(myTok.current(), myTok.peek()) ) {
        do {
            myTok.grow();
        } while ( myTok && !isLineBreak(myTok.peek()) );

        goto L_ignoreToken;
    }

    if ( isMultiLineCommentStart(myTok.current(), myTok.peek()) ) {
        myTok.grow();
        do {
            myTok.grow();
            takeLineBreaks();
        } while ( myTok && !isMultiLineCommentEnd(myTok.current(), myTok.peek()) );

        if ( !myTok ) {
            // todo: emit lexer error
            return token(TokenKind::Undefined);
        }

        myTok.grow();

        goto L_ignoreToken;
    }

    bool vacuum = spaces == 0;
    auto tryEmitVacuum = [&emitVacuum, &vacuum, this] {
        emitVacuum = vacuum && precedesVacuum(myLastTokenKind);
    };

    if ( !nestings() && (freshline || (spaces && myLoc.column == 1)) )
        return indent(myLoc, spaces);

    switch ( myTok.current() ) {
    case '\'':
        tryEmitVacuum();

        while ( myTok.peek() != '\'' )
            myTok.grow();

        myTok.grow();
        return token(TokenKind::String);

    case '"':
        tryEmitVacuum();

        while ( myTok.peek() != '"' )
            myTok.grow();

        myTok.grow();
        return token(TokenKind::String);

    case '.':
        if ( myTok.peek() == '.' ) {
            myTok.grow();
            return token(TokenKind::DotDot);
        }

        return token(TokenKind::Dot);

    case '=':
        if ( myTok.peek() == '>' ) {
            tryEmitVacuum();
            myTok.grow();
            return token(TokenKind::Yield);
        }

        return token(TokenKind::Equal);

    case ':':
        switch ( myTok.peek() ) {
        case '|': myTok.grow(); return token(TokenKind::ColonPipe     );
        case '&': myTok.grow(); return token(TokenKind::ColonAmpersand);
        case '=': myTok.grow(); return token(TokenKind::ColonEqual    );
        case '*': myTok.grow(); if ( myTok.peek() == '<' ) { myTok.grow(); addNest(); return token(TokenKind::ColonStarAngle    ); } return token(TokenKind::ColonStar    );
        case '?': myTok.grow(); if ( myTok.peek() == '<' ) { myTok.grow(); addNest(); return token(TokenKind::ColonQuestionAngle); } return token(TokenKind::ColonQuestion);
        case '/': myTok.grow(); if ( myTok.peek() == '<' ) { myTok.grow(); addNest(); return token(TokenKind::ColonSlashAngle   ); } return token(TokenKind::ColonSlash   );
        case '+': myTok.grow(); return token(TokenKind::ColonPlus );
        case '-': myTok.grow(); return token(TokenKind::ColonMinus);
        case '.': myTok.grow(); return token(TokenKind::ColonDot  );
        case '<': myTok.grow();
                  addNest();  return token(TokenKind::ColonOpenAngle);
        }

        return token(TokenKind::Colon);

    case '\\':
        if ( !isIdentifierStart(myTok.peek()) )
            return token(TokenKind::Undefined);

        tryEmitVacuum();

        do myTok.grow();
        while ( isIdentifierMid(myTok.peek()) );

        myTok.shrinkLeft(); // removes '\' from start of lexeme
        return token(TokenKind::MetaVariable);

    case '-':
        switch ( myTok.peek() ) {
        case '-': myTok.grow(); return token(TokenKind::MinusMinus);
        case '>':
            tryEmitVacuum();
            myTok.grow();
            return token(TokenKind::Arrow);
        }

        if ( vacuum )
            return token(TokenKind::Hyphen);

        if ( isNumber(myTok.peek()) ) {
            myTok.grow();
            goto L_lexNumber;
        }

        goto L_undefined;
    }
    
    if ( isIdentifierStart(myTok.current()) ) {
        tryEmitVacuum();

        while ( isIdentifierMid(myTok.peek()) )
            myTok.grow();

        return token(identifierKind(myTok.window()));
    }
    else if ( isNumber(myTok.current()) ) {
        tryEmitVacuum();
L_lexNumber:
        while ( isNumber(myTok.peek()) )
            myTok.grow();

        if ( myTok.peek() != '.' )
            return token(TokenKind::Integer);

        if ( !isNumber(myTok.peek<2>()) )
            return token(TokenKind::Integer);

        myTok.grow<2>();
        while ( isNumber(myTok.peek()) )
            myTok.grow();

        char e = myTok.peek();
        if ( e == 'e' || e == 'E' ) {
            myTok.grow();
            auto s = myTok.peek<2>();
            if ( !isNumber(s) ) {
                if ( s != '-' && s != '+' )
                    return token(TokenKind::Rational);

                if ( !isNumber(myTok.peek<3>()) )
                    return token(TokenKind::Rational);

                myTok.grow<3>();
            }
            else {
                myTok.grow<2>();
            }

            while ( isNumber(myTok.peek()) )
                myTok.grow();

            return token(TokenKind::Rational);
        }

        return token(TokenKind::Rational);
    }

    // Single characters

    switch ( myTok.current() ) {
    case '(':
    case '[':
    case '<':
        tryEmitVacuum();
    }

    switch ( myTok.current() ) {
    case '(': addNest(); return token(TokenKind::OpenParen  );
    case '[': addNest(); return token(TokenKind::OpenBracket);
    case '<': addNest(); return token(TokenKind::OpenAngle  );
    case ')': removeNest(); return token(TokenKind::CloseParen  );
    case ']': removeNest(); return token(TokenKind::CloseBracket);
    case '>': removeNest(); return token(TokenKind::CloseAngle  );
    case '{': return token(TokenKind::OpenBrace );
    case '}': return token(TokenKind::CloseBrace);
    case '|': return token(TokenKind::Pipe      );
    case ',': return token(TokenKind::Comma     );
    case '@': return token(TokenKind::At        );
    case ';': return token(TokenKind::Semicolon );
    case '?': return token(TokenKind::Question  );
    case '/': return token(TokenKind::Slash     );
    }

L_undefined:
    myError = true;
    return token(TokenKind::Undefined);

#undef TryEmitVacuum
}

} // namespace kyfoo::lexer
