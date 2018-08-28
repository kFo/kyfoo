#include <kyfoo/lexer/Scanner.hpp>

#include <cassert>
#include <cctype>

#include <sstream>

#include <map>

namespace kyfoo::lexer {

namespace
{
    std::istringstream g_nullStream;

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
        case '/':
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

    bool isMetaVariable(char c)
    {
        return c == '\\';
    }

    TokenKind identifierKind(std::string_view lexeme)
    {
        static std::map<std::string_view, TokenKind> map;
        static struct _init_map { _init_map(std::map<std::string_view, TokenKind>& map) {
            for ( int i = (int)TokenKind::_keywordStart + 1; i != (int)TokenKind::_keywordEnd; ++i )
                map[to_string((TokenKind)i)] = (TokenKind)i;
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
        }

        return false;
    }
}

Scanner::Scanner(std::istream& stream)
    : myStream(stream)
    , myState(InternalScanState{ 0 })
{
}

Scanner::Scanner(std::deque<Token>&& buffer)
    : myStream(g_nullStream)
    , myState(InternalScanState{ 0 })
    , myBuffer(std::move(buffer))
{
}

Token Scanner::next()
{
    if ( !mySavePoints.empty() ) {
        if ( myState.readIndex + 1 >= myBuffer.size() )
            peek(myState.readIndex + 1);
    }

    if ( !myBuffer.empty() ) {
        auto ret = myBuffer[myState.readIndex];
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

Token Scanner::peek(uz lookAhead)
{
    auto const peekTarget = myState.readIndex + lookAhead;
    if ( peekTarget < myBuffer.size() )
        return myBuffer[peekTarget];
    
    do {
        auto next = readNext();
        if ( next.kind() == TokenKind::EndOfFile )
            return next;

        myBuffer.push_back(next);
    } while ( peekTarget >= myBuffer.size() );

    return myBuffer[peekTarget];
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
    return myStream.eof() && myBuffer.empty();
}

bool Scanner::hasError() const
{
    return myError;
}

Scanner::operator bool() const
{
    return !eof() && !myError;
}

char Scanner::nextChar()
{
    ++myLoc.column;
    if ( myCharBuffer.empty() )
        return static_cast<char>(myStream.get());

    auto ret = myCharBuffer.front();
    myCharBuffer.pop_front();
    return ret;
}

char Scanner::peekChar()
{
    if ( myCharBuffer.empty() ) {
        auto ret = static_cast<char>(myStream.get());
        myCharBuffer.push_back(ret);
        return ret;
    }

    return myCharBuffer.front();
}

void Scanner::putback(char c)
{
    --myLoc.column;
    myCharBuffer.push_front(c);
}

Token Scanner::indent(SourceLocation loc, indent_width_t indent)
{
    auto token = [this, &loc](TokenKind kind) {
        myLastTokenKind = kind;
        return Token(kind, "", loc);
    };

    indent_width_t current = 0;
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
        
        myBuffer.push_back(Token(TokenKind::IndentLT, "", loc));
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

Token Scanner::readNext()
{
#define TryEmitVacuum(c)                               \
    if ( vacuum && precedesVacuum(myLastTokenKind) ) { \
        putback(c);                                    \
        return token2(TokenKind::Vacuum, "");          \
    }

    char c = nextChar();
    std::string lexeme;
    column_index_t column = myLoc.column;

    auto token = [&, this](TokenKind kind) {
        myLastTokenKind = kind;
        return Token(kind, lexeme, {myLoc.line, column});
    };

    auto token2 = [&, this](TokenKind kind, std::string const& l) {
        myLastTokenKind = kind;
        return Token(kind, l, {myLoc.line, column});
    };

    if ( myStream.eof() ) {
        return token(TokenKind::EndOfFile);
    }
    else if ( myStream.bad() ) {
        myError = true;
        return token2(TokenKind::Undefined, "");
    }
    else if ( myStream.fail() ) {
        throw std::runtime_error("lexing failure");
    }

    auto takeLineBreaks = [this, &c] {
        int ret = 0;
        while ( isLineBreak(c) ) {
            if ( c == '\r' && peekChar() == '\n' )
                nextChar();

            c = nextChar();
            bumpLine();
            ++ret;
        }

        return ret;
    };

    auto takeSpaces = [this, &c, &takeLineBreaks] {
        indent_width_t ret = 0;

    L_more:
        indent_width_t spaces = 0;
        while ( isSpace(c) ) {
            ++spaces; c = nextChar();
        }

        if ( isLineComment(c, peekChar()) ) {
            ++spaces; nextChar();
            do {
                ++spaces; c = nextChar();
            } while ( !isLineBreak(c) );
        }

        if ( isMultiLineCommentStart(c, peekChar()) ) {
            int open = 1;
            ++spaces; nextChar();
            ++spaces; c = nextChar();
            while ( open ) {
                if ( myStream.eof() )
                    break;

                if ( isLineBreak(c) )
                    takeLineBreaks();

                if ( isMultiLineCommentStart(c, peekChar()) ) {
                    ++open;
                    ++spaces; nextChar();
                }
                else if ( isMultiLineCommentEnd(c, peekChar()) ) {
                    --open;
                    ++spaces; nextChar();
                }

                ++spaces; c = nextChar();
            }
        }

        ret += spaces;
        if ( spaces ) goto L_more;

        return ret;
    };

    auto spaces = takeSpaces();
    auto lineBreaks = takeLineBreaks();
    bool vacuum = !spaces && !lineBreaks;

    if ( myStream.eof() )
        return token(TokenKind::EndOfFile);

    if ( lineBreaks ) {
        do {
            spaces = takeSpaces();
            lineBreaks = takeLineBreaks();
        } while ( lineBreaks );

        if ( myStream.eof() )
            return token(TokenKind::EndOfFile);

        if ( !nestings() ) {
            putback(c);
            return indent(myLoc, spaces);
        }
    }
    else if ( !nestings() && spaces && column == 1 ) {
        putback(c);
        return indent(myLoc, spaces);
    }

    // Resync column with start of lexeme
    column = myLoc.column;

    if ( c == '\'' ) {
        TryEmitVacuum(c)

        lexeme += c;
        while ( peekChar() != '\'' )
            lexeme += nextChar();

        lexeme += nextChar();

        return token(TokenKind::String);
    }
    else if ( c == '"' ) {
        TryEmitVacuum(c)

        lexeme += c;
        while ( peekChar() != '"' )
            lexeme += nextChar();

        lexeme += nextChar();

        return token(TokenKind::String);
    }
    else if ( c == '.' ) {
        if ( peekChar() == '.' ) {
            nextChar();
            return token2(TokenKind::DotDot, "..");
        }

        return token2(TokenKind::Dot, ".");
    }
    else if ( c == '=' ) {
        if ( peekChar() == '>' ) {
            TryEmitVacuum(c)
            nextChar();
            return token2(TokenKind::Yield, "=>");
        }

        return token2(TokenKind::Equal, "=");
    }
    else if ( c == ':' ) {
        switch ( peekChar() ) {
        case '|': nextChar(); return token2(TokenKind::ColonPipe     , ":|");
        case '&': nextChar(); return token2(TokenKind::ColonAmpersand, ":&");
        case '=': nextChar(); return token2(TokenKind::ColonEqual    , ":=");
        case '*': nextChar(); if ( peekChar() == '<' ) { nextChar(); addNest(); return token2(TokenKind::ColonStarAngle    , ":*<"); } return token2(TokenKind::ColonStar     , ":*");
        case '?': nextChar(); if ( peekChar() == '<' ) { nextChar(); addNest(); return token2(TokenKind::ColonQuestionAngle, ":?<"); } return token2(TokenKind::ColonQuestion , ":?");
        case '/': nextChar(); if ( peekChar() == '<' ) { nextChar(); addNest(); return token2(TokenKind::ColonSlashAngle   , ":/<"); } return token2(TokenKind::ColonSlash    , ":/");
        case '+': nextChar(); return token2(TokenKind::ColonPlus     , ":+");
        case '-': nextChar(); return token2(TokenKind::ColonMinus    , ":-");
        case '.': nextChar(); return token2(TokenKind::ColonDot      , ":.");
        case '<': nextChar();
                  addNest();  return token2(TokenKind::ColonOpenAngle, ":<");
        }

        return token2(TokenKind::Colon, ":");
    }
    else if ( isMetaVariable(c) ) {
        if ( !isIdentifierStart(peekChar()) )
            return token2(TokenKind::Undefined, "\\");

        TryEmitVacuum(c)

        do lexeme += nextChar();
        while ( isIdentifierMid(peekChar()) );

        return token(TokenKind::MetaVariable);
    }
    else if ( c == '-' ) {
        switch ( peekChar() ) {
        case '-': nextChar(); return token2(TokenKind::MinusMinus, "--");
        case '>':
            TryEmitVacuum(c)
            nextChar();
            return token2(TokenKind::Arrow     , "->");
        }

        if ( vacuum )
            return token2(TokenKind::Hyphen, "-");

        if ( isNumber(peekChar()) ) {
            lexeme += c;
            c = nextChar();
            goto L_lexNumber;
        }

        goto L_undefined;
    }
    else if ( isIdentifierStart(c) ) {
        TryEmitVacuum(c)

        lexeme += c;
        while ( isIdentifierMid(peekChar()) )
            lexeme += nextChar();

        return token(identifierKind(lexeme));
    }
    else if ( isNumber(c) ) {
        TryEmitVacuum(c)
L_lexNumber:
        lexeme += c;
        while ( isNumber(peekChar()) )
            lexeme += nextChar();

        if ( peekChar() != '.' )
            return token(TokenKind::Integer);

        c = nextChar();
        if ( !isNumber(peekChar()) ) {
            putback(c);
            return token(TokenKind::Integer);
        }

        lexeme += '.';
        while ( isNumber(peekChar()) )
            lexeme += nextChar();

        char e = peekChar();
        if ( e == 'e' || e == 'E' ) {
            nextChar();
            c = peekChar();
            if ( !isNumber(c) ) {
                if ( c != '-' && c != '+' ) {
                    putback(e);
                    return token(TokenKind::Rational);
                }

                c = nextChar();
                if ( !isNumber(peekChar()) ) {
                    putback(e);
                    putback(c);
                    return token(TokenKind::Rational);
                }

                lexeme += e;
                lexeme += c;
            }
            else {
                lexeme += e;
            }

            do lexeme += nextChar();
            while ( isNumber(peekChar()) );

            return token(TokenKind::Rational);
        }

        return token(TokenKind::Rational);
    }

    // Single characters

    switch ( c ) {
    case '(':
    case '[':
    case '<':
        TryEmitVacuum(c)
    }

    switch ( c ) {
    case '(': addNest(); return token2(TokenKind::OpenParen  , "(");
    case '[': addNest(); return token2(TokenKind::OpenBracket, "[");
    case '<': addNest(); return token2(TokenKind::OpenAngle  , "<");
    case ')': removeNest(); return token2(TokenKind::CloseParen  , ")");
    case ']': removeNest(); return token2(TokenKind::CloseBracket, "]");
    case '>': removeNest(); return token2(TokenKind::CloseAngle  , ">");
    case '{': return token2(TokenKind::OpenBrace   , "{");
    case '}': return token2(TokenKind::CloseBrace  , "}");
    case '|': return token2(TokenKind::Pipe        , "|");
    case ',': return token2(TokenKind::Comma       , ",");
    case '@': return token2(TokenKind::At          , "@");
    case ';': return token2(TokenKind::Semicolon   , ";");
    }

L_undefined:
    myError = true;
    return token2(TokenKind::Undefined, "");

#undef TryEmitVacuum
}

} // namespace kyfoo::lexer
