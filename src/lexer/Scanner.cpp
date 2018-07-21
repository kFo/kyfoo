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
        case '-':
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
    indent_width_t current = 0;
    if ( !myIndents.empty() )
        current = myIndents.back();

    if ( indent == current )
        return Token(TokenKind::IndentEQ, "", loc);

    if ( indent > current ) {
        myIndents.push_back(indent);
        return Token(TokenKind::IndentGT, "", loc);
    }

    Token ret(TokenKind::IndentLT, "", loc);
    myIndents.pop_back();

    while ( !myIndents.empty() && myIndents.back() != indent ) {
        if ( myIndents.back() < indent ) {
            myError = true;
            return Token(TokenKind::IndentError, "", loc);
        }
        
        myBuffer.push_back(Token(TokenKind::IndentLT, "", loc));
        myIndents.pop_back();
    }

    if ( myIndents.empty() && indent != 0 ) {
        myError = true;
        return Token(TokenKind::IndentError, "", loc);
    }

    return ret;
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
#define TOK(t) Token(TokenKind::##t, lexeme, {myLoc.line, column})
#define TOK2(t, l) Token(TokenKind::##t, l, {myLoc.line, column})

    char c = nextChar();
    std::string lexeme;
    column_index_t column = myLoc.column;

    if ( myStream.eof() ) {
        return TOK(EndOfFile);
    }
    else if ( myStream.bad() ) {
        myError = true;
        return TOK2(Undefined, "");
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

    if ( myStream.eof() )
        return TOK(EndOfFile);

    if ( lineBreaks ) {
        do {
            spaces = takeSpaces();
            lineBreaks = takeLineBreaks();
        } while ( lineBreaks );

        if ( myStream.eof() )
            return TOK(EndOfFile);

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
        lexeme += c;
        while ( peekChar() != '\'' )
            lexeme += nextChar();

        lexeme += nextChar();

        return TOK(String);
    }
    else if ( c == '"' ) {
        lexeme += c;
        while ( peekChar() != '"' )
            lexeme += nextChar();

        lexeme += nextChar();

        return TOK(String);
    }
    else if ( c == '.' ) {
        if ( peekChar() == '.' ) {
            nextChar();
            return TOK2(DotDot, "..");
        }

        return TOK2(Dot, ".");
    }
    else if ( c == '=' ) {
        if ( peekChar() == '>' ) {
            nextChar();
            return TOK2(Yield, "=>");
        }

        return TOK2(Equal, "=");
    }
    else if ( c == ':' ) {
        switch ( peekChar() ) {
        case '|': nextChar(); return TOK2(ColonPipe     , ":|");
        case '&': nextChar(); return TOK2(ColonAmpersand, ":&");
        case '=': nextChar(); return TOK2(ColonEqual    , ":=");
        case '?': nextChar(); return TOK2(ColonQuestion , ":?");
        case '/': nextChar(); return TOK2(ColonSlash    , ":/");
        case '+': nextChar(); return TOK2(ColonPlus     , ":+");
        case '-': nextChar(); return TOK2(ColonMinus    , ":-");
        case '.': nextChar(); return TOK2(ColonDot      , ":.");
        case '<': nextChar();
                  addNest();  return TOK2(ColonOpenAngle, ":<");
        }

        return TOK2(Colon, ":");
    }
    else if ( isMetaVariable(c) ) {
        if ( !isIdentifierStart(peekChar()) )
            return TOK2(Undefined, "\\");

        do lexeme += nextChar();
        while ( isIdentifierMid(peekChar()) );

        return TOK(MetaVariable);
    }
    else if ( c == '-' ) {
        switch ( peekChar() ) {
        case '>': nextChar(); return TOK2(Arrow     , "->");
        case '-': nextChar(); return TOK2(MinusMinus, "--");
        }

        if ( isNumber(peekChar()) ) {
            lexeme += c;
            c = nextChar();
            goto L_lexNumber;
        }

        goto L_lexIdentifier;
    }
    else if ( isIdentifierStart(c) ) {
L_lexIdentifier:
        lexeme += c;
        while ( isIdentifierMid(peekChar()) )
            lexeme += nextChar();

        return Token(identifierKind(lexeme), lexeme, {myLoc.line, column});
    }
    else if ( isNumber(c) ) {
L_lexNumber:
        lexeme += c;
        while ( isNumber(peekChar()) )
            lexeme += nextChar();

        if ( peekChar() != '.' )
            return TOK(Integer);

        c = nextChar();
        if ( !isNumber(peekChar()) ) {
            putback(c);
            return TOK(Integer);
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
                    return TOK(Rational);
                }

                c = nextChar();
                if ( !isNumber(peekChar()) ) {
                    putback(e);
                    putback(c);
                    return TOK(Rational);
                }

                lexeme += e;
                lexeme += c;
            }
            else {
                lexeme += e;
            }

            do lexeme += nextChar();
            while ( isNumber(peekChar()) );

            return TOK(Rational);
        }

        return TOK(Rational);
    }

    // Single characters

    switch ( c ) {
    case '(': addNest(); return TOK2(OpenParen  , "(");
    case '[': addNest(); return TOK2(OpenBracket, "[");
    case '<': addNest(); return TOK2(OpenAngle  , "<");
    case ')': removeNest();  return TOK2(CloseParen  , ")");
    case ']': removeNest();  return TOK2(CloseBracket, "]");
    case '>': removeNest();  return TOK2(CloseAngle  , ">");
    case '{': return TOK2(OpenBrace   , "{");
    case '}': return TOK2(CloseBrace  , "}");
    case '|': return TOK2(Pipe        , "|");
    case ',': return TOK2(Comma       , ",");
    case '@': return TOK2(At          , "@");
    case ';': return TOK2(Semicolon   , ";");
    }

    myError = true;
    return TOK2(Undefined, "");

#undef TOK2
#undef TOK
}

} // namespace kyfoo::lexer
