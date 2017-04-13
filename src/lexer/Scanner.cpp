#include <kyfoo/lexer/Scanner.hpp>

#include <cassert>
#include <cctype>

#include <map>

namespace kyfoo {
    namespace lexer {

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

    TokenKind identifierKind(std::string const& lexeme)
    {
        static std::map<std::string, TokenKind> map;
        static struct _init_map { _init_map(std::map<std::string, TokenKind>& map) {
            for ( int i = (int)TokenKind::_keywordStart + 1; i != (int)TokenKind::_keywordEnd; ++i )
                map[toString((TokenKind)i)] = (TokenKind)i;
        } } init_map(map);

        auto e = map.find(lexeme);
        if ( e != map.end() )
            return e->second;
        
        return TokenKind::Identifier;
    }
}

Scanner::Scanner(std::istream& stream)
    : myStream(stream)
    , myState(InternalScanState{ 0, 0 })
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

Token Scanner::peek(std::size_t lookAhead)
{
    auto const peekTarget = myState.readIndex + lookAhead;
    if ( peekTarget < myBuffer.size() )
        return myBuffer[peekTarget];
    
    do myBuffer.push_back( readNext() );
    while ( peekTarget >= myBuffer.size() );

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

indent_width_t Scanner::indentWidth() const
{
    return myState.indentWidth;
}

bool Scanner::eof() const
{
    return myStream.eof();
}

bool Scanner::hasError() const
{
    return myError;
}

Scanner::operator bool() const
{
    return myStream && !myError;
}

char Scanner::nextChar()
{
    ++myColumn;
    return static_cast<char>(myStream.get());
}

char Scanner::peekChar()
{
    return static_cast<char>(myStream.peek());
}

void Scanner::bumpLine()
{
    myExpectingIndent = true;
    ++myLine;
    myColumn = 1;
}

Token Scanner::readNext()
{
#define TOK(t) Token(TokenKind::##t, myLine, column, lexeme)
#define TOK2(t, l) Token(TokenKind::##t, myLine, column, l)

    char c = peekChar();
    std::string lexeme;
    column_index_t column = myColumn;

    if ( myExpectingIndent ) {
        myExpectingIndent = false;

        if ( isSpace(c) ) {
            do lexeme += nextChar();
            while ( isSpace(peekChar()) );

            myState.indentWidth = lexeme.size();
            return TOK(Indent);
        }
        else {
            myState.indentWidth = 0;
        }
    }
    else {
        // Burn spaces if an indent is not expected
        while ( isSpace(c) ) {
            nextChar();
            c = peekChar();
        }
    }

    // Resync column with start of lexeme
    column = myColumn;

    if ( myStream.eof() ) {
        return TOK(EndOfFile);
    }
    else if ( isLetter(c) ) {
        do lexeme += nextChar();
        while ( isLetter(peekChar()) || isNumber(peekChar()) );

        return Token(identifierKind(lexeme), myLine, column, lexeme);
    }
    else if ( isNumber(c) ) {
        do lexeme += nextChar();
        while ( isNumber(peekChar()) );

        if ( peekChar() == '.' ) {
            c = nextChar();
            if ( !isNumber(peekChar()) ) {
                myStream.unget();

                return TOK(Integer);
            }

            lexeme += '.';
            do lexeme += nextChar();
            while ( isNumber(peekChar()) );

            c = peekChar();
            if ( c == 'e' || c == 'E' ) {
                char e = nextChar();
                c = peekChar();
                if ( !isNumber(c) ) {
                    if ( c != '-' && c != '+' ) {
                        myStream.unget(); // e

                        return TOK(Decimal);
                    }

                    c = nextChar();
                    if ( !isNumber(peekChar()) ) {
                        myStream.unget(); // e
                        myStream.unget(); // c

                        return TOK(Decimal);
                    }

                    lexeme += e;
                    lexeme += c;
                }
                else {
                    lexeme += e;
                }

                do lexeme += nextChar();
                while ( isNumber(peekChar()) );

                return TOK(Decimal);
            }

            return TOK(Decimal);
        }

        return TOK(Integer);
    }
    else if ( c == ';' ) {
        while ( !isLineBreak(c = nextChar()) )
            lexeme += c;

        if ( isLineBreak(peekChar()) )
            nextChar();

        bumpLine();

        return readNext();
    }
    else if ( c == '\'' ) {
        do lexeme += nextChar();
        while ( peekChar() != '\'' );

        lexeme += nextChar();

        return TOK(String);
    }
    else if ( c == '"' ) {
        do lexeme += nextChar();
        while ( peekChar() != '"' );

        lexeme += nextChar();

        return TOK(String);
    }
    else if ( c == '.' ) {
        c = nextChar();
        if ( peekChar() == '.' ) {
            c = nextChar();

            return TOK2(Range, "..");
        }

        return TOK2(Dot, ".");
    }
    else if ( c == '\r' ) {
        lexeme += nextChar();

        if ( peekChar() == '\n' )
            lexeme += nextChar();

        auto ret = TOK(LineBreak);
        bumpLine();

        return ret;
    }
    else if ( c == '\n' ) {
        nextChar();
        auto ret = TOK2(LineBreak, "\n");
        bumpLine();

        return ret;
    }
    else if ( c == '=' ) {
        c = nextChar();

        if ( peekChar() == '>' ) {
            nextChar();

            return TOK2(Yield, "=>");
        }

        return TOK2(Equal, "=");
    }

    // Single characters

    c = nextChar();
    switch ( c ) {
    case '(': return TOK2(OpenParen   , "(");
    case ')': return TOK2(CloseParen  , ")");
    case '[': return TOK2(OpenBracket , "[");
    case ']': return TOK2(CloseBracket, "]");
    case '<': return TOK2(OpenAngle   , "<");
    case '>': return TOK2(CloseAngle  , ">");
    case '{': return TOK2(OpenBrace   , "{");
    case '}': return TOK2(CloseBrace  , "}");
    case ':': return TOK2(Colon       , ":");
    case '|': return TOK2(Pipe        , "|");
    case ',': return TOK2(Comma       , ",");
    case '+': return TOK2(Plus        , "+");
    case '-': return TOK2(Minus       , "-");
    case '*': return TOK2(Star        , "*");
    case '/': return TOK2(Slash       , "/");
    }

    myError = true;
    return TOK2(EndOfFile, "");

#undef TOK2
#undef TOK
}

    } // namespace lexer
} // namespace kyfoo
