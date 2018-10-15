#pragma once

#include <deque>
#include <vector>

#include <kyfoo/Slice.hpp>
#include <kyfoo/lexer/Token.hpp>

namespace kyfoo::lexer {

using indent_width_t = uz;

/**
 * Non-ASCII meta characters
 *
 * Characters are 8-bit values in range [0, 256). ASCII values are 7-bit
 * values in range [0, 128). These meta-characters are non-ASCII
 * characters used as signals in ASCII character streams.
 */
enum MetaChar : char
{
    EndOfInput = 0b1111'1111,
};

template <typename T>
class Tokenizer
{
public:
    using unit_t = char;
    using parent_view_t = Slice<char const>;
    using window_t = Slice<char const>;
    using pointer_t = parent_view_t::pointer;

public:
    Tokenizer() = default;

    explicit Tokenizer(parent_view_t buffer)
        : myWindow(buffer.data(), 1)
        , myEnd(buffer.end())
    {
    }

public:
    bool hasNext() const
    {
        return myWindow.begin() != myEnd;
    }

    window_t next()
    {
        return take();
    }

public:
    window_t take()
    {
        auto ret = window();
        bump();
        return ret;
    }

    window_t window() const
    {
        return myWindow;
    }

    void bump()
    {
        myWindow = window_t(myWindow.end(), 1);
    }

    template <uz N = 1u>
    void grow()
    {
        myWindow = window_t(myWindow.data(), myWindow.length() + N);
    }

    void shrinkLeft()
    {
        myWindow.popFront();
    }

    unit_t current() const
    {
        return myWindow.back();
    }

    template <uz N = 1u>
    unit_t peek() const
    {
        auto end = myWindow.end() + N - 1;
        if ( myEnd <= end )
            return EndOfInput;

        return *end;
    }

    unit_t peek(uz n) const
    {
        auto end = myWindow.end() + n - 1;
        if ( myEnd <= end )
            return EndOfInput;

        return *end;
    }

public:
    explicit operator bool () const
    {
        return hasNext();
    }

private:
    window_t myWindow;
    pointer_t myEnd;
};

class Scanner
{
public:
    explicit Scanner(Slice<char const> stream);
    explicit Scanner(std::deque<Token>&& buffer);

public:
    Scanner(Scanner const&) = delete;
    Scanner& operator = (Scanner const&) = delete;

public:
    Token next();
    Token peek(uz lookAhead = 0);

    void beginScan();
    void endScan();
    void rollbackScan();

    bool eof() const;
    bool hasError() const;

    explicit operator bool() const;

protected:
    Token readNext();

    Token indent(SourceLocation loc, indent_width_t indent);
    void bumpLine();

    void addNest();
    void removeNest();
    int nestings() const;

private:
    Tokenizer<char const> myTok;

    struct InternalScanState
    {
        uz readIndex;
    };
    InternalScanState myState;

    std::vector<InternalScanState> mySavePoints;
    std::vector<indent_width_t> myIndents;
    std::deque<Token> myBuffer;

    SourceLocation myLoc = { 1, 1 };
    int myNestings = 0;
    TokenKind myLastTokenKind = TokenKind::Undefined;
    TokenKind myCurrentTokenKind = TokenKind::Undefined;
    bool myError = false;
};

class ScanPoint
{
public:
    /*implicit*/ ScanPoint(Scanner& scanner)
        : myScanner(scanner)
    {
        myScanner.beginScan();
        myOpen = true;
    }

    ScanPoint(ScanPoint const& rhs)
        : ScanPoint(rhs.myScanner)
    {
    }

    void operator=(ScanPoint const&) = delete;

    ScanPoint(ScanPoint&&) = delete;
    void operator=(ScanPoint&&) = delete;

    ~ScanPoint()
    {
        if ( myOpen )
            myScanner.rollbackScan();
    }

public:
    bool commit()
    {
        myScanner.endScan();
        myOpen = false;

        return true;
    }

    void restart()
    {
        myScanner.rollbackScan();
        myScanner.beginScan();
        myOpen = true;
    }

public:
    Token next()
    {
        return myScanner.next();
    }

    Token peek(uz lookAhead = 0)
    {
        return myScanner.peek(lookAhead);
    }

private:
    Scanner& myScanner;
    bool myOpen = false;
};

} // namespace kyfoo::lexer
