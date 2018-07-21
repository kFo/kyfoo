#pragma once

#include <deque>
#include <vector>

#include <kyfoo/lexer/Token.hpp>

namespace kyfoo::lexer {

using indent_width_t = uz;

class Scanner
{
public:
    explicit Scanner(std::istream&);
    explicit Scanner(std::deque<Token>&& buffer);

public:
    Scanner(Scanner const&) = delete;
    Scanner& operator = (Scanner const&) = delete;
    void swap(Scanner&) = delete;

public:
    Token next();
    Token peek(uz = 0);

    void beginScan();
    void endScan();
    void rollbackScan();

    bool eof() const;
    bool hasError() const;

    explicit operator bool() const;

protected:
    Token readNext();

    char nextChar();
    char peekChar();
    void putback(char c);

    Token indent(SourceLocation loc, indent_width_t indent);
    void bumpLine();

    void addNest();
    void removeNest();
    int nestings() const;

private:
    std::istream& myStream;
    std::deque<char> myCharBuffer;

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
