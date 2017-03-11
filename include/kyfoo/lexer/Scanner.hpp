#pragma once

#include <deque>
#include <vector>

#include "Token.hpp"

namespace kyfoo {
    namespace lexer {

using indent_width_t = std::size_t;

class Scanner
{
public:
    explicit Scanner(std::istream&);

public:
    Scanner(Scanner const&) = delete;
    Scanner& operator = (Scanner const&) = delete;
    void swap(Scanner&) = delete;

public:
    Token next();
    Token peek(std::size_t = 0);

    void beginScan();
    void endScan();
    void rollbackScan();

    indent_width_t indentWidth() const;

    bool eof() const;
    bool hasError() const;

    operator bool() const;

protected:
    Token readNext();

    char nextChar();
    char peekChar();

    void bumpLine();

private:
    std::istream& myStream;

    struct InternalScanState
    {
        std::size_t readIndex;
        indent_width_t indentWidth;
    };
    InternalScanState myState;

    std::vector<InternalScanState> mySavePoints;
    std::deque<Token> myBuffer;

    bool myExpectingIndent = false;
    line_index_t myLine = 1;
    column_index_t myColumn = 1;
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

    Token peek(std::size_t lookAhead = 0)
    {
        return myScanner.peek(lookAhead);
    }

    indent_width_t indentWidth() const
    {
        return myScanner.indentWidth();
    }

private:
    Scanner& myScanner;
    bool myOpen = false;
};

    } // namespace lexer
} // namespace kyfoo
