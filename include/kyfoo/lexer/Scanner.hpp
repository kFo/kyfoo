#pragma once

#include <deque>
#include <vector>

#include <kyfoo/allocators/AscendingAllocator.hpp>
#include <kyfoo/allocators/AscendingPageAllocator.hpp>
#include <kyfoo/allocators/Mallocator.hpp>
#include <kyfoo/allocators/Region.hpp>

#include <kyfoo/Allocators.hpp>
#include <kyfoo/Factory.hpp>
#include <kyfoo/Slice.hpp>
#include <kyfoo/lexer/Token.hpp>

namespace kyfoo::lexer {

using IndentWidth = uz;

/**
 * Non-ASCII meta characters
 *
 * Characters are 8-bit values in range [0, 256). ASCII values are 7-bit
 * values in range [0, 128). These meta-characters are non-ASCII
 * characters used as signals in ASCII character streams.
 */
enum MetaChar : unsigned char
{
    EndOfInput = 0b1111'1111,
};

template <typename T>
class Tokenizer
{
public:
    using Unit = char;
    using ParentView = Slice<char const>;
    using Window = Slice<char const>;
    using Pointer = ParentView::pointer;

public:
    Tokenizer() = default;

    explicit Tokenizer(ParentView buffer)
        : myWindow(buffer.data(), 1)
        , myEnd(buffer.end())
    {
    }

public:
    bool hasNext() const
    {
        return myWindow.begin() != myEnd;
    }

    Window next()
    {
        return take();
    }

public:
    Window take()
    {
        auto ret = window();
        bump();
        return ret;
    }

    Window window() const
    {
        return myWindow;
    }

    void bump()
    {
        myWindow = Window(myWindow.end(), 1);
    }

    template <uz N = 1u>
    void grow()
    {
        myWindow = Window(myWindow.data(), myWindow.card() + N);
    }

    void shrinkLeft()
    {
        myWindow.popFront();
    }

    Unit current() const
    {
        return myWindow.back();
    }

    template <uz N = 1u>
    Unit peek() const
    {
        auto end = myWindow.end() + N - 1;
        if ( myEnd <= end )
            return EndOfInput;

        return *end;
    }

    Unit peek(uz n) const
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
    Window myWindow;
    Pointer myEnd;
};

template <typename Allocator>
class TokenFactory : private AscendingFactory<Token, Allocator>
{
    using Base = AscendingFactory<Token, Allocator>;

public:
    explicit TokenFactory(uz reserve = 0)
        : Base(reserve ? reserve * sizeof(Token) : allocationGranularity())
    {
    }

    Token const& mkToken(TokenKind kind, stringv lexeme, SourceLocation loc)
    {
        return Base::mk(kind, lexeme, loc);
    }

    Token const& mkToken(TokenKind kind, SourceLocation loc)
    {
        return Base::mk(kind, loc);
    }
};

using DefaultTokenFactory = TokenFactory<
    allocators::AscendingAllocator<
        allocators::Region<allocators::AscendingPageAllocator, sizeof(Token)>,
        allocators::Mallocator>>;

class Scanner
{
public:
    explicit Scanner(DefaultTokenFactory& tokenFactory, Slice<char const> stream);
    explicit Scanner(DefaultTokenFactory& tokenFactory, std::deque<Token const*> buffer);

public:
    Scanner(Scanner const&) = delete;
    Scanner& operator = (Scanner const&) = delete;

public:
    Token const& next();
    Token const& peek(uz lookAhead = 0);

    void beginScan();
    void endScan();
    void rollbackScan();

    bool eof() const;
    bool hasError() const;

    explicit operator bool() const;

protected:
    Token const& readNext();

    Token const& indent(SourceLocation loc, IndentWidth indent);
    void bumpLine();

    void addNest();
    void removeNest();
    int nestings() const;

private:
    DefaultTokenFactory& myTokenFactory;
    Tokenizer<char const> myTok;

    struct InternalScanState
    {
        uz readIndex;
    };
    InternalScanState myState;

    std::vector<InternalScanState> mySavePoints;
    std::vector<IndentWidth> myIndents;
    std::deque<Token const*> myBuffer;

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
    Token const& next()
    {
        return myScanner.next();
    }

    Token const& peek(uz lookAhead = 0)
    {
        return myScanner.peek(lookAhead);
    }

private:
    Scanner& myScanner;
    bool myOpen = false;
};

} // namespace kyfoo::lexer
