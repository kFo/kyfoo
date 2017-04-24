#pragma once

#include <cstdlib>

#include <istream>
#include <memory>
#include <ostream>
#include <string>
#include <vector>

#include <kyfoo/lexer/Token.hpp>

namespace kyfoo {
    namespace ast {

#define PRIMITIVE_TYPES(X) \
    X(char) \
    X(unsigned char) \
    X(short) \
    X(unsigned short) \
    X(int) \
    X(unsigned int) \
    X(long) \
    X(unsigned long) \
    X(long long) \
    X(unsigned long long) \
    X(float) \
    X(double)

class IStream;
class IIO
{
public:
    virtual ~IIO() = default;

    virtual void io(IStream& stream) const = 0;
};

class IStream
{
public:
    virtual ~IStream() = default;

    virtual void openGroup(const char* name) = 0;
    virtual void closeGroup() = 0;

    virtual void openArray(const char* name) = 0;
    virtual void closeArray() = 0;

#define X(a) virtual void next(const char* name, a const& prim) = 0;
    PRIMITIVE_TYPES(X);
#undef X

    virtual void next(const char* name, std::string const& string) = 0;
    virtual void next(const char* name, const char* string) = 0;
    virtual void next(const char* name, IIO const& io) = 0;
    virtual void next(const char* name, IIO const* io) = 0;
    virtual void next(const char* name, lexer::Token const& token) = 0;

public:
    template <typename T>
    void next(const char* name, std::unique_ptr<T> const& p)
    {
        IIO* ptr = p.get();
        next(name, ptr);
    }

    template <typename T>
    void next(const char* name, std::vector<std::unique_ptr<T>> const & v)
    {
        openArray(name);
        for ( auto&& e : v )
            next("item", e);
        closeArray();
    }
};

class JsonOutput : public IStream
{
    std::ostream* myStream = nullptr;
    bool myNeedComma = false;
    std::string myIndent;

    enum Nest { Object, Array };
    std::vector<Nest> myNesting;

public:
    explicit JsonOutput(std::ostream& stream)
        : myStream(&stream)
    {
    }

public:
    void openGroup(const char* name) override
    {
        newLine();
        key(name);
        *myStream << "{\n";

        myNeedComma = false;
        myIndent += std::string(4, ' ');
        myNesting.push_back(Object);
    }

    void closeGroup() override
    {
        for ( int i = 0; i < 4; ++i )
            myIndent.pop_back();

        newLine();
        *myStream << "}";
        myNesting.pop_back();
    }

    void openArray(const char* name) override
    {
        newLine();
        key(name);
        *myStream << "[\n";

        myNeedComma = false;
        myIndent += std::string(4, ' ');
        myNesting.push_back(Array);
    }

    void closeArray() override
    {
        for ( int i = 0; i < 4; ++i )
            myIndent.pop_back();

        newLine();
        *myStream << "]";
        myNesting.pop_back();
    }

#define X(a) \
    void next(const char* name, a const& prim) override \
    { \
        newLine(); \
        key(name); \
        *myStream << prim; \
    }

    PRIMITIVE_TYPES(X)
#undef X

    void next(const char* name, std::string const& string) override
    {
        next(name, string.c_str());
    }

    void next(const char* name, const char* string) override
    {
        newLine();
        key(name);
        *myStream << "\"" << string << "\"";
    }

    void next(const char* name, IIO const* ptr) override
    {
        if ( ptr ) {
            next(name, *ptr);
        }
        else {
            newLine();
            key(name);
            *myStream << "{}";
        }
    }

    void next(const char* name, IIO const& rhs) override
    {
        openGroup(name);
        rhs.io(*this);
        closeGroup();
    }

    void next(const char* name, lexer::Token const& token) override
    {
        newLine();
        key(name);
        *myStream
            << "{ kind: " << toString(token.kind())
            << ", lexeme: \"" << token.lexeme() << "\""
            << ", line: " << token.line()
            << ", column: " << token.column() << " }";
    }

private:
    void newLine()
    {
        if ( myNeedComma )
            *myStream << ",\n";
        else
            myNeedComma = true;

        *myStream << myIndent;
    }

    void key(const char* name)
    {
        if ( !myNesting.empty() && myNesting.back() == Object )
            *myStream << name << ": ";
    }
};

    } // namespace ast
} // namespace kyfoo
