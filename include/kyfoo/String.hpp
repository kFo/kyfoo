#pragma once

#include <ostream>
#include <string>

#include <kyfoo/Slice.hpp>

namespace kyfoo {

using stringv = Slice<char const>;

struct StringComp {
    struct is_transparent;

    bool operator()(std::string const& lhs, std::string const& rhs) const {
        return lhs < rhs;
    }

    bool operator()(stringv lhs, stringv rhs) const {
        return lhs < rhs;
    }

    bool equiv(std::string const& lhs, std::string const& rhs) const {
        return lhs == rhs;
    }

    bool equiv(stringv lhs, stringv rhs) const {
        return lhs == rhs;
    }
};

inline std::string mkString(stringv v) noexcept
{
    return std::string(begin(v), end(v));
}

int stoi(stringv s);

inline std::ostream& operator << (std::ostream& stream, stringv v)
{
    std::streamsize const card = v.card();
    auto fills = card < stream.width()
        ? stream.width() - card
        : 0;

    if ( stream.flags() & stream.right ) {
        for ( auto const c = stream.fill(); fills--; )
            stream.put(c);

        stream.width(0);
        return stream.write(v.data(), v.card());
    }

    stream.write(v.data(), v.card());
    if ( stream.flags() & stream.left ) {
        for ( auto const c = stream.fill(); fills--; )
            stream.put(c);
    }

    stream.width(0);
    return stream;
}

struct Quoted {
    stringv v;
};

inline Quoted quoted(stringv v)
{
    return { v };
};

inline std::ostream& operator << (std::ostream& stream, Quoted v)
{
    stream << '"';
    for ( auto c : v.v ) {
        switch (c) {
        case '\\':
        case '"':
            stream << '\\';
            break;
        }
        stream << c;
    }
    return stream << '"';
}

} // namespace kyfoo
