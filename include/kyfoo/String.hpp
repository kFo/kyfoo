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

struct Quoted {
    stringv v;
};

inline Quoted quoted(stringv v)
{
    return { v };
};

    namespace ascii {
        template <typename Sink>
        void write(Sink& stream, Quoted v)
        {
            stream.write('"');
            for ( auto c : v.v ) {
                switch (c) {
                case '\\':
                case '"':
                    stream.write('\\');
                    break;
                }
                stream.write(c);
            }
            stream.write('"');
        }
    }

} // namespace kyfoo
