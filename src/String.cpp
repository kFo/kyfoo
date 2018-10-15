#include <kyfoo/String.hpp>

#include <exception>

namespace kyfoo {

namespace {
    bool isSpace(char c)
    {
        switch (c) {
        case ' ':
        case '\f':
        case '\n':
        case '\r':
        case '\t':
        case '\v':
            return true;

        default:
            return false;
        }
    }
} // namespace

int stoi(stringv s)
{
    auto first = cbegin(s);
    auto const last = cend(s);
    for ( ; first != last; ++first )
        if ( !isSpace(*first) )
            break;

    if ( first == last )
        throw std::invalid_argument("stoi argument invalid");

    int ret = 0;
    for ( ; first != last; ++first ) {
        if ( *first < '0' )
            goto L_outOfRange;

        auto digit = *first - '0';
        if ( digit > 9 )
            goto L_outOfRange;

        ret *= 10;
        ret += digit;
    }

    return ret;

L_outOfRange:
    throw std::out_of_range("stoi argument out of range");
}

} // namespace kyfoo
