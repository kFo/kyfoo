#include <cctype>

#include <exception>

#include <kyfoo/Utilities.hpp>

namespace kyfoo {

int stoi(std::string_view s)
{
    auto first = cbegin(s);
    auto const last = cend(s);
    for ( ; first != last; ++first )
        if ( !std::isspace(*first) )
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
