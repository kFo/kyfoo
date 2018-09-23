#pragma once

namespace kyfoo {

template <typename T>
class Interval
{
    T myLower;
    T myUpper;

public:
    Interval(T lower, T higher)
        : myLower(std::move(lower))
        , myUpper(std::move(higher))
    {
    }

public:
    T const& lower() const
    {
        return myLower;
    }

    T const& upper() const
    {
        return myUpper;
    }
};

template <typename T>
bool subset(Interval<T> const& lhs, Interval<T> const& rhs)
{
    return lhs.lower() >= rhs.lower() && lhs.upper() <= rhs.upper();
}

} // namespace kyfoo
