#pragma once

namespace kyfoo {

class Tribool
{
public:
    enum State
    {
        False,
        True,
        Indeterminate,
    };

public:
    constexpr Tribool() noexcept = default;
    constexpr /*implicit*/ Tribool(State rhs) noexcept : myState(rhs) {}
    constexpr /*implicit*/ Tribool(bool rhs) noexcept : myState(!rhs ? False : True) {}

    constexpr Tribool& operator = (State rhs) noexcept
    {
        myState = rhs;
        return *this;
    }

    constexpr Tribool& operator = (bool rhs) noexcept
    {
        myState = !rhs ? False : True;
        return *this;
    }

public:
    constexpr bool operator == (Tribool rhs) const
    {
        return myState == rhs.myState;
    }

    constexpr bool operator != (Tribool rhs) const
    {
        return !operator==(rhs);
    }

    constexpr explicit operator bool () const
    {
        return myState == True;
    }

    constexpr Tribool operator ~ () const
    {
        const State fn[] = { True, False, Indeterminate };
        return fn[myState];
    }

public:
    constexpr Tribool& operator |= (Tribool rhs)
    {
        if ( myState == False )
            return *this = rhs;

        if ( myState == True )
            return *this;

        if ( rhs.myState == True )
            return *this = rhs;

        return *this = Indeterminate;
    }

    constexpr Tribool& operator &= (Tribool rhs)
    {
        if ( myState == False )
            return *this;

        if ( myState == True || rhs.myState == False )
            return *this = rhs;

        return *this = Indeterminate;
    }

    constexpr Tribool& operator ^= (Tribool rhs)
    {
        if ( myState == Indeterminate || rhs.myState == Indeterminate )
            return *this = Indeterminate;

        return *this = myState != rhs.myState;
    }

private:
    State myState = Indeterminate;
};

constexpr Tribool operator | (Tribool lhs, Tribool rhs) noexcept { return lhs |= rhs; }
constexpr Tribool operator & (Tribool lhs, Tribool rhs) noexcept { return lhs &= rhs; }
constexpr Tribool operator ^ (Tribool lhs, Tribool rhs) noexcept { return lhs ^= rhs; }

} // namespace kyfoo
