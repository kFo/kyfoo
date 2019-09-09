#pragma once

#include <cassert>

#include <algorithm>

#include <kyfoo/Array.hpp>
#include <kyfoo/Types.hpp>

namespace kyfoo {

template <typename T>
struct bit_count
{
    enum { value = sizeof(T) * 8 };
};

template <typename T>
inline constexpr uz bit_count_v = bit_count<T>::value;

class BigInt
{
public:
    using Limb = u32;
    using DoubleLimb = u64;

    enum Sign
    {
        Positive,
        Negative,
    };

    enum Ordering
    {
        LT,
        EQ,
        GT,
    };

public:
    explicit BigInt(u64 rhs)
    {
        static_assert(sizeof(rhs) >= sizeof(Limb));

        auto const card = sizeof(rhs) / sizeof(Limb);
        myLimbs.appendRange(Slice{reinterpret_cast<Limb*>(&rhs), card});
        normalize();
    }

public:
    bool operator<(BigInt const& rhs) const
    {
        if ( mySign == Positive ) {
            if ( rhs.mySign == Negative )
                return false;
        }
        else if ( rhs.mySign == Positive ) {
            return true;
        }

        assert(mySign == rhs.mySign);

        return magCmp(rhs) == LT;
    }

    bool operator<=(BigInt const& rhs) const
    {
        if ( mySign == Positive ) {
            if ( rhs.mySign == Negative )
                return false;
        }
        else if ( rhs.mySign == Positive ) {
            return true;
        }

        assert(mySign == rhs.mySign);

        switch ( magCmp(rhs) ) {
        case LT:
        case EQ:
            return true;

        default:
            return false;
        }
    }

    bool operator>(BigInt const& rhs) const
    {
        if ( mySign == Positive ) {
            if ( rhs.mySign == Negative )
                return true;
        }
        else if ( rhs.mySign == Positive ) {
            return false;
        }

        assert(mySign == rhs.mySign);

        return magCmp(rhs) == GT;
    }

    bool operator>=(BigInt const& rhs) const
    {
        if ( mySign == Positive ) {
            if ( rhs.mySign == Negative )
                return true;
        }
        else if ( rhs.mySign == Positive ) {
            return false;
        }

        assert(mySign == rhs.mySign);

        switch ( magCmp(rhs) ) {
        case EQ:
        case GT:
            return true;

        default:
            return false;
        }
    }

    bool operator==(BigInt const& rhs) const
    {
        return mySign == rhs.mySign
            && magCmp(rhs) == EQ;
    }

    bool operator!=(BigInt const& rhs) const
    {
        return !operator==(rhs);
    }

    BigInt& operator+=(BigInt const& rhs)
    {
        if ( mySign == rhs.mySign )
            magAdd(rhs);
        else
            magSub(rhs);

        return *this;
    }

    BigInt& operator-=(BigInt const& rhs)
    {
        if ( mySign == rhs.mySign )
            magSub(rhs);
        else
            magAdd(rhs);

        return *this;
    }

    BigInt& operator<<=(uz rhs)
    {
        auto constexpr limbBits = bit_count_v<Limb>;

        uz const offset = rhs / limbBits;
        uz const shift = rhs % limbBits;
        uz const invShift = limbBits - shift;

        myLimbs.appendRange(Replicate<u32>(offset));
        for ( uz i = myLimbs.card() - 1; i > offset; --i ) {
            auto s = myLimbs[i - offset] << shift;
            auto ss = myLimbs[i - offset - 1] >> invShift;
            myLimbs[i] = s | ss;
        }

        myLimbs[offset] = myLimbs[0] << shift;

        for ( uz i = 0; i < offset; ++i )
            myLimbs[i] = 0;

        return *this;
    }

    BigInt& operator>>=(uz rhs)
    {
        auto constexpr limbBits = bit_count_v<Limb>;

        uz const offset = rhs / limbBits;
        uz const shift = rhs % limbBits;
        uz const invShift = limbBits - shift;

        auto const size = myLimbs.card();
        auto const msl = size - offset - 1;
        for ( uz i = 0; i < msl; ++i ) {
            auto const s = myLimbs[i + offset] >> shift;
            auto const ss = myLimbs[i + offset + 1] << invShift;
            myLimbs[i] = s | ss;
        }

        myLimbs[msl] = myLimbs[msl + offset] >> shift;
        myLimbs.trunc(msl + 1);

        normalize();

        return *this;
    }

    void negate()
    {
        if ( mySign == Positive )
            mySign = Negative;
        else
            mySign = Positive;
    }

private:
    void magAdd(BigInt const& rhs)
    {
        auto constexpr limbBits = bit_count_v<Limb>;
        DoubleLimb constexpr lsl = ~Limb(0);

        auto const lhsSize = myLimbs.card();
        auto const rhsSize = rhs.myLimbs.card();
        auto const minSize = std::min(lhsSize, rhsSize);

        DoubleLimb carry = 0;
        uz i = 0;
        for ( ; i < minSize; ++i ) {
            carry += myLimbs[i];
            carry += rhs.myLimbs[i];
            myLimbs[i] = static_cast<Limb>(carry & lsl);
            carry >>= limbBits;
        }

        for ( ; i < lhsSize; ++i ) {
            carry += myLimbs[i];
            myLimbs[i] = static_cast<Limb>(carry & lsl);
            carry >>= limbBits;
        }

        for ( ; i < rhsSize; ++i ) {
            carry += rhs.myLimbs[i];
            myLimbs.append(static_cast<Limb>(carry & lsl));
            carry >>= limbBits;
        }

        if ( carry )
            myLimbs.append(static_cast<Limb>(carry & lsl));
    }

    void magSub(BigInt const& rhs)
    {
        auto constexpr limbBits = bit_count_v<Limb>;

        auto const lhsSize = myLimbs.card();
        auto const rhsSize = rhs.myLimbs.card();
        auto const [minSize, maxSize] = std::minmax(lhsSize, rhsSize);

        myLimbs.appendRange(Replicate<u32>(maxSize - myLimbs.card()));

        Limb const* bigger;
        Limb const* smaller;
        if ( auto c = magCmp(rhs); c == LT ) {
            bigger = rhs.myLimbs.data();
            smaller = myLimbs.data();
            negate();
        }
        else if ( c == GT ) {
            bigger = myLimbs.data();
            smaller = rhs.myLimbs.data();
        }
        else {
            myLimbs.trunc(1);
            myLimbs[0] = 0;
            mySign = Positive;
            return;
        }

        DoubleLimb borrow = 0;
        uz i = 0;
        for ( ; i < minSize; ++i ) {
            DoubleLimb const b = bigger[i];
            DoubleLimb const s = smaller[i];

            borrow = b - s - borrow;
            myLimbs[i] = static_cast<Limb>(borrow);
            borrow = (borrow >> limbBits) & 1;
        }

        for ( ; borrow && i < maxSize; ++i ) {
            DoubleLimb const b = bigger[i];
            borrow = b - borrow;
            myLimbs[i] = static_cast<Limb>(borrow);
            borrow = (borrow >> limbBits) & 1;
        }

        for ( ; i < maxSize; ++i )
            myLimbs[i] = bigger[i];

        normalize();
    }

    Ordering magCmp(BigInt const& rhs) const
    {
        auto const lhsSize = myLimbs.card();
        auto const rhsSize = rhs.myLimbs.card();

        if ( lhsSize < rhsSize )
            return LT;
        else if ( rhsSize < lhsSize )
            return GT;

        for ( auto i = lhsSize - 1; ~i; --i ) {
            auto const l = myLimbs[i];
            auto const r = rhs.myLimbs[i];

            if ( l < r )
                return LT;
            else if ( r < l )
                return GT;
        }

        return EQ;
    }

    void normalize()
    {
        while ( myLimbs.card() > 1 && !myLimbs.back() )
            myLimbs.pop();
    }

private:
    ab<Limb> myLimbs;
    Sign mySign = Positive;
};

inline BigInt operator+(BigInt lhs, BigInt const& rhs)
{
    return lhs += rhs;
}

inline BigInt operator-(BigInt lhs, BigInt const& rhs)
{
    return lhs -= rhs;
}

inline BigInt operator<<(BigInt lhs, uz rhs)
{
    return lhs <<= rhs;
}

inline BigInt operator>>(BigInt lhs, uz rhs)
{
    return lhs >>= rhs;
}

inline BigInt operator-(BigInt rhs)
{
    rhs.negate();
    return rhs;
}

} // namespace kyfoo
