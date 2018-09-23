#pragma once

#include <cassert>

#include <algorithm>
#include <vector>

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
    using limb_t = u32;
    using double_limb_t = u64;

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
        static_assert(sizeof(rhs) >= sizeof(limb_t));

        auto const size = sizeof(rhs) / sizeof(limb_t);
        limb_t* first = reinterpret_cast<limb_t*>(&rhs);
        auto last = first + size;

        myLimbs.assign(first, last);
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
        auto constexpr limbBits = bit_count_v<limb_t>;

        uz const offset = rhs / limbBits;
        uz const shift = rhs % limbBits;
        uz const invShift = limbBits - shift;

        myLimbs.resize(myLimbs.size() + offset);
        for ( uz i = myLimbs.size() - 1; i > offset; --i ) {
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
        auto constexpr limbBits = bit_count_v<limb_t>;

        uz const offset = rhs / limbBits;
        uz const shift = rhs % limbBits;
        uz const invShift = limbBits - shift;

        auto const size = myLimbs.size();
        for ( uz i = 0; i < size - offset - 1; ++i ) {
            auto const s = myLimbs[i + offset] >> shift;
            auto const ss = myLimbs[i + offset + 1] << invShift;
            myLimbs[i] = s | ss;
        }
        myLimbs[size - offset] = myLimbs[size] >> shift;
        myLimbs.resize(size - offset + 1);

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
        auto constexpr limbBits = bit_count_v<limb_t>;
        double_limb_t constexpr lsl = ~limb_t(0);

        auto const lhsSize = myLimbs.size();
        auto const rhsSize = rhs.myLimbs.size();
        auto const minSize = std::min(lhsSize, rhsSize);

        double_limb_t carry = 0;
        uz i = 0;
        for ( ; i < minSize; ++i ) {
            carry += myLimbs[i];
            carry += rhs.myLimbs[i];
            myLimbs[i] = static_cast<limb_t>(carry & lsl);
            carry >>= limbBits;
        }

        for ( ; i < lhsSize; ++i ) {
            carry += myLimbs[i];
            myLimbs[i] = static_cast<limb_t>(carry & lsl);
            carry >>= limbBits;
        }

        for ( ; i < rhsSize; ++i ) {
            carry += rhs.myLimbs[i];
            myLimbs.emplace_back(static_cast<limb_t>(carry & lsl));
            carry >>= limbBits;
        }

        if ( carry )
            myLimbs.emplace_back(static_cast<limb_t>(carry & lsl));
    }

    void magSub(BigInt const& rhs)
    {
        auto constexpr limbBits = bit_count_v<limb_t>;

        auto const lhsSize = myLimbs.size();
        auto const rhsSize = rhs.myLimbs.size();
        auto const [minSize, maxSize] = std::minmax(lhsSize, rhsSize);

        myLimbs.resize(maxSize);

        limb_t const* bigger;
        limb_t const* smaller;
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
            myLimbs.resize(1);
            myLimbs[0] = 0;
            mySign = Positive;
            return;
        }

        double_limb_t borrow = 0;
        uz i = 0;
        for ( ; i < minSize; ++i ) {
            double_limb_t const b = bigger[i];
            double_limb_t const s = smaller[i];

            borrow = b - s - borrow;
            myLimbs[i] = static_cast<limb_t>(borrow);
            borrow = (borrow >> limbBits) & 1;
        }

        for ( ; borrow && i < maxSize; ++i ) {
            double_limb_t const b = bigger[i];
            borrow = b - borrow;
            myLimbs[i] = static_cast<limb_t>(borrow);
            borrow = (borrow >> limbBits) & 1;
        }

        for ( ; i < maxSize; ++i )
            myLimbs[i] = bigger[i];

        normalize();
    }

    Ordering magCmp(BigInt const& rhs) const
    {
        auto const lhsSize = myLimbs.size();
        auto const rhsSize = rhs.myLimbs.size();

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
        while ( myLimbs.size() > 1 && !myLimbs.back() )
            myLimbs.pop_back();
    }

private:
    std::vector<limb_t> myLimbs;
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
