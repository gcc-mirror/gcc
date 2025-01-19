// A relatively minimal unsigned 128-bit integer class type, used by the
// floating-point std::to_chars implementation on targets that lack __int128.

// Copyright (C) 2021-2025 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

struct uint128_t
{
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
  uint64_t lo, hi;
#else
  uint64_t hi, lo;
#endif

  uint128_t() = default;

  constexpr
  uint128_t(uint64_t lo, uint64_t hi = 0)
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
    : lo(lo), hi(hi)
#else
    : hi(hi), lo(lo)
#endif
  { }

  constexpr explicit
  operator bool() const
  { return *this != 0; }

  template<typename T, typename = std::enable_if_t<std::is_integral_v<T>>>
    constexpr explicit
    operator T() const
    {
      static_assert(sizeof(T) <= sizeof(uint64_t));
      return static_cast<T>(lo);
    }

  friend constexpr uint128_t
  operator&(uint128_t x, const uint128_t y)
  {
    x.lo &= y.lo;
    x.hi &= y.hi;
    return x;
  }

  friend constexpr uint128_t
  operator|(uint128_t x, const uint128_t y)
  {
    x.lo |= y.lo;
    x.hi |= y.hi;
    return x;
  }

  friend constexpr uint128_t
  operator<<(uint128_t x, const uint128_t y)
  {
    __glibcxx_assert(y < 128);
    // TODO: Convince GCC to use shldq on x86 here.
    if (y.lo >= 64)
      {
	x.hi = x.lo << (y.lo - 64);
	x.lo = 0;
      }
    else if (y.lo != 0)
      {
	x.hi <<= y.lo;
	x.hi |= x.lo >> (64 - y.lo);
	x.lo <<= y.lo;
      }
    return x;
  }

  friend constexpr uint128_t
  operator>>(uint128_t x, const uint128_t y)
  {
    __glibcxx_assert(y < 128);
    // TODO: Convince GCC to use shrdq on x86 here.
    if (y.lo >= 64)
      {
	x.lo = x.hi >> (y.lo - 64);
	x.hi = 0;
      }
    else if (y.lo != 0)
      {
	x.lo >>= y.lo;
	x.lo |= x.hi << (64 - y.lo);
	x.hi >>= y.lo;
      }
    return x;
  }

  constexpr uint128_t
  operator~() const
  { return {~lo, ~hi}; }

  constexpr uint128_t
  operator-() const
  { return operator~() + 1; }

  friend constexpr uint128_t
  operator+(uint128_t x, const uint128_t y)
  {
    x.hi += __builtin_add_overflow(x.lo, y.lo, &x.lo);
    x.hi += y.hi;
    return x;
  }

  friend constexpr uint128_t
  operator-(uint128_t x, const uint128_t y)
  {
    x.hi -= __builtin_sub_overflow(x.lo, y.lo, &x.lo);
    x.hi -= y.hi;
    return x;
  }

  static constexpr uint128_t
  umul64_64_128(const uint64_t x, const uint64_t y)
  {
    const uint64_t xl = x & 0xffffffff;
    const uint64_t xh = x >> 32;
    const uint64_t yl = y & 0xffffffff;
    const uint64_t yh = y >> 32;
    const uint64_t ll = xl * yl;
    const uint64_t lh = xl * yh;
    const uint64_t hl = xh * yl;
    const uint64_t hh = xh * yh;
    const uint64_t m = (ll >> 32) + lh + (hl & 0xffffffff);
    const uint64_t l = (ll & 0xffffffff ) | (m << 32);
    const uint64_t h = (m >> 32) + (hl >> 32) + hh;
    return {l, h};
  }

  friend constexpr uint128_t
  operator*(const uint128_t x, const uint128_t y)
  {
    uint128_t z = umul64_64_128(x.lo, y.lo);
    z.hi += x.lo * y.hi + x.hi * y.lo;
    return z;
  }

  friend constexpr uint128_t
  operator/(const uint128_t x, const uint128_t y)
  {
    // Ryu performs 128-bit division only by 5 and 10, so that's what we
    // implement.  The strategy here is to relate division of x with that of
    // x.hi and x.lo separately.
    __glibcxx_assert(y == 5 || y == 10);
    // The following implements division by 5 and 10.  In either case, we
    // first compute division by 5:
    //   x/5 = (x.hi*2^64 + x.lo)/5
    //       = (x.hi*(2^64-1) + x.hi + x.lo)/5
    //       = x.hi*((2^64-1)/5) + (x.hi + x.lo)/5 since CST=(2^64-1)/5 is exact
    //       = x.hi*CST + x.hi/5 + x.lo/5 + ((x.lo%5) + (x.hi%5) >= 5)
    // We go a step further and replace the last adjustment term with a
    // lookup table, which we encode as a binary literal.  This seems to
    // yield smaller code on x86 at least.
    constexpr auto cst = ~uint64_t(0) / 5;
    uint128_t q = uint128_t{x.hi}*cst + uint128_t{x.hi/5 + x.lo/5};
    constexpr auto lookup = 0b111100000u;
    q += (lookup >> ((x.hi % 5) + (x.lo % 5))) & 1;
    if (y == 10)
      q >>= 1;
    return q;
  }

  friend constexpr uint128_t
  operator%(const uint128_t x, const uint128_t y)
  {
    // Ryu performs 128-bit modulus only by 2, 5 and 10, so that's what we
    // implement.  The strategy here is to relate modulus of x with that of
    // x.hi and x.lo separately.
    if (y == 2)
      return x & 1;
    __glibcxx_assert(y == 5 || y == 10);
    // The following implements modulus by 5 and 10.  In either case,
    // we first compute modulus by 5:
    //   x (mod 5) = x.hi*2^64 + x.lo (mod 5)
    //             = x.hi + x.lo (mod 5) since 2^64 ≡ 1 (mod 5)
    // So the straightforward implementation would be
    //   ((x.hi % 5) + (x.lo % 5)) % 5
    // But we go a step further and replace the outermost % with a
    // lookup table:
    //             = {0,1,2,3,4,0,1,2,3}[(x.hi % 5) + (x.lo % 5)] (mod 5)
    // which we encode as an octal literal.
    constexpr auto lookup = 0321043210u;
    auto r = (lookup >> 3*((x.hi % 5) + (x.lo % 5))) & 7;
    if (y == 10)
      // x % 10 = (x % 5)      if x / 5 is even
      //          (x % 5) + 5  if x / 5 is odd
      // The compiler should be able to CSE the below computation of x/5 and
      // the above modulus operations with a nearby inlined computation of x/10.
      r += 5 * ((x/5).lo & 1);
    return r;
  }

  friend constexpr bool
  operator==(const uint128_t x, const uint128_t y)
  { return x.hi == y.hi && x.lo == y.lo; }

  friend constexpr bool
  operator<(const uint128_t x, const uint128_t y)
  { return x.hi < y.hi || (x.hi == y.hi && x.lo < y.lo); }

  friend constexpr auto
  __bit_width(const uint128_t x)
  {
    if (auto w = std::__bit_width(x.hi))
      return w + 64;
    else
      return std::__bit_width(x.lo);
  }

  friend constexpr auto
  __countr_zero(const uint128_t x)
  {
    auto c = std::__countr_zero(x.lo);
    if (c == 64)
      return 64 + std::__countr_zero(x.hi);
    else
      return c;
  }

  constexpr uint128_t&
  operator--()
  { return *this -= 1; }

  constexpr uint128_t&
  operator++()
  { return *this += 1; }

  constexpr uint128_t&
  operator+=(const uint128_t y)
  { return *this = *this + y; }

  constexpr uint128_t&
  operator-=(const uint128_t y)
  { return *this = *this - y; }

  constexpr uint128_t&
  operator*=(const uint128_t y)
  { return *this = *this * y; }

  constexpr uint128_t&
  operator<<=(const uint128_t y)
  { return *this = *this << y; }

  constexpr uint128_t&
  operator>>=(const uint128_t y)
  { return *this = *this >> y; }

  constexpr uint128_t&
  operator|=(const uint128_t y)
  { return *this = *this | y; }

  constexpr uint128_t&
  operator&=(const uint128_t y)
  { return *this = *this & y; }

  constexpr uint128_t&
  operator%=(const uint128_t y)
  { return *this = *this % y; }

  constexpr uint128_t&
  operator/=(const uint128_t y)
  { return *this = *this / y; }

  friend constexpr bool
  operator!=(const uint128_t x, const uint128_t y)
  { return !(x == y); }

  friend constexpr bool
  operator>(const uint128_t x, const uint128_t y)
  { return y < x; }

  friend constexpr bool
  operator>=(const uint128_t x, const uint128_t y)
  { return !(x < y); }
};
