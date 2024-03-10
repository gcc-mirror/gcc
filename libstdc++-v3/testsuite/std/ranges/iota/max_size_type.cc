// Copyright (C) 2020-2023 Free Software Foundation, Inc.
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

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// { dg-options "-std=gnu++2a" }
// { dg-do run { target c++2a } }
// { dg-timeout-factor 4 }

#include <limits>
#include <ranges>
#include <testsuite_hooks.h>

using max_size_t = std::ranges::__detail::__max_size_type;
using max_diff_t = std::ranges::__detail::__max_diff_type;
using rep_t = max_size_t::__rep;
#if __SIZEOF_INT128__
using signed_rep_t = __int128;
#else
using signed_rep_t = long long;
#endif

static_assert(sizeof(max_size_t) == sizeof(max_diff_t));
static_assert(sizeof(rep_t) == sizeof(signed_rep_t));

static_assert(std::regular<max_size_t>);
static_assert(std::totally_ordered<max_size_t>);

static_assert(std::regular<max_diff_t>);
static_assert(std::totally_ordered<max_diff_t>);

// We can't use numeric_limits<rep_t>::max() here because __int128 is an
// integral type only in GNU mode.
constexpr max_size_t mu = max_size_t(~rep_t(0));
constexpr max_size_t ou = 1;
constexpr max_diff_t ns = -1;

void
test01()
{
  static_assert(max_size_t(7) % 3 == 1);
  static_assert(max_size_t(7) % 4 == 3);

  static_assert(-max_diff_t(1) == max_diff_t(-1));
  static_assert(max_diff_t(3) % 2 == 1);
  static_assert(max_diff_t(-3) / 2 == -1);
  static_assert(max_diff_t(-3) % 2 == -1);
  static_assert(max_diff_t(3) % -2 == 1);
  static_assert(max_diff_t(-3) << 1 == -6);
  static_assert(max_diff_t(-3) >> 1 == -2);
  static_assert(max_diff_t(-3) >> 2 == -1);
  static_assert(max_diff_t(-3) >> 10 == -1);
  static_assert(max_diff_t(3) >> 1 == 1);
  static_assert(max_diff_t(3) >> 2 == 0);

  static_assert(max_diff_t(-5) / 3 == -1);
  static_assert(max_diff_t(5) / -3 == -1);
  static_assert(max_diff_t(-5) / -3 == 1);
  static_assert(max_diff_t(5) / 3 == 1);

  static_assert(max_diff_t(-6) / 3 == -2);
  static_assert(max_diff_t(6) / -3 == -2);
  static_assert(max_diff_t(-6) / -3 == 2);
  static_assert(max_diff_t(6) / 3 == 2);

  static_assert(~max_size_t(-3) == 2);
  static_assert(~max_diff_t(-3) == 2);

  static_assert(max_diff_t(1) < max_diff_t(3));
  static_assert(max_diff_t(-1) < max_diff_t(3));
  static_assert(max_diff_t(1) > max_diff_t(-3));
  static_assert(max_diff_t(-1) > max_diff_t(-3));

  static_assert(max_diff_t(mu)/-1 == -max_diff_t(mu));
  static_assert(-max_diff_t(mu)/1 == -max_diff_t(mu));
  static_assert(max_diff_t(mu)>>1 == max_diff_t(mu)/2);
  static_assert(-max_diff_t(mu+1) == max_diff_t(mu+1));
  static_assert(-(mu+1) == mu+1);
  static_assert((mu+1)<<1 == 0);
  static_assert(max_diff_t(mu+1)<<1 == 0);
  static_assert(max_diff_t(mu+1)>>1 < 0);

  static_assert(int(max_diff_t(mu+1)) == 0);
  static_assert(rep_t(max_diff_t(mu+1)) == 0);
  static_assert(int(max_diff_t(mu)) == -1);
  static_assert(rep_t(max_diff_t(mu)) == rep_t(-1));

  static_assert(2*mu+1 > 2*mu);
  static_assert(~(2*mu+1) == 0);
  static_assert(mu/mu == 1);
  static_assert(2*mu > mu);
  static_assert(2*mu-mu == mu);
  static_assert((2*mu)/mu == 2);
  static_assert((2*mu+1)/mu == 2);
  static_assert((2*mu-1)/(mu-1) == 2);
  static_assert((2*mu-1)/mu == 1);
  static_assert((2*mu+-1)/mu == 1);
  static_assert(2*mu-1 < 2*mu);
  static_assert(2*mu-1 <= 2*mu);
  static_assert(2*mu+1 > 2*mu);
  static_assert(2*mu+1 >= 2*mu);
  static_assert((2*mu)/1 == 2*mu);
  static_assert(mu/mu-1 == 0);
  static_assert(mu*0 == 0);
  static_assert((2*mu-1)*0 == 0);
  static_assert((2*mu-1)>>1 == mu-1);
  static_assert(mu+-1+1 == mu);
  static_assert(mu+1+-1 == mu);
  static_assert(mu+1);
  static_assert((2*mu)/2 == mu);
  static_assert((2*mu)>>1 == mu);
  static_assert((mu<<1)>>1 == mu);
  static_assert(1/mu == 0);
  static_assert(mu/1 == mu);
  static_assert(((mu+1)|mu) == -1);
  static_assert((mu+1)+(mu+1) < mu+1);

  static_assert(max_size_t(ns) == -1);
  static_assert(-max_diff_t(ou) == -1);
  static_assert(-max_diff_t(-ou) == 1);
  static_assert(max_size_t(-max_diff_t(-ou)) == 1);
  static_assert(ns*ns == max_diff_t(ou));
  static_assert(max_size_t(ns)*max_size_t(ns) == ou);
  static_assert(-max_diff_t(0) == max_diff_t(0));
  static_assert(-ou-ou == -2*ou);

  static_assert(int(ns) == -1);
  static_assert(rep_t(ns) == rep_t(-1));

  static_assert(max_size_t() == 0);
  static_assert(max_diff_t() == 0);

  auto f = [] (auto a) { a /= a; return a; };
  static_assert(f(max_size_t(5)) == 1);
  static_assert(f(max_size_t(-5)) == 1);
  static_assert(f(max_diff_t(5)) == 1);

  auto g = [] (auto a) { a >>= a; return a; };
  static_assert(g(max_size_t(5)) == 0);
  static_assert(g(max_diff_t(5)) == 0);

  auto h = [] (auto a) { a <<= a; return a; };
  static_assert(h(max_size_t(3)) == 24);
  static_assert(h(max_diff_t(3)) == 24);

  auto w = [] (auto a) {
    const auto b = a;
    VERIFY( a++ == b   && a == b+1 );
    VERIFY( a-- == b+1 && a == b );

    VERIFY( ++(++a) == b+2 );
    VERIFY( --(--a) == b );
    return true;
  };
  static_assert(w(max_size_t(10)));
  static_assert(w(-max_diff_t(10)));

#if __cpp_lib_three_way_comparison
  static_assert(max_size_t{1} <=> max_size_t{9} == std::strong_ordering::less);
  static_assert(max_size_t{3} <=> max_size_t{2} == std::strong_ordering::greater);
  static_assert(max_size_t{5} <=> max_size_t{5} == std::strong_ordering::equal);
  static_assert(~max_size_t{1} <=> ~max_size_t{9} == std::strong_ordering::greater);
  static_assert(~max_size_t{3} <=> ~max_size_t{2} == std::strong_ordering::less);
  static_assert(~max_size_t{5} <=> ~max_size_t{5} == std::strong_ordering::equal);
  static_assert(~max_size_t{5} <=> max_size_t{9} == std::strong_ordering::greater);
  static_assert(~max_size_t{9} <=> max_size_t{5} == std::strong_ordering::greater);
  static_assert(max_size_t{5} <=> ~max_size_t{9} == std::strong_ordering::less);
  static_assert(max_size_t{9} <=> ~max_size_t{5} == std::strong_ordering::less);

  static_assert(max_diff_t{1} <=> max_diff_t{9} == std::strong_ordering::less);
  static_assert(max_diff_t{3} <=> max_diff_t{2} == std::strong_ordering::greater);
  static_assert(max_diff_t{5} <=> max_diff_t{5} == std::strong_ordering::equal);
  static_assert(max_diff_t{-1} <=> max_diff_t{-9} == std::strong_ordering::greater);
  static_assert(max_diff_t{-3} <=> max_diff_t{-2} == std::strong_ordering::less);
  static_assert(max_diff_t{-5} <=> max_diff_t{-5} == std::strong_ordering::equal);
  static_assert(max_diff_t{-5} <=> max_diff_t{9} == std::strong_ordering::less);
  static_assert(max_diff_t{-9} <=> max_diff_t{5} == std::strong_ordering::less);
  static_assert(max_diff_t{5} <=> max_diff_t{-9} == std::strong_ordering::greater);
  static_assert(max_diff_t{9} <=> max_diff_t{-5} == std::strong_ordering::greater);
#endif
}

template<bool signed_p, bool shorten_p>
void
test02()
{
  using hw_type = std::conditional_t<signed_p, signed_rep_t, rep_t>;
  using max_type = std::conditional_t<signed_p, max_diff_t, max_size_t>;
  using shorten_type = std::conditional_t<shorten_p, hw_type, max_type>;
  const int hw_type_bit_size = sizeof(hw_type) * __CHAR_BIT__;
  const int limit = 1000;
  const int log2_limit = 10;
  static_assert((1 << log2_limit) >= limit);
  const int min = (signed_p ? -limit : 0);
  const int max = limit;
  for (hw_type i = min; i <= max; i++)
    {
      bool ok = true;
      if (signed_p || shorten_p)
	{
	  ok &= (~i == shorten_type(~max_type(i)));
	  ok &= (-i == shorten_type(-max_type(i)));
	}
      for (hw_type j = min; j <= max; j++)
	{
	  ok &= (i*j == shorten_type(max_type(i)*j));
	  ok &= (i+j == shorten_type(max_type(i)+j));
	  if (j != 0)
	    {
	      ok &= (i/j == shorten_type(max_type(i)/j));
	      ok &= (i%j == shorten_type(max_type(i)%j));
	    }
	  if (signed_p || shorten_p)
	    ok &= (i-j == shorten_type(max_type(i)-j));
	  ok &= ((i&j) == shorten_type(max_type(i)&j));
	  ok &= ((i|j) == shorten_type(max_type(i)|j));
	  ok &= ((i^j) == shorten_type(max_type(i)^j));
	  if (j >= 0 && j < hw_type(hw_type_bit_size)
	      && (shorten_p || j < hw_type(hw_type_bit_size) - log2_limit))
	    {
	      ok &= ((i>>j) == shorten_type(max_type(i)>>j));
	      ok &= ((i<<j) == shorten_type(max_type(i)<<j));
	    }
	  ok &= (i>j) == (max_type(i) > j);
	  ok &= (i<j) == (max_type(i) < j);
	  ok &= (i>=j) == (max_type(i) >= j);
	  ok &= (i<=j) == (max_type(i) <= j);
	  ok &= (i==j) == (max_type(i) == j);
	  ok &= (i!=j) == (max_type(i) != j);
	  if (!ok)
	    {
	      fprintf(stderr,
		      "Inconsistency found: %d %d %lld %lld\n",
		      signed_p, shorten_p, (long long)i, (long long)j) ;
	       VERIFY(0);
	    }
	}
    }
}

template<bool signed_p, bool toggle_base_p>
void
test03()
{
  using hw_type = std::conditional_t<signed_p, signed_rep_t, rep_t>;
  using max_type = std::conditional_t<signed_p, max_diff_t, max_size_t>;
  using base_type = std::conditional_t<toggle_base_p, hw_type, max_type>;
  constexpr int hw_type_bit_size = sizeof(hw_type) * __CHAR_BIT__;
  constexpr int limit = 1000;
  constexpr int log2_limit = 10;
  static_assert((1 << log2_limit) >= limit);
  const int min = (signed_p ? -limit : 0);
  const int max = limit;
  for (hw_type i = min; i <= max; i++)
    {
      bool ok = true;
      base_type k;
      for (hw_type j = min; j <= max; j++)
	{
	  k = i; k *= j;
	  ok &= (k == (max_type(i)*j));
	  k = i; k += j;
	  ok &= (k == (max_type(i)+j));
	  if (j != 0)
	    {
	      k = i; k /= j;
	      ok &= (k == (max_type(i)/j));
	      k = i; k %= j;
	      ok &= (k == (max_type(i)%j));
	    }
	  if (signed_p)
	    {
	      k = i; k -= j;
	      ok &= (k == (max_type(i)-j));
	    }
	  k = i; k &= j;
	  ok &= (k == (max_type(i)&j));
	  k = i; k |= j;
	  ok &= (k == (max_type(i)|j));
	  k = i; k ^= j;
	  ok &= (k == (max_type(i)^j));
	  if (j >= 0 && j < hw_type(hw_type_bit_size)
	      && (!toggle_base_p || j < hw_type(hw_type_bit_size) - log2_limit))
	    {
	      k = i; k >>= j;
	      ok &= (k == (max_type(i)>>j));
	      k = i; k <<= j;
	      ok &= (k == (max_type(i)<<j));
	    }
	  if (!ok)
	    {
	      fprintf(stderr,
		      "Inconsistency found: %d %d %lld %lld\n",
		      signed_p, toggle_base_p, (long long)i, (long long)j) ;
	       VERIFY(0);
	    }
	}
    }
}

void
test04()
{
  constexpr int limit = 1000;
  for (int i = -limit; i <= limit; i++)
    {
      VERIFY( -max_size_t(-i) == i );
      for (int j = i; j <= limit; j++)
	{
	  VERIFY( max_size_t(-i) * max_size_t(-j) == i*j );
	  VERIFY( max_size_t(-j) * max_size_t(-i) == j*i );
	  VERIFY( rep_t(((mu+1)+i)*((mu+1)+j)) == rep_t(i*j) );
	  VERIFY( rep_t(((mu+1)+j)*((mu+1)+i)) == rep_t(j*i) );
	  if (i >= 0 && j > 0)
	    {
	      auto r = (mu+i)-((mu+i)/j)*j;
	      VERIFY( r >= 0 && r < j );
	      VERIFY( r == (mu+i)%j );
	    }
	}
    }
}

void
test05()
{
#if __SIZEOF_INT128__
  max_size_t x = 0;
  x = static_cast<__int128>(0);
  x = static_cast<unsigned __int128>(0);

  max_diff_t y = 0;
  y = static_cast<__int128>(0);;
  y = static_cast<unsigned __int128>(0);
#endif
}

using std::numeric_limits;

static_assert(numeric_limits<max_size_t>::is_specialized);
static_assert(!numeric_limits<max_size_t>::is_signed);
static_assert(numeric_limits<max_size_t>::is_integer);
static_assert(numeric_limits<max_size_t>::is_exact);
// We can't unconditionally use numeric_limits here because __int128 is an
// integral type only in GNU mode.
#if __SIZEOF_INT128__
static_assert(numeric_limits<max_size_t>::digits == 129);
static_assert(numeric_limits<max_size_t>::digits10 == 38);
static_assert(numeric_limits<max_size_t>::max()
	      == 2*max_size_t(~rep_t(0)) + 1);
#else
static_assert(numeric_limits<max_size_t>::digits
	      == numeric_limits<rep_t>::digits + 1);
static_assert(numeric_limits<max_size_t>::digits10
	      == numeric_limits<rep_t>::digits10);
static_assert(numeric_limits<max_size_t>::max()
	      == 2*max_size_t(numeric_limits<rep_t>::max())+1);
#endif
static_assert(numeric_limits<max_size_t>::min() == 0);
static_assert(numeric_limits<max_size_t>::max()
	      == max_size_t(-1));
static_assert((numeric_limits<max_size_t>::max()
	       >> (numeric_limits<max_size_t>::digits-1)) == 1);
static_assert(numeric_limits<max_size_t>::lowest()
	      == numeric_limits<max_size_t>::min());

static_assert(numeric_limits<max_diff_t>::is_specialized);
static_assert(numeric_limits<max_diff_t>::is_signed);
static_assert(numeric_limits<max_diff_t>::is_integer);
static_assert(numeric_limits<max_diff_t>::is_exact);
static_assert(numeric_limits<max_diff_t>::digits
	      == numeric_limits<max_size_t>::digits - 1);
static_assert(numeric_limits<max_diff_t>::digits10
	      == numeric_limits<max_size_t>::digits10);
// We can't unconditionally use numeric_limits here because __int128 is an
// integral type only in GNU mode.
#if __SIZEOF_INT128__
static_assert(numeric_limits<max_diff_t>::min() == -max_diff_t(~rep_t(0))-1);
static_assert(numeric_limits<max_diff_t>::max() == ~rep_t(0));
#else
static_assert(numeric_limits<max_diff_t>::min()
	      == -max_diff_t(numeric_limits<rep_t>::max())-1);
static_assert(numeric_limits<max_diff_t>::max()
	      == numeric_limits<rep_t>::max());
#endif
static_assert(numeric_limits<max_diff_t>::lowest()
	      == numeric_limits<max_diff_t>::min());
static_assert(max_diff_t(max_size_t(1)
			 << (numeric_limits<max_size_t>::digits-1))
	      == numeric_limits<max_diff_t>::min());

int
main()
{
  test01();

  test02<false,false>();
  test02<false,true>();
  test02<true,false>();
  test02<true,true>();

  test03<false,false>();
  test03<false,true>();
  test03<true,false>();
  test03<true,true>();

  test04();
  test05();
}
