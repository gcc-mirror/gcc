// Copyright (C) 2020 Free Software Foundation, Inc.
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

// <charconv> is supported in C++14 as a GNU extension
// { dg-do run { target c++14 } }

#include <charconv>
#include <string>
#include <limits>
#include <cmath>
#include <cstdlib>
#include <testsuite_hooks.h>

// Test std::from_chars floating-point conversions.

void
test01()
{
  std::string s;
  double d;
  std::from_chars_result res;

  for (auto fmt : { std::chars_format::fixed, std::chars_format::scientific,
		    std::chars_format::general, std::chars_format::hex })
  {
    s = "Info";
    res = std::from_chars(s.data(), s.data() + s.length(), d, fmt);
    VERIFY( std::isinf(d) );
    VERIFY( res.ptr == s.data() + 3 );
    VERIFY( res.ec == std::errc{} );

    s = "-INFIN";
    res = std::from_chars(s.data(), s.data() + s.length(), d, fmt);
    VERIFY( std::isinf(d) );
    VERIFY( d < 0 );
    VERIFY( res.ptr == s.data() + 4 );
    VERIFY( res.ec == std::errc{} );

    s = "InFiNiTy aNd BeYoNd";
    res = std::from_chars(s.data(), s.data() + s.length(), d, fmt);
    VERIFY( std::isinf(d) );
    VERIFY( res.ptr == s.data() + 8 );
    VERIFY( res.ec == std::errc{} );

    s = "nAn";
    res = std::from_chars(s.data(), s.data() + s.length(), d, fmt);
    VERIFY( std::isnan(d) );
    VERIFY( res.ptr == s.data() + 3 );
    VERIFY( res.ec == std::errc{} );

    s = "-NAN()";
    res = std::from_chars(s.data(), s.data() + s.length(), d, fmt);
    VERIFY( std::isnan(d) );
    VERIFY( res.ptr == s.data() + s.length() );
    VERIFY( res.ec == std::errc{} );
  }
}

void
test02()
{
  std::string s;
  double d = 1.0;
  std::from_chars_result res;

  s = "0x123";
  res = std::from_chars(s.data(), s.data() + s.length(), d);
  VERIFY( d == 0.0 );
  VERIFY( res.ptr == s.data() + 1 );
  VERIFY( res.ec == std::errc{} );

  d = 1.0;
  res = std::from_chars(s.data(), s.data() + s.length(), d,
			std::chars_format::fixed);
  VERIFY( d == 0.0 );
  VERIFY( res.ptr == s.data() + 1 );
  VERIFY( res.ec == std::errc{} );

  d = 1.0;
  res = std::from_chars(s.data(), s.data() + s.length(), d,
			std::chars_format::scientific);
  VERIFY( d == 1.0 );
  VERIFY( res.ptr == s.data() );
  VERIFY( res.ec == std::errc::invalid_argument );

  d = 1.0;
  res = std::from_chars(s.data(), s.data() + s.length(), d,
			std::chars_format::general);
  VERIFY( d == 0.0 );
  VERIFY( res.ptr == s.data() + 1 );
  VERIFY( res.ec == std::errc{} );

  d = 1.0;
  res = std::from_chars(s.data(), s.data() + s.length(), d,
			std::chars_format::hex);
  VERIFY( d == 0.0 );
  VERIFY( res.ptr == s.data() + 1 );
  VERIFY( res.ec == std::errc{} );
}

void
test03()
{
  std::string s;
  double d = 1.0;
  std::from_chars_result res;

  s = "0.5e+2azzz";
  res = std::from_chars(s.data(), s.data() + s.length(), d);
  VERIFY( d == 0.5e+2 );
  VERIFY( res.ptr == s.data() + s.length() - 1 - 3 );
  VERIFY( res.ec == std::errc{} );

  res = std::from_chars(s.data(), s.data() + s.length(), d,
			std::chars_format::fixed);
  VERIFY( d == 0.5 );
  VERIFY( res.ptr == s.data() + 3 );
  VERIFY( res.ec == std::errc{} );

  d = 1.0;
  res = std::from_chars(s.data(), s.data() + s.length(), d,
			std::chars_format::scientific);
  VERIFY( d == 0.5e+2 );
  VERIFY( res.ptr == s.data() + s.length() - 1 - 3 );
  VERIFY( res.ec == std::errc{} );

  d = 1.0;
  res = std::from_chars(s.data(), s.data() + s.length(), d,
			std::chars_format::general);
  VERIFY( d == 0.5e+2 );
  VERIFY( res.ptr == s.data() + s.length() - 1 - 3 );
  VERIFY( res.ec == std::errc{} );

  d = 1.0;
  res = std::from_chars(s.data(), s.data() + s.length(), d,
			std::chars_format::hex);
  VERIFY( d == 0x0.5Ep0 );
  VERIFY( res.ptr == s.data() + 4 );
  VERIFY( res.ec == std::errc{} );

  s = "1.Ap-2zzz";
  res = std::from_chars(s.data(), s.data() + s.length(), d,
			std::chars_format::hex);
  VERIFY( d == 0.40625 );
  VERIFY( res.ptr == s.data() + s.length() - 3 );
  VERIFY( res.ec == std::errc{} );
}

void
test04()
{
  // Huge input strings
  std::string s(1000, '0');
  double d = 1.0;
  std::from_chars_result res;
  res = std::from_chars(s.data(), s.data() + s.length(), d);
  VERIFY( res.ptr == s.data() + s.length() );
  VERIFY( res.ec == std::errc{} );
  VERIFY( d == 0.0 );

  s += ".5";
  res = std::from_chars(s.data(), s.data() + s.length(), d);
  VERIFY( res.ptr == s.data() + s.length() );
  VERIFY( res.ec == std::errc{} );
  VERIFY( d == 0.5 );

  s += "e2";
  auto len = s.length();
  s += std::string(1000, 'a');
  res = std::from_chars(s.data(), s.data() + s.length(), d);
  VERIFY( res.ptr == s.data() + len );
  VERIFY( res.ec == std::errc{} );
  VERIFY( d == 50 );
}

using std::to_string;

#ifdef __GLIBCXX_TYPE_INT_N_0
std::string
to_string(unsigned __GLIBCXX_TYPE_INT_N_0 val)
{
  using Limits = std::numeric_limits<unsigned __GLIBCXX_TYPE_INT_N_0>;
  std::string s(Limits::digits10+2, '0');
  for (auto iter = s.end(); val != 0; val /= 10)
    *--iter = '0' + (val % 10);
  return s;
}
#endif

void
test05()
{
  std::from_chars_result res;
  float flt;
  double dbl;
  long double ldbl;

  // Small integer values that are exactly representable

  for (int i = 0; i < 100; ++i)
  {
    std::string s = to_string(i);
    int len = s.length();
    s += "123";
    const char* s1 = s.c_str();
    const char* s1_end = s1 + len;

    for (auto fmt : { std::chars_format::fixed,
		      std::chars_format::general,
		      std::chars_format::hex })
    {
      if (fmt == std::chars_format::hex && i > 9)
	continue;

      res = std::from_chars(s1, s1_end, flt, fmt);
      VERIFY( res.ec == std::errc{} );
      VERIFY( res.ptr == s1_end );
      VERIFY( flt == i );

      res = std::from_chars(s1, s1_end, dbl, fmt);
      VERIFY( res.ec == std::errc{} );
      VERIFY( res.ptr == s1_end );
      VERIFY( dbl == i );

      res = std::from_chars(s1, s1_end, ldbl, fmt);
      VERIFY( res.ec == std::errc{} );
      VERIFY( res.ptr == s1_end );
      VERIFY( ldbl == i );
    }

    if (i > 9)
      continue;

    // Test single-digit integers with small exponents.

    const char s2[] = { '.', *s1, 'e', '0', '0', '0', '1' };
    const char* s2_end = s2 + sizeof(s2);

    const char s3[] = { *s1, '0', 'e', '-', '0', '0', '1' };
    const char* s3_end = s3 + sizeof(s3);

    for (auto fmt : { std::chars_format::scientific,
		      std::chars_format::general })
    {
      res = std::from_chars(s2, s2_end, flt, fmt);
      VERIFY( res.ec == std::errc{} );
      VERIFY( res.ptr == s2_end );
      VERIFY( flt == i );

      res = std::from_chars(s3, s3_end, flt, fmt);
      VERIFY( res.ec == std::errc{} );
      VERIFY( res.ptr == s3_end );
      VERIFY( flt == i );

      res = std::from_chars(s2, s2_end, dbl, fmt);
      VERIFY( res.ec == std::errc{} );
      VERIFY( res.ptr == s2_end );
      VERIFY( dbl == i );

      res = std::from_chars(s3, s3_end, dbl, fmt);
      VERIFY( res.ec == std::errc{} );
      VERIFY( res.ptr == s3_end );
      VERIFY( dbl == i );

      res = std::from_chars(s2, s2_end, ldbl, fmt);
      VERIFY( res.ec == std::errc{} );
      VERIFY( res.ptr == s2_end );
      VERIFY( ldbl == i );

      res = std::from_chars(s3, s3_end, ldbl, fmt);
      VERIFY( res.ec == std::errc{} );
      VERIFY( res.ptr == s3_end );
      VERIFY( ldbl == i );
    }
  }
}

template<typename FloatT, typename UIntT>
void
test_max_mantissa()
{
  using Float_limits = std::numeric_limits<FloatT>;
  using UInt_limits = std::numeric_limits<UIntT>;

  if constexpr (Float_limits::is_iec559
		&& Float_limits::digits < UInt_limits::digits)
  {
    std::printf("Testing %d-bit float, using %zu-bit integer\n",
	Float_limits::digits + (int)std::log2(Float_limits::max_exponent) + 1,
	sizeof(UIntT) * __CHAR_BIT__);

    std::from_chars_result res;
    FloatT flt;

    for (int i = 0; i < 10; ++i)
    {
      // (1 << digits) - 1 is the maximum value of the mantissa
      const auto val = ((UIntT)1 << Float_limits::digits) - 1 - i;
      std::string s = to_string(val);
      auto len = s.length();
      s += "000"; // these should be ignored
      for (auto fmt : { std::chars_format::fixed,
			std::chars_format::general })
      {
	res = std::from_chars(s.data(), s.data() + len, flt, fmt);
	VERIFY( res.ec == std::errc{} );
	VERIFY( res.ptr == s.data() + len );
	VERIFY( flt == val );
      }
      s.resize(len);
      const auto orig_len = len;
      s += "e+000";
      len = s.length();
      s += "111";
      for (auto fmt : { std::chars_format::scientific,
			std::chars_format::general })
      {
	res = std::from_chars(s.data(), s.data() + len, flt, fmt);
	VERIFY( res.ec == std::errc{} );
	VERIFY( res.ptr == s.data() + len );
	VERIFY( flt == val );

	std::string s2 = s.substr(0, len - 5);
	s2.insert(s2.cbegin() + orig_len - 1, '.');
	s2 += "e000000000001";
	res = std::from_chars(s.data(), s.data() + len, flt, fmt);
	VERIFY( res.ec == std::errc{} );
	VERIFY( res.ptr == s.data() + len );
	VERIFY( flt == val );
      }
    }
  }
}

void
test06()
{
  test_max_mantissa<float, unsigned long>();
  test_max_mantissa<double, unsigned long long>();
#ifdef __GLIBCXX_TYPE_INT_N_0
  test_max_mantissa<long double, unsigned __GLIBCXX_TYPE_INT_N_0>();
#endif
}

int
main()
{
  test01();
  test02();
  test03();
  test04();
  test05();
  test06();
}
