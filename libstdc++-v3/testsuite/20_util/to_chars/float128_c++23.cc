// Copyright (C) 2022-2025 Free Software Foundation, Inc.
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

// { dg-do run { target c++23 } }
// { dg-require-effective-target ieee_floats }
// { dg-require-effective-target size32plus }
// { dg-add-options ieee }
// { dg-xfail-run-if "from_chars limited to double-precision" { aarch64-*-vxworks* aarch64-*-rtems* } }

#include <charconv>
#include <stdfloat>
#include <limits>
#include <numbers>
#include <testsuite_hooks.h>

#if defined(__STDCPP_FLOAT128_T__) \
    && (defined(_GLIBCXX_LDOUBLE_IS_IEEE_BINARY128) \
	|| defined(_GLIBCXX_HAVE_FLOAT128_MATH))
void
test(std::chars_format fmt = std::chars_format{})
{
  std::float128_t tests[] = {
    std::numeric_limits<std::float128_t>::denorm_min(),
    std::numeric_limits<std::float128_t>::min(),
    0.0f128,
    -42.0f128,
    1234.5678912345f128,
    std::numbers::e_v<std::float128_t>,
    std::numbers::log2e_v<std::float128_t>,
    std::numbers::log10e_v<std::float128_t>,
    std::numbers::pi_v<std::float128_t>,
    std::numbers::inv_pi_v<std::float128_t>,
    std::numbers::inv_sqrtpi_v<std::float128_t>,
    std::numbers::ln2_v<std::float128_t>,
    std::numbers::ln10_v<std::float128_t>,
    std::numbers::sqrt2_v<std::float128_t>,
    std::numbers::sqrt3_v<std::float128_t>,
    std::numbers::inv_sqrt3_v<std::float128_t>,
    std::numbers::egamma_v<std::float128_t>,
    std::numbers::phi_v<std::float128_t>,
// Solaris has non-conforming printf, see PR98384 and PR107815.
#if !(defined(__sun__) && defined(__svr4__))
    std::numeric_limits<std::float128_t>::max()
#endif
  };
  char str1[10000], str2[10000];
  for (auto u : tests)
    {
      auto [ptr1, ec1] = (fmt == std::chars_format{}
			  ? std::to_chars(str1, str1 + sizeof(str1), u)
			  : std::to_chars(str1, str1 + sizeof(str1), u, fmt));
      VERIFY( ec1 == std::errc() );
//    std::cout << u << ' ' << std::string_view (str1, ptr1) << '\n';
      if (fmt == std::chars_format::fixed)
	{
	  auto [ptr2, ec2] = std::to_chars(str2, str2 + (ptr1 - str1), u, fmt);
	  VERIFY( ec2 == std::errc() && ptr2 - str2 == ptr1 - str1 );
	  auto [ptr3, ec3] = std::to_chars(str2, str2 + (ptr1 - str1 - 1), u, fmt);
	  VERIFY( ec3 != std::errc() );
	}
      std::float128_t v;
      auto [ptr4, ec4] = std::from_chars(str1, ptr1, v,
					 fmt == std::chars_format{}
					 ? std::chars_format::general : fmt);
      VERIFY( ec4 == std::errc() && ptr4 == ptr1 );
      VERIFY( u == v );

      if (fmt == std::chars_format{})
	continue;

      auto [ptr5, ec5] = std::to_chars(str1, str1 + sizeof(str1), u, fmt, 90);
      VERIFY( ec5 == std::errc() );
//    std::cout << u << ' ' << std::string_view (str1, ptr5) << '\n';
      v = 4.0f128;
      auto [ptr6, ec6] = std::from_chars(str1, ptr5, v, fmt);
      VERIFY( ec6 == std::errc() && ptr6 == ptr5 );
      if (fmt == std::chars_format::fixed && u > 0.0f128 && u < 0.000001f128)
	VERIFY( v == 0.0 );
      else
	VERIFY( u == v );
    }
}
#endif

int
main()
{
#if defined(__STDCPP_FLOAT128_T__) \
    && (defined(_GLIBCXX_LDOUBLE_IS_IEEE_BINARY128) \
	|| defined(_GLIBCXX_HAVE_FLOAT128_MATH))
  test();
  test(std::chars_format::fixed);
  test(std::chars_format::scientific);
  test(std::chars_format::general);
  test(std::chars_format::hex);
#endif
}
