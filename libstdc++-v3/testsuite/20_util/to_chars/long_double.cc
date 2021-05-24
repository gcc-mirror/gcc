// Copyright (C) 2020-2021 Free Software Foundation, Inc.
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

// <charconv> is supported in C++14 as a GNU extension, but this test uses C++17
// hexadecimal floating-point literals.

// When long double is larger than double, the long double to_chars overloads
// are partially implemented in terms of printf, so this test in turn uses
// printf to verify correctness these overloads.
// When long double == double, the long double to_chars overloads are simple
// wrappers around the corresponding double overloads.  Since they don't go
// through printf, we can't portably verify their output by comparing it with
// that of printf, so it's simplest to just not run this test on such targets;
// correctness of these overloads is already implied by that of the double
// overloads.
// { dg-do run { target { c++17 && large_long_double } } }
// { dg-do compile { target { c++17 && { ! large_long_double } } } }

// The system printf on these targets appear to be buggy.  FIXME: Make this test
// more portable and robust to differences in system printf behavior.
// { dg-xfail-run-if "Non-conforming printf (see PR98384)" { *-*-solaris* *-*-darwin* } }

// { dg-require-effective-target ieee-floats }
// { dg-require-effective-target size32plus }

#include <charconv>

#include <cmath>
#include <cstring>
#include <iterator>
#include <optional>
#include <limits>

#include <testsuite_hooks.h>

using namespace std;

namespace detail
{
  long double
  nextupl(long double x)
  { return nexttowardl(x, numeric_limits<long double>::infinity()); }

  long double
  nextdownl(long double x)
  { return nexttowardl(x, -numeric_limits<long double>::infinity()); }
}

// The long double overloads of std::to_chars currently just go through printf
// (except for the hexadecimal formatting).

// Test our hand-written hexadecimal formatting implementation.
void
test01()
{
  // Verifies correctness of the hexadecimal form [BEGIN,END) for VALUE by
  // round-tripping it through from_chars (if available).
  auto verify_via_from_chars = [] (char *begin, char *end, long double value) {
#if __cpp_lib_to_chars >= 201611L
    long double roundtrip;
    auto result = from_chars(begin, end, roundtrip, chars_format::hex);
    VERIFY( result.ec == errc{} );
    VERIFY( result.ptr == end );
    VERIFY( roundtrip == value );
#endif
  };

  // Verifies correctness of the null-terminated hexadecimal form at BEGIN
  // for VALUE and PRECISION by comparing it with the output of printf's %La
  // conversion specifier.
  auto verify_via_printf = [] (char *begin, long double value,
			       optional<int> precision = nullopt) {
    char printf_buffer[1024] = {};
    if (precision.has_value())
      sprintf(printf_buffer, "%.*La", precision.value(), value);
    else
      sprintf(printf_buffer, "%La", value);

    // Only compare with the output of printf if the leading hex digits agree.
    // If the leading hex digit of our form doesn't agree with that of printf,
    // then the two forms may still be equivalent (e.g. 1.1p+0 vs 8.8p-3).  But
    // if the leading hex digits do agree, then we do expect the two forms to be
    // the same.
    if (printf_buffer[strlen("0x")] == begin[0])
      VERIFY( !strcmp(begin, printf_buffer+strlen("0x")) );
  };

  const long double hex_testcases[]
    = { detail::nextdownl(numeric_limits<long double>::max()),
	detail::nextupl(numeric_limits<long double>::min()),
	42.0L,
	0x1.2p+0L,
	0x1.23p+0L,
	0x1.234p+0L,
	0x1.2345p+0L,
	0x1.23456p+0L,
	0x1.234567p+0L,
	0x1.2345678p+0L,
	0x1.23456789p+0L,
	0x1.23456789p+0L,
	0x1.23456789ap+0L,
	0x1.23456789abp+0L,
	0x1.23456789abcp+0L,
	0x1.23456789abcdp+0L,
	0x1.23456789abcdep+0L,
	0x1.23456789abcdefp+0L,
	0x1.23456789abcdef0p+0L,
	0x1.23456789abcdef01p+0L,
	0x1.23456789abcdef012p+0L,
	0x1.23456789abcdef0123p+0L,
	0x1.23456789abcdef01234p+0L,
	0x1.23456789abcdef012345p+0L,
	0x1.23456789abcdef0123456p+0L,
	0x1.23456789abcdef01234567p+0L,
	0x1.23456789abcdef012345678p+0L,
	0x1.23456789abcdef0123456789p+0L,
	0x1.23456789abcdef0123456789ap+0L,
	0x1.23456789abcdef0123456789abp+0L,
	0x1.23456789abcdef0123456789abcp+0L,
	0x1.23456789abcdef0123456789abcdp+0L,
    };

  for (int exponent : {-11000, -3000, -300, -50, -7, 0, 7, 50, 300, 3000, 11000})
    for (long double testcase : hex_testcases)
      {
	testcase = ldexpl(testcase, exponent);
	if (testcase == 0.0L || isinf(testcase))
	  continue;

	char to_chars_buffer[1024] = {};
	auto result = to_chars(begin(to_chars_buffer), end(to_chars_buffer),
			       testcase, chars_format::hex);
	VERIFY( result.ec == errc{} );
	*result.ptr = '\0';
	verify_via_from_chars(begin(to_chars_buffer), result.ptr, testcase);
	verify_via_printf(to_chars_buffer, testcase);

	// Verify the nearby values, and also check they have a different
	// shortest form.
	for (long double nearby
	     : { detail::nextdownl(testcase), detail::nextupl(testcase) })
	  {
	    char nearby_buffer[1024] = {};
	    result = to_chars(begin(nearby_buffer), end(nearby_buffer),
			      nearby, chars_format::hex);
	    VERIFY( result.ec == errc{} );
	    *result.ptr = '\0';
	    VERIFY( strcmp(nearby_buffer, to_chars_buffer) != 0);
	    verify_via_from_chars(begin(nearby_buffer), result.ptr, nearby);
	    verify_via_printf(nearby_buffer, nearby);
	  }

	for (int precision = -1; precision < 50; precision++)
	  {
	    result = to_chars(begin(to_chars_buffer), end(to_chars_buffer),
			      testcase, chars_format::hex, precision);
	    VERIFY( result.ec == errc{} );
	    *result.ptr = '\0';
	    verify_via_printf(to_chars_buffer, testcase, precision);
	  }
      }
}

// Test the rest of the formatting modes, which go through printf.
void
test02()
{
  const long double growth_factor = 1.442695040888963407359924681001892137L;
  for (chars_format fmt : {chars_format::fixed, chars_format::scientific,
			   chars_format::general})
    for (long double __value = 1.0L, count = 0; !isinf(__value);
	 ++count <= 100.0L ? __value *= growth_factor : __value *= __value)
      for (const long double value : {__value, 1.0L/__value})
	{
	  for (const int precision : {-1, 0, 10, 100, 10000})
	    {
	      const char* const printf_specifier
		= (fmt == chars_format::fixed ? "%.*Lf"
		   : fmt == chars_format::scientific ? "%.*Le"
		   : fmt == chars_format::general ? "%.*Lg"
		   : nullptr);
	      unsigned output_length = snprintf(nullptr, 0, printf_specifier,
						precision, value);

	      char printf_buffer[output_length+1];
	      snprintf(printf_buffer, output_length+1, printf_specifier,
		       precision, value);

	      char to_chars_buffer[output_length];
	      auto result = to_chars(to_chars_buffer,
				     to_chars_buffer+output_length,
				     value, fmt, precision);
	      VERIFY( result.ec == errc{} );
	      VERIFY( !memcmp(printf_buffer, to_chars_buffer, output_length) );

	      result = to_chars(to_chars_buffer,
				to_chars_buffer+output_length-1,
				value, fmt, precision);
	      VERIFY( result.ec == errc::value_too_large );
	    }

	  // Verify that the nearby values have a different shortest form.
	  char to_chars_buffer[50000];
	  auto result = to_chars(begin(to_chars_buffer), end(to_chars_buffer),
				 value, fmt);
	  VERIFY( result.ec == errc{} );
	  *result.ptr = '\0';
	  char nearby_buffer[50000];
	    {
	      const long double smaller = detail::nextdownl(value);
	      result = to_chars(begin(nearby_buffer), end(nearby_buffer),
				smaller, fmt);
	      VERIFY( result.ec == errc{} );
	      *result.ptr = '\0';
	      VERIFY( strcmp(to_chars_buffer, nearby_buffer) != 0 );
	    }

	    {
	      long double larger = detail::nextupl(value);
	      result = to_chars(begin(nearby_buffer), end(nearby_buffer),
				larger, fmt);
	      VERIFY( result.ec == errc{} );
	      *result.ptr = '\0';
	      VERIFY( strcmp(to_chars_buffer, nearby_buffer) != 0 );
	    }
	}
}

int
main()
{
  test01();
  test02();
}
