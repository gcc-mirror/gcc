// { dg-do run { target c++11 } }
// { dg-require-string-conversions "" }
// { dg-xfail-run-if "broken long double IO" { newlib_broken_long_double_io  } }

// 2008-06-15  Paolo Carlini  <paolo.carlini@oracle.com>

// Copyright (C) 2008-2025 Free Software Foundation, Inc.
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

// 21.4 Numeric Conversions [string.conversions]

#include <string>
#include <limits>
#include <stdexcept>
#include <testsuite_hooks.h>

void
test01()
{
  /* If these conditions are not met, basic_string.h doesn't define
     std::stold(const string&, size_t* = 0), and then the test would
     fail to compile.  */
#if (_GLIBCXX_HAVE_STRTOLD && ! _GLIBCXX_HAVE_BROKEN_STRTOLD) \
  || __DBL_MANT_DIG__ == __LDBL_MANT_DIG__
  bool test = false;
  using namespace std;

  try
    {
      string one;
      stold(one);
    }
  catch(const std::invalid_argument&)
    {
      test = true;
    }
  catch(...)
    {
    }
  VERIFY( test );

  test = false;
  try
    {
      string one("a");
      stold(one);
    }
  catch(const std::invalid_argument&)
    {
      test = true;
    }
  catch(...)
    {
    }
  VERIFY( test );

  long double ld1 = 0.0L;
  size_t idx1 = 0;
  try
    {
      string one("2.0a");
      ld1 = stold(one, &idx1);
    }
  catch(...)
    {
      test = false;
    }
  VERIFY( test );
  VERIFY( ld1 == 2.0L );
  VERIFY( idx1 == 3 );

  test = false;
  try
    {
      string one("1e");
      one.append(2 * numeric_limits<long double>::max_exponent10, '9');
      ld1 = stold(one);
    }
  catch(const std::out_of_range&)
    {
      test = true;
    }
  catch(...)
    {
    }
  VERIFY( test );
  VERIFY( ld1 == 2.0L );

  try
    {
      long double ld0 = numeric_limits<long double>::max() / 100.0L;
      string one(to_string(ld0));
      stold(one);
    }
  catch(...)
    {
      test = false;
    }
  VERIFY( test );
#endif
}

int main()
{
  test01();
  return 0;
}
