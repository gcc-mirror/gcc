// { dg-do run { target c++11 } }
// { dg-require-string-conversions "" }
// { dg-xfail-run-if "broken long double IO" { newlib_broken_long_double_io  } }

// 2008-06-15  Paolo Carlini  <paolo.carlini@oracle.com>

// Copyright (C) 2008-2018 Free Software Foundation, Inc.
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
  bool test = false;

  using namespace std;

  try
    {
      string one;
      stod(one);      
    }
  catch(std::invalid_argument)
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
      stod(one);      
    }
  catch(std::invalid_argument)
    {
      test = true;
    }
  catch(...)
    {
    }
  VERIFY( test );

  double d1 = 0.0;
  size_t idx1 = 0;
  try
    {
      string one("2.0a");
      d1 = stod(one, &idx1);      
    }
  catch(...)
    {
      test = false;
    }
  VERIFY( test );
  VERIFY( d1 == 2.0 );
  VERIFY( idx1 == 3 );

  test = false;
  try
    {
      string one("1e");
      one.append(2 * numeric_limits<double>::max_exponent10, '9');
      d1 = stod(one);
    }
  catch(std::out_of_range)
    {
      test = true;
    }
  catch(...)
    {
    }
  VERIFY( test );
  VERIFY( d1 == 2.0 );

  try
    {
      long double ld0 = numeric_limits<double>::max() / 100.0;
      string one(to_string(ld0));
      stod(one);
    }
  catch(...)
    {
      test = false;
    }
  VERIFY( test );

  if ((numeric_limits<long double>::max() / 10000.0L)
      > numeric_limits<double>::max())
    {
      test = false;
      d1 = -1.0;
      try
	{
	  long double ld1 = numeric_limits<double>::max();
	  ld1 *= 100.0;
	  string one(to_string(ld1));
	  d1 = stod(one);
	}
      catch(std::out_of_range)
	{
	  test = true;
	}
      catch(...)
	{
	}
      VERIFY( test );
      VERIFY( d1 == -1.0 );
    }
}

int main()
{
  test01();
  return 0;
}
