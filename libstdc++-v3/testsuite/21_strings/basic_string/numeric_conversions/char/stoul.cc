// { dg-do run { target c++11 } }
// { dg-require-string-conversions "" }

// 2008-06-15  Paolo Carlini  <paolo.carlini@oracle.com>

// Copyright (C) 2008-2019 Free Software Foundation, Inc.
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
      stoul(one);      
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
      stoul(one);      
    }
  catch(std::invalid_argument)
    {
      test = true;
    }
  catch(...)
    {
    }
  VERIFY( test );

  unsigned long ul1 = 0;
  try
    {
      string one("a");
      ul1 = stoul(one, 0, 16);      
    }
  catch(...)
    {
      test = false;
    }
  VERIFY( test );
  VERIFY( ul1 == 10 );

  size_t idx1 = 0;
  try
    {
      string one("78");
      ul1 = stoul(one, &idx1, 8);      
    }
  catch(...)
    {
      test = false;
    }
  VERIFY( test );
  VERIFY( ul1 == 7 );
  VERIFY( idx1 = 1 );

  try
    {
      string one("10112");
      ul1 = stoul(one, &idx1, 2);      
    }
  catch(...)
    {
      test = false;
    }
  VERIFY( test );
  VERIFY( ul1 == 11 );
  VERIFY( idx1 == 4 );

  try
    {
      string one("0XE");
      ul1 = stoul(one, &idx1, 0);      
    }
  catch(...)
    {
      test = false;
    }
  VERIFY( test );
  VERIFY( ul1 == 14 );
  VERIFY( idx1 == 3 );

  test = false;
  try
    {
      string one(1000, '9');
      ul1 = stoul(one);
    }
  catch(std::out_of_range)
    {
      test = true;
    }
  catch(...)
    {
    }
  VERIFY( test );
  VERIFY( ul1 == 14 );

  try
    {
      ul1 = numeric_limits<unsigned long>::max();
      string one(to_string((unsigned long long)ul1));
      ul1 = stoul(one);
    }
  catch(...)
    {
      test = false;
    }
  VERIFY( test );
  VERIFY( ul1 == numeric_limits<unsigned long>::max() );
}

int main()
{
  test01();
  return 0;
}
