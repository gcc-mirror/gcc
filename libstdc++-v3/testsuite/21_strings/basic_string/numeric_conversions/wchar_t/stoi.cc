// { dg-do run { target c++11 } }
// { dg-require-string-conversions "" }
// 2008-06-15  Paolo Carlini  <paolo.carlini@oracle.com>

// Copyright (C) 2008-2016 Free Software Foundation, Inc.
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
#if _GLIBCXX_USE_C99_WCHAR

  bool test __attribute__((unused)) = false;
  using namespace std;

  try
    {
      wstring one;
      stoi(one);      
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
      wstring one(L"a");
      stoi(one);      
    }
  catch(std::invalid_argument)
    {
      test = true;
    }
  catch(...)
    {
    }
  VERIFY( test );

  int i1 = 0;
  try
    {
      wstring one(L"a");
      i1 = stoi(one, 0, 16);      
    }
  catch(...)
    {
      test = false;
    }
  VERIFY( test );
  VERIFY( i1 == 10 );

  size_t idx1 = 0;
  try
    {
      wstring one(L"78");
      i1 = stoi(one, &idx1, 8);      
    }
  catch(...)
    {
      test = false;
    }
  VERIFY( test );
  VERIFY( i1 == 7 );
  VERIFY( idx1 = 1 );

  try
    {
      wstring one(L"10112");
      i1 = stoi(one, &idx1, 2);      
    }
  catch(...)
    {
      test = false;
    }
  VERIFY( test );
  VERIFY( i1 == 11 );
  VERIFY( idx1 == 4 );

  try
    {
      wstring one(L"0XE");
      i1 = stoi(one, &idx1, 0);      
    }
  catch(...)
    {
      test = false;
    }
  VERIFY( test );
  VERIFY( i1 == 14 );
  VERIFY( idx1 == 3 );

  test = false;
  try
    {
      wstring one(1000, L'9');
      i1 = stoi(one);
    }
  catch(std::out_of_range)
    {
      test = true;
    }
  catch(...)
    {
    }
  VERIFY( test );
  VERIFY( i1 == 14 );

  try
    {
      i1 = numeric_limits<int>::max();
      wstring one(to_wstring((long long)i1));
      i1 = stoi(one);
    }
  catch(...)
    {
      test = false;
    }
  VERIFY( test );
  VERIFY( i1 == numeric_limits<int>::max() );

  try
    {
      i1 = numeric_limits<int>::min();
      wstring one(to_wstring((long long)i1));
      i1 = stoi(one);
    }
  catch(...)
    {
      test = false;
    }
  VERIFY( test );
  VERIFY( i1 == numeric_limits<int>::min() );

  test = false;
  i1 = 1;
  try
    {
      long long ll0 = numeric_limits<int>::max();
      ++ll0;
      wstring one(to_wstring(ll0));
      i1 = stoi(one);
    }
  catch(std::out_of_range)
    {
      test = true;
    }
  catch(...)
    {
    }
  VERIFY( test );
  VERIFY( i1 == 1 );

  test = false;
  try
    {
      long long ll1 = numeric_limits<int>::min();
      --ll1;
      wstring one(to_wstring(ll1));
      i1 = stoi(one);
    }
  catch(std::out_of_range)
    {
      test = true;
    }
  catch(...)
    {
    }
  VERIFY( test );
  VERIFY( i1 == 1 );

#endif
}

int main()
{
  test01();
  return 0;
}
