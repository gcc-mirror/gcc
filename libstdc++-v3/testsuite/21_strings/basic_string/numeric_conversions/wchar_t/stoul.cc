// { dg-do run { target c++11 } }
// { dg-require-string-conversions "" }
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
#if _GLIBCXX_USE_C99_WCHAR

  bool test = false;
  using namespace std;

  try
    {
      wstring one;
      stoul(one);
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
      wstring one(L"a");
      stoul(one);
    }
  catch(const std::invalid_argument&)
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
      wstring one(L"a");
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
      wstring one(L"78");
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
      wstring one(L"10112");
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
      wstring one(L"0XE");
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
      wstring one(1000, L'9');
      ul1 = stoul(one);
    }
  catch(const std::out_of_range&)
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
      wstring one(to_wstring((unsigned long long)ul1));
      ul1 = stoul(one);
    }
  catch(...)
    {
      test = false;
    }
  VERIFY( test );
  VERIFY( ul1 == numeric_limits<unsigned long>::max() );

#endif
}

int main()
{
  test01();
  return 0;
}
