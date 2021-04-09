// { dg-do run { target c++11 } }
// { dg-timeout-factor 2 }

//
// 2013-09-02  Tim Shen <timshen91@gmail.com>
//
// Copyright (C) 2013-2021 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// 28.11.2 regex_match
// Tests ECMAScript back-refernce against a std::string.

#include <regex>
#include <testsuite_hooks.h>
#include <testsuite_regex.h>

using namespace __gnu_test;
using namespace std;

void
test01()
{
  regex re("([A-Z])\\1*");
  smatch m;
  {
    string s = "AAAA";
    regex_match(s, m, re);
    VERIFY( m[0].matched );
    VERIFY( m[1].matched );
    VERIFY( std::string(m[0].first, m[0].second) == "AAAA" );
    VERIFY( std::string(m[1].first, m[1].second) == "A" );
  }
  {
    string s = "BBBB";
    regex_match(s, m, re);
    VERIFY( m[0].matched );
    VERIFY( m[1].matched );
    VERIFY( std::string(m[0].first, m[0].second) == "BBBB" );
    VERIFY( std::string(m[1].first, m[1].second) == "B" );
  }
  {
    string s = "BBBA";
    regex_match(s, m, re);
    VERIFY( !m[0].matched );
    VERIFY( !m[1].matched );
  }
  {
    try
      {
        regex re("(a(b)(c\\1(d)))");
        VERIFY( false );
      }
    catch (...)
      {
        VERIFY( true );
      }
  }
}

int
main()
{
  test01();
  return 0;
}
