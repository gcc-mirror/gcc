// 2010-12-19  Paolo Carlini  <paolo.carlini@oracle.com>
//
// Copyright (C) 2010 Free Software Foundation, Inc.
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
//
// { dg-options "-std=gnu++0x" }
// { dg-require-string-conversions "" }

#include <string>
#include <testsuite_hooks.h>

void test01()
{
  bool test __attribute__((unused)) = true;
  using std::string;
  using std::move;

  string s01("abc");
  s01.reserve(30);
  string s02("def");
  s02.reserve(30); 
  VERIFY( move(s01) + move(s02) == string("abcdef") );

  string s03("abcdefghijklmnopqrstuvw");
  string s04("xyz");
  s04.reserve(30); 
  VERIFY( move(s03) + move(s04) == string("abcdefghijklmnopqrstuvwxyz") );

  string s05("abc");
  s05.reserve(30);
  string s06("defghijklmnopqrstuvwxyz");
  VERIFY( move(s05) + move(s06) == string("abcdefghijklmnopqrstuvwxyz") );

  const string sc1("abc");
  string s07("def");
  s07.reserve(30);
  VERIFY( sc1 + move(s07) == string("abcdef") );

  const string sc2("def");
  string s08("abc");
  s08.reserve(30);
  VERIFY( move(s08) + sc2 == string("abcdef") );
  
  string s09("abc");
  s09.reserve(30);
  VERIFY( move(s09) + 'd' == string("abcd") );

  string s10("abc");
  s10.reserve(30);
  VERIFY( move(s10) + "def" == string("abcdef") );

  string s11("bcd");
  s11.reserve(30);
  VERIFY( 'a' + move(s11) == string("abcd") );

  string s12("def");
  s12.reserve(30);
  VERIFY( "abc" + move(s12) == string("abcdef") );
}

int main()
{
  test01();
  return 0;
}
