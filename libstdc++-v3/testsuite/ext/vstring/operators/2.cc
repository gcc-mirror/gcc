// 2010-12-19  Paolo Carlini  <paolo.carlini@oracle.com>
//
// Copyright (C) 2010-2019 Free Software Foundation, Inc.
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
// { dg-do run { target c++11 } }
// { dg-require-string-conversions "" }

#include <ext/vstring.h>
#include <testsuite_hooks.h>

void test01()
{
  using __gnu_cxx::__vstring;
  using std::move;

  __vstring s01("abc");
  s01.reserve(30);
  __vstring s02("def");
  s02.reserve(30); 
  VERIFY( move(s01) + move(s02) == __vstring("abcdef") );

  __vstring s03("abcdefghijklmnopqrstuvw");
  __vstring s04("xyz");
  s04.reserve(30); 
  VERIFY( move(s03) + move(s04) == __vstring("abcdefghijklmnopqrstuvwxyz") );

  __vstring s05("abc");
  s05.reserve(30);
  __vstring s06("defghijklmnopqrstuvwxyz");
  VERIFY( move(s05) + move(s06) == __vstring("abcdefghijklmnopqrstuvwxyz") );

  const __vstring sc1("abc");
  __vstring s07("def");
  s07.reserve(30);
  VERIFY( sc1 + move(s07) == __vstring("abcdef") );

  const __vstring sc2("def");
  __vstring s08("abc");
  s08.reserve(30);
  VERIFY( move(s08) + sc2 == __vstring("abcdef") );
  
  __vstring s09("abc");
  s09.reserve(30);
  VERIFY( move(s09) + 'd' == __vstring("abcd") );

  __vstring s10("abc");
  s10.reserve(30);
  VERIFY( move(s10) + "def" == __vstring("abcdef") );

  __vstring s11("bcd");
  s11.reserve(30);
  VERIFY( 'a' + move(s11) == __vstring("abcd") );

  __vstring s12("def");
  s12.reserve(30);
  VERIFY( "abc" + move(s12) == __vstring("abcdef") );
}

int main()
{
  test01();
  return 0;
}
