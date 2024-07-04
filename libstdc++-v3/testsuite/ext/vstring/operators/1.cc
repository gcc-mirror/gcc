// 2010-12-15  Paolo Carlini  <paolo.carlini@oracle.com>
//
// Copyright (C) 2010-2024 Free Software Foundation, Inc.
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

  VERIFY( (__vstring("abc") + __vstring("def")
	   == __vstring("abcdef")) );
  __vstring s1("abc");
  VERIFY( s1 + __vstring("def") == __vstring("abcdef") );
  __vstring s2("def");
  VERIFY( __vstring("abc") + s2 == __vstring("abcdef") );
  VERIFY( __vstring("abc") + 'd' == __vstring("abcd") );
  VERIFY( __vstring("abc") + "def" == __vstring("abcdef") );
  VERIFY( 'a' + __vstring("bcd") == __vstring("abcd") );
  VERIFY( "abc" + __vstring("def") == __vstring("abcdef") );

  VERIFY( (__vstring("abcdefghij") + __vstring("klmnopqrst")
	   == __vstring("abcdefghijklmnopqrst")) );
  __vstring s1l("abcdefghij");
  VERIFY( (s1l + __vstring("klmnopqrst")
	   == __vstring("abcdefghijklmnopqrst")) );
  __vstring s2l("klmnopqrst");
  VERIFY( (__vstring("abcdefghij") + s2l
	   == __vstring("abcdefghijklmnopqrst")) );
  VERIFY( (__vstring("abcdefghijklmno") + 'p'
	   == __vstring("abcdefghijklmnop")) );
  VERIFY( (__vstring("abcdefghijklmno") + "pqrst"
	   == __vstring("abcdefghijklmnopqrst")) );
  VERIFY( ('a' + __vstring("bcdefghijklmnop")
	   == __vstring("abcdefghijklmnop")) );
  VERIFY( ("abcde" + __vstring("fghijklmnopqrst")
	   == __vstring("abcdefghijklmnopqrst")) );

  VERIFY( (__vstring("abcdefghijklmnopqrst") + __vstring("uvwxy")
	   == __vstring("abcdefghijklmnopqrstuvwxy")) );
  VERIFY( (__vstring("abcde") + __vstring("fghijklmnopqrstuvwxy")
	   == __vstring("abcdefghijklmnopqrstuvwxy")) );
  __vstring s1ll1("abcdefghijklmnopqrst");
  VERIFY( (s1ll1 + __vstring("uvwxy")
	   == __vstring("abcdefghijklmnopqrstuvwxy")) );
  __vstring s1ll2("abcde");
  VERIFY( (s1ll2 + __vstring("fghijklmnopqrstuvwxy")
	   == __vstring("abcdefghijklmnopqrstuvwxy")) );
  __vstring s2ll1("fghijklmnopqrstuvwxy");
  VERIFY( (__vstring("abcde") + s2ll1
	   == __vstring("abcdefghijklmnopqrstuvwxy")) );
  __vstring s2ll2("uvwxy");
  VERIFY( (__vstring("abcdefghijklmnopqrst") + s2ll2
	   == __vstring("abcdefghijklmnopqrstuvwxy")) );
  VERIFY( (__vstring("abcdefghijklmnopqrst") + 'u'
	   == __vstring("abcdefghijklmnopqrstu")) );
  VERIFY( (__vstring("abcdefghijklmnopqrst") + "uvwxy"
	   == __vstring("abcdefghijklmnopqrstuvwxy")) );
  VERIFY( (__vstring("abcde") + "fghijklmnopqrstuvwxy"
	   == __vstring("abcdefghijklmnopqrstuvwxy")) );
  VERIFY( ('a' + __vstring("bcdefghijklmnopqrstuvwxy")
	   == __vstring("abcdefghijklmnopqrstuvwxy")) );
  VERIFY( ("abcde" + __vstring("fghijklmnopqrstuvwxy")
	   == __vstring("abcdefghijklmnopqrstuvwxy")) );
  VERIFY( ("abcdefghijklmnopqrst" + __vstring("uvwxy")
	   == __vstring("abcdefghijklmnopqrstuvwxy")) );
}

int main()
{
  test01();
  return 0;
}
