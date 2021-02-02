// 2010-12-17  Paolo Carlini  <paolo.carlini@oracle.com>
//
// Copyright (C) 2010-2021 Free Software Foundation, Inc.
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

#include <string>
#include <testsuite_hooks.h>

void test01()
{
  using std::string;

  VERIFY( (string("abc") + string("def")
	   == string("abcdef")) );
  string s1("abc");
  VERIFY( s1 + string("def") == string("abcdef") );
  string s2("def");
  VERIFY( string("abc") + s2 == string("abcdef") );
  VERIFY( string("abc") + 'd' == string("abcd") );
  VERIFY( string("abc") + "def" == string("abcdef") );
  VERIFY( 'a' + string("bcd") == string("abcd") );
  VERIFY( "abc" + string("def") == string("abcdef") );

  VERIFY( (string("abcdefghij") + string("klmnopqrst")
	   == string("abcdefghijklmnopqrst")) );
  string s1l("abcdefghij");
  VERIFY( (s1l + string("klmnopqrst")
	   == string("abcdefghijklmnopqrst")) );
  string s2l("klmnopqrst");
  VERIFY( (string("abcdefghij") + s2l
	   == string("abcdefghijklmnopqrst")) );
  VERIFY( (string("abcdefghijklmno") + 'p'
	   == string("abcdefghijklmnop")) );
  VERIFY( (string("abcdefghijklmno") + "pqrst"
	   == string("abcdefghijklmnopqrst")) );
  VERIFY( ('a' + string("bcdefghijklmnop")
	   == string("abcdefghijklmnop")) );
  VERIFY( ("abcde" + string("fghijklmnopqrst")
	   == string("abcdefghijklmnopqrst")) );

  VERIFY( (string("abcdefghijklmnopqrst") + string("uvwxy")
	   == string("abcdefghijklmnopqrstuvwxy")) );
  VERIFY( (string("abcde") + string("fghijklmnopqrstuvwxy")
	   == string("abcdefghijklmnopqrstuvwxy")) );
  string s1ll1("abcdefghijklmnopqrst");
  VERIFY( (s1ll1 + string("uvwxy")
	   == string("abcdefghijklmnopqrstuvwxy")) );
  string s1ll2("abcde");
  VERIFY( (s1ll2 + string("fghijklmnopqrstuvwxy")
	   == string("abcdefghijklmnopqrstuvwxy")) );
  string s2ll1("fghijklmnopqrstuvwxy");
  VERIFY( (string("abcde") + s2ll1
	   == string("abcdefghijklmnopqrstuvwxy")) );
  string s2ll2("uvwxy");
  VERIFY( (string("abcdefghijklmnopqrst") + s2ll2
	   == string("abcdefghijklmnopqrstuvwxy")) );
  VERIFY( (string("abcdefghijklmnopqrst") + 'u'
	   == string("abcdefghijklmnopqrstu")) );
  VERIFY( (string("abcdefghijklmnopqrst") + "uvwxy"
	   == string("abcdefghijklmnopqrstuvwxy")) );
  VERIFY( (string("abcde") + "fghijklmnopqrstuvwxy"
	   == string("abcdefghijklmnopqrstuvwxy")) );
  VERIFY( ('a' + string("bcdefghijklmnopqrstuvwxy")
	   == string("abcdefghijklmnopqrstuvwxy")) );
  VERIFY( ("abcde" + string("fghijklmnopqrstuvwxy")
	   == string("abcdefghijklmnopqrstuvwxy")) );
  VERIFY( ("abcdefghijklmnopqrst" + string("uvwxy")
	   == string("abcdefghijklmnopqrstuvwxy")) );
}

int main()
{
  test01();
  return 0;
}
