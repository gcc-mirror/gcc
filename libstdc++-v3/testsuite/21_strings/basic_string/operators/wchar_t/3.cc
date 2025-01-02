// 2010-12-17  Paolo Carlini  <paolo.carlini@oracle.com>
//
// Copyright (C) 2010-2025 Free Software Foundation, Inc.
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
  using std::wstring;

  VERIFY( (wstring(L"abc") + wstring(L"def")
	   == wstring(L"abcdef")) );
  wstring s1(L"abc");
  VERIFY( s1 + wstring(L"def") == wstring(L"abcdef") );
  wstring s2(L"def");
  VERIFY( wstring(L"abc") + s2 == wstring(L"abcdef") );
  VERIFY( wstring(L"abc") + L'd' == wstring(L"abcd") );
  VERIFY( wstring(L"abc") + L"def" == wstring(L"abcdef") );
  VERIFY( L'a' + wstring(L"bcd") == wstring(L"abcd") );
  VERIFY( L"abc" + wstring(L"def") == wstring(L"abcdef") );

  VERIFY( (wstring(L"abcdefghij") + wstring(L"klmnopqrst")
	   == wstring(L"abcdefghijklmnopqrst")) );
  wstring s1l(L"abcdefghij");
  VERIFY( (s1l + wstring(L"klmnopqrst")
	   == wstring(L"abcdefghijklmnopqrst")) );
  wstring s2l(L"klmnopqrst");
  VERIFY( (wstring(L"abcdefghij") + s2l
	   == wstring(L"abcdefghijklmnopqrst")) );
  VERIFY( (wstring(L"abcdefghijklmno") + L'p'
	   == wstring(L"abcdefghijklmnop")) );
  VERIFY( (wstring(L"abcdefghijklmno") + L"pqrst"
	   == wstring(L"abcdefghijklmnopqrst")) );
  VERIFY( (L'a' + wstring(L"bcdefghijklmnop")
	   == wstring(L"abcdefghijklmnop")) );
  VERIFY( (L"abcde" + wstring(L"fghijklmnopqrst")
	   == wstring(L"abcdefghijklmnopqrst")) );

  VERIFY( (wstring(L"abcdefghijklmnopqrst") + wstring(L"uvwxy")
	   == wstring(L"abcdefghijklmnopqrstuvwxy")) );
  VERIFY( (wstring(L"abcde") + wstring(L"fghijklmnopqrstuvwxy")
	   == wstring(L"abcdefghijklmnopqrstuvwxy")) );
  wstring s1ll1(L"abcdefghijklmnopqrst");
  VERIFY( (s1ll1 + wstring(L"uvwxy")
	   == wstring(L"abcdefghijklmnopqrstuvwxy")) );
  wstring s1ll2(L"abcde");
  VERIFY( (s1ll2 + wstring(L"fghijklmnopqrstuvwxy")
	   == wstring(L"abcdefghijklmnopqrstuvwxy")) );
  wstring s2ll1(L"fghijklmnopqrstuvwxy");
  VERIFY( (wstring(L"abcde") + s2ll1
	   == wstring(L"abcdefghijklmnopqrstuvwxy")) );
  wstring s2ll2(L"uvwxy");
  VERIFY( (wstring(L"abcdefghijklmnopqrst") + s2ll2
	   == wstring(L"abcdefghijklmnopqrstuvwxy")) );
  VERIFY( (wstring(L"abcdefghijklmnopqrst") + L'u'
	   == wstring(L"abcdefghijklmnopqrstu")) );
  VERIFY( (wstring(L"abcdefghijklmnopqrst") + L"uvwxy"
	   == wstring(L"abcdefghijklmnopqrstuvwxy")) );
  VERIFY( (wstring(L"abcde") + L"fghijklmnopqrstuvwxy"
	   == wstring(L"abcdefghijklmnopqrstuvwxy")) );
  VERIFY( (L'a' + wstring(L"bcdefghijklmnopqrstuvwxy")
	   == wstring(L"abcdefghijklmnopqrstuvwxy")) );
  VERIFY( (L"abcde" + wstring(L"fghijklmnopqrstuvwxy")
	   == wstring(L"abcdefghijklmnopqrstuvwxy")) );
  VERIFY( (L"abcdefghijklmnopqrst" + wstring(L"uvwxy")
	   == wstring(L"abcdefghijklmnopqrstuvwxy")) );
}

int main()
{
  test01();
  return 0;
}
