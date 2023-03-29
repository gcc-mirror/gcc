// Copyright (C) 2017-2023 Free Software Foundation, Inc.
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

// { dg-do run { target c++11 } }

#include <codecvt>
#include <locale>
#include <string>
#include <testsuite_hooks.h>

using std::wstring_convert;
using std::codecvt_utf8;

void
test01()
{
  std::string src = (const char*)u8"1234\U00001111\U0001ffff";
  wstring_convert<codecvt_utf8<char16_t>, char16_t> c("bad", u"BAD");

  // utf-8 to ucs2 conversion should fail on character outside BMP
  auto ucs2 = c.from_bytes(src);
  VERIFY( ucs2 == u"BAD" );
  VERIFY( c.converted() == 7 );

  // ucs2 to utf-8 conversion should fail on invalid ucs2 input:
  std::u16string utf16 = u"1234\U00001111\U0001ffff";
  auto out = c.to_bytes(utf16);
  VERIFY( out == "bad" );
  VERIFY( c.converted() == 5 );

  // And should also fail on incomplete surrogate pair (not return partial):
  out = c.to_bytes(utf16.substr(0, utf16.size()-1));
  VERIFY( out == "bad" );
  VERIFY( c.converted() == 5 );
}

void
test02()
{
  std::string src = (const char*)u8"1234\U00001111\U0001ffff";
  wstring_convert<codecvt_utf8<char16_t, 0x1000>, char16_t> c("bad", u"BAD");

  // utf-8 to ucs2 conversion should fail on character above Maxcode=0x1000
  auto ucs2 = c.from_bytes(src);
  VERIFY( ucs2 == u"BAD" );
  VERIFY( c.converted() == 4 );
}

void
test03()
{
  std::string src = (const char*)u8"1234\U00001111\U0001ffff";
  wstring_convert<codecvt_utf8<char32_t, 0x10000>, char32_t> c("bad", U"BAD");

  // utf-8 to ucs4 conversion should fail on character above Maxcode=0x10000
  auto ucs4 = c.from_bytes(src);
  VERIFY( ucs4 == U"BAD" );
  VERIFY( c.converted() == 7 );
}

void
test04()
{
  std::string src = (const char*)u8"1234\U00001111\U0001ffff";
  wstring_convert<codecvt_utf8<char32_t, 0x1000>, char32_t> c("bad", U"BAD");

  // utf-8 to ucs4 conversion should fail on character above Maxcode=0x1000
  auto ucs4 = c.from_bytes(src);
  VERIFY( ucs4 == U"BAD" );
  VERIFY( c.converted() == 4 );
}

int
main()
{
  test01();
  test02();
  test03();
  test04();
}
