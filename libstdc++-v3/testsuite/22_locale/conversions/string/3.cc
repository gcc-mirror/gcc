// { dg-do run { target c++11 } }

// Copyright (C) 2015-2020 Free Software Foundation, Inc.
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

// 22.3.3.2.2  String conversions

#include <locale>
#include <string>
#include <testsuite_hooks.h>

template<typename Elem>
struct cvt : std::codecvt<Elem, char, std::mbstate_t> { };

template<typename Elem>
using str_conv = std::wstring_convert<cvt<Elem>, Elem>;

using std::string;
using std::u16string;
using std::u32string;

// test construction with state, for partial conversions

void test01()
{
  typedef str_conv<char> wsc;

  wsc c;
  string input = (const char*)u8"\u00a3 shillings pence";
  string woutput = c.from_bytes(input.substr(0, 1));
  auto partial_state = c.state();
  auto partial_count = c.converted();

  auto woutput2 = c.from_bytes("state reset on next conversion");
  VERIFY( woutput2 == "state reset on next conversion" );

  wsc c2(new cvt<char>, partial_state);
  woutput += c2.from_bytes(input.substr(partial_count));
  VERIFY( (const char*)u8"\u00a3 shillings pence" == woutput );

  string roundtrip = c2.to_bytes(woutput);
  VERIFY( input == roundtrip );
}

void test02()
{
  typedef str_conv<char16_t> wsc;

  wsc c;
  string input = (const char*)u8"\u00a3 shillings pence";
  u16string woutput = c.from_bytes(input.substr(0, 1));
  auto partial_state = c.state();
  auto partial_count = c.converted();

  auto woutput2 = c.from_bytes("state reset on next conversion");
  VERIFY( woutput2 == u"state reset on next conversion" );

  wsc c2(new cvt<char16_t>, partial_state);
  woutput += c2.from_bytes(input.substr(partial_count));
  VERIFY( u"\u00a3 shillings pence" == woutput );

  string roundtrip = c2.to_bytes(woutput);
  VERIFY( input == roundtrip );
}

void test03()
{
  typedef str_conv<char32_t> wsc;

  wsc c;
  string input = (const char*)u8"\u00a3 shillings pence";
  u32string woutput = c.from_bytes(input.substr(0, 1));
  auto partial_state = c.state();
  auto partial_count = c.converted();

  auto woutput2 = c.from_bytes("state reset on next conversion");
  VERIFY( woutput2 == U"state reset on next conversion" );

  wsc c2(new cvt<char32_t>, partial_state);
  woutput += c2.from_bytes(input.substr(partial_count));
  VERIFY( U"\u00a3 shillings pence" == woutput );

  string roundtrip = c2.to_bytes(woutput);
  VERIFY( input == roundtrip );
}


int main()
{
  test01();
  test02();
  test03();
}
