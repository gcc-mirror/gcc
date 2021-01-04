// Copyright (C) 2017-2021 Free Software Foundation, Inc.
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

#include <locale>
#include <codecvt>
#include <testsuite_hooks.h>

// PR libstdc++/79980

constexpr std::codecvt_mode mode(std::codecvt_mode m)
{ return static_cast<std::codecvt_mode>(m | std::consume_header); }

template<typename WCh, unsigned long Max = 0x10FFFF,
	 std::codecvt_mode Mode = std::consume_header>
  using Conv
    = std::wstring_convert<std::codecvt_utf16<WCh, Max, mode(Mode)>, WCh>;

void
test01()
{
  const char src[] = "\xFE\xFF\xAB\xCD";
  Conv<char16_t> conv;
  auto dst = conv.from_bytes(src, src+4);
  VERIFY( dst[0] == 0xabcd );
}

void
test02()
{
  const char src[] = "\xFF\xFE\xAB\xCD";
  Conv<char16_t> conv;
  auto dst = conv.from_bytes(src, src+4);
  VERIFY( dst[0] == 0xcdab );
}

void
test03()
{
  const char src[] = "\xFE\xFF\xAB\xCD";
  Conv<char16_t, 0x10FFFF, std::little_endian> conv;
  auto dst = conv.from_bytes(src, src+4);
  VERIFY( dst[0] == 0xabcd );
}

void
test04()
{
  const char src[] = "\xFF\xFE\xAB\xCD";
  Conv<char16_t, 0x10FFFF, std::little_endian> conv;
  auto dst = conv.from_bytes(src, src+4);
  VERIFY( dst[0] == 0xcdab );
}

void
test05()
{
  const char src[] = "\0\x61\xAB\xCD"; // character greater than 0x00FF
  Conv<char16_t, 0xFF> conv("to_bytes failed", u"from_bytes failed");
  std::u16string result = conv.from_bytes(src, src+4);
  VERIFY( result == u"from_bytes failed" );
  VERIFY( conv.converted() == 2 );
}

void
test06()
{
  const char src[] = "\0\x61\xAB\xCD";
  Conv<char16_t> conv("to_bytes failed", u"from_bytes failed");
  std::u16string result = conv.from_bytes(src, src+3); // incomplete character
  VERIFY( result == u"from_bytes failed" );
  VERIFY( conv.converted() == 2 );
}

void
test07()
{
  Conv<char16_t> conv("to_bytes failed", u"from_bytes failed");
  // ucs2 to utf-16 conversion should fail on invalid ucs2 input:
  std::u16string utf16 = u"1234\U00001111\U0001ffff";
  auto out = conv.to_bytes(utf16);
  VERIFY( out == "to_bytes failed" );
  VERIFY( conv.converted() == 5 );

  // And should also fail on incomplete surrogate pair (not return partial):
  out = conv.to_bytes(utf16.substr(0, utf16.size()-1));
  VERIFY( out == "to_bytes failed" );
  VERIFY( conv.converted() == 5 );
}

void
test08()
{
  // Read/write UTF-16 code units from data not correctly aligned for char16_t
  Conv<char16_t, 0x10FFFF, std::generate_header> conv;
  const char src[] = "-\xFE\xFF\0\x61\xAB\xCD";
  auto out = conv.from_bytes(src + 1, src + 7);
  VERIFY( out[0] == 0x0061 );
  VERIFY( out[1] == 0xabcd );
  auto bytes = conv.to_bytes(out);
  VERIFY( bytes == std::string(src + 1, 6) );
}

void
test09()
{
  // Read/write UTF-16 code units from data not correctly aligned for char16_t
  Conv<char32_t, 0x10FFFF, std::generate_header> conv;
  const char src[] = "-\xFE\xFF\xD8\x08\xDF\x45";
  auto out = conv.from_bytes(src + 1, src + 7);
  VERIFY( out == U"\U00012345" );
  auto bytes = conv.to_bytes(out);
  VERIFY( bytes == std::string(src + 1, 6) );
}

int main()
{
  test01();
  test02();
  test03();
  test04();
  test05();
  test06();
  test07();
  test08();
  test09();
}
