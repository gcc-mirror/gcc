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

// test conversion errors, with and without error strings

void test01()
{
  typedef str_conv<char> sc;

  const sc::byte_string berr = "invalid wide string";
  const sc::wide_string werr = "invalid byte string";

  sc c(berr, werr);
  string input = "Stop";
  input += char(0xFF);
  string woutput = c.from_bytes(input);
  VERIFY( input == woutput ); // noconv case doesn't detect invalid input
  string winput = "Stop";
  winput += char(0xFF);
  string output = c.to_bytes(winput);
  VERIFY( winput == output ); // noconv case doesn't detect invalid input
}

void test02()
{
  typedef str_conv<char16_t> sc;

  const sc::byte_string berr = "invalid wide string";
  const sc::wide_string werr = u"invalid byte string";

  sc c(berr, werr);
  string input = "Stop";
  input += char(0xFF);
  u16string woutput = c.from_bytes(input);
  VERIFY( werr == woutput );
  u16string winput = u"Stop";
  winput += char16_t(0xDC00);
  string output = c.to_bytes(winput);
  VERIFY( berr == output );
}

void test03()
{
  typedef str_conv<char32_t> sc;

  const sc::byte_string berr = "invalid wide string";
  const sc::wide_string werr = U"invalid byte string";

  sc c(berr, werr);
  string input = "Halt";
  input += char(0xff);
  u32string woutput = c.from_bytes(input);
  VERIFY( werr == woutput );
  u32string winput = U"Halt";
  winput += char32_t(-1);
  string output = c.to_bytes(winput);
  VERIFY( berr == output );
}

int main()
{
  test01();
  test02();
  test03();
}
