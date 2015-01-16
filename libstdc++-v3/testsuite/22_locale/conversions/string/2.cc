// { dg-options "-std=gnu++11" }

// Copyright (C) 2012 Free Software Foundation
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
using std::wstring;

// test conversion errors, with and without error strings

void test01()
{
  typedef str_conv<wchar_t> sc;

  const sc::byte_string berr = "invalid wide string";
  const sc::wide_string werr = L"invalid byte string";

  sc c(berr, werr);
  string input = "Stop";
  input += char(0xff);
  input += char(0xff);
  wstring woutput = c.from_bytes(input);
  VERIFY( werr == woutput );
  wstring winput = L"Stop";
  winput += wchar_t(0xff);
  winput += wchar_t(0xff);
  string output = c.to_bytes(winput);
  VERIFY( berr == output );
}

int main()
{
  test01();
}
