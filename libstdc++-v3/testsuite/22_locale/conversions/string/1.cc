// { dg-do run { target c++11 } }

// Copyright (C) 2015-2017 Free Software Foundation, Inc.
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

void test01()
{
  typedef str_conv<char> sc;  // noconv
  sc c;
  string input = "King for a day...";
  string output = c.from_bytes(input);
  VERIFY( input == output );
  VERIFY( c.converted() == output.length() );
  string roundtrip = c.to_bytes(output);
  VERIFY( input == roundtrip );
  VERIFY( c.converted() == roundtrip.length() );
}

void test02()
{
  typedef str_conv<wchar_t> wsc;
  wsc c;
  string input = "Fool for a lifetime";
  wstring output = c.from_bytes(input);
  VERIFY( c.converted() == output.length() );
  VERIFY( L"Fool for a lifetime" == output );
  string roundtrip = c.to_bytes(output);
  VERIFY( input == roundtrip );
  VERIFY( c.converted() == roundtrip.length() );

  VERIFY( c.from_bytes(input[0]) == output.substr(0, 1) );
  VERIFY( c.from_bytes(input.c_str()) == output );
  VERIFY( c.from_bytes(input.data(), input.data()+input.size()) == output );

  VERIFY( c.to_bytes(output[0]) == input.substr(0, 1) );
  VERIFY( c.to_bytes(output.c_str()) == input );
  VERIFY( c.to_bytes(output.data(), output.data()+output.size()) == input );
}

int main()
{
  test01();
  test02();
}
