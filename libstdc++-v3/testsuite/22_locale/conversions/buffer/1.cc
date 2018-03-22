// { dg-do run { target c++11 } }

// Copyright (C) 2015-2018 Free Software Foundation, Inc.
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

// 22.3.3.2.3  Buffer conversions

#include <locale>
#include <sstream>
#include <testsuite_hooks.h>

template<typename Elem>
struct cvt : std::codecvt<Elem, char, std::mbstate_t> { };

template<typename Elem>
using buf_conv = std::wbuffer_convert<cvt<Elem>, Elem>;

using std::string;
using std::stringstream;
using std::wstring;
using std::wstringstream;

void test01()
{
  buf_conv<wchar_t> buf;
  std::stringbuf sbuf;
  VERIFY( buf.rdbuf() == nullptr );
  VERIFY( buf.rdbuf(&sbuf) == nullptr );
  VERIFY( buf.rdbuf() == &sbuf );
  VERIFY( buf.rdbuf(nullptr) == &sbuf );
}

void test02()
{
  std::stringbuf sbuf;
  buf_conv<char> buf(&sbuf);  // noconv

  stringstream ss;
  ss.std::ios::rdbuf(&buf);
  string input = "King for a day...";
  ss << input << std::flush;
  string output = sbuf.str();
  VERIFY( input == output );
}

void test03()
{
  std::stringbuf sbuf;
  buf_conv<wchar_t> buf(&sbuf);

  wstringstream ss;
  ss.std::wios::rdbuf(&buf);
  wstring input = L"Fool for a lifetime";
  ss << input << std::flush;
  string output = sbuf.str();
  VERIFY( output == "Fool for a lifetime" );
}

int main()
{
  test01();
  test02();
  test03();
}
