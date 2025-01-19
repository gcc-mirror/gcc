// Copyright (C) 2020-2025 Free Software Foundation, Inc.
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

// { dg-do run { target c++20 } }

// LWG 2499
// operator>>(basic_istream&, CharT*) makes it hard to avoid buffer overflows

#include <sstream>
#include <testsuite_hooks.h>

template<typename T>
void
test(std::basic_istream<wchar_t, T>& in)
{
  wchar_t wc[3];
  in >> wc;
  VERIFY( in.good() );
  VERIFY( wc[0] == L'a' && wc[1] == L'b' && wc[2] == L'\0' );

  wc[2] = L'#';
  in >> wc;
  VERIFY( in.good() );
  VERIFY( wc[0] == L'c' && wc[1] == L'\0' && wc[2] == L'#' );

  in >> wc;
  VERIFY( in.good() );
  VERIFY( wc[0] == L'd' && wc[1] == L'\0' && wc[2] == L'#' );

  wc[2] = L'#';
  in >> wc;
  VERIFY( in.eof() );
  VERIFY( wc[0] == L'e' && wc[1] == L'\0' && wc[2] == L'#' );
}

void
test01()
{
  std::wistringstream in(L"abc d e");
  test(in);
}

void
test02()
{
  struct WT : std::char_traits<wchar_t> { };
  std::basic_istringstream<wchar_t, WT> in(L"abc d e");
  test(in);
}

int main()
{
  test01();
  test02();
}
