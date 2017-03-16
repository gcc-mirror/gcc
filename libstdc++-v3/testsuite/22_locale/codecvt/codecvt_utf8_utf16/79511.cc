// Copyright (C) 2017 Free Software Foundation, Inc.
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

// PR libstdc++/79511

template<typename ElemT>
  std::basic_string<ElemT> conv(const char* src)
  {
    std::wstring_convert<std::codecvt_utf8_utf16<ElemT>, ElemT> conv;
    return conv.from_bytes(src);
  }

void
test01()
{
  static char const src[] = "\xEF\xBF\xBF";
  VERIFY( conv<char16_t>(src) == u"\xffff" );
  VERIFY( conv<char32_t>(src) == U"\xffff" );
#ifdef _GLIBCXX_USE_WCHAR_T
  VERIFY( conv<wchar_t>(src) == L"\xffff" );
#endif
}

void
test02()
{
  static char const src[] = "\xE2\x82\xAC";
  VERIFY( conv<char16_t>(src) == u"\x20ac" );
  VERIFY( conv<char32_t>(src) == U"\x20ac" );
#ifdef _GLIBCXX_USE_WCHAR_T
  VERIFY( conv<wchar_t>(src) == L"\x20ac" );
#endif
}

int
main()
{
  test01();
  test02();
}
