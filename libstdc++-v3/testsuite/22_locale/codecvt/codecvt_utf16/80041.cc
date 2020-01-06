// Copyright (C) 2017-2020 Free Software Foundation, Inc.
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
#include <testsuite_hooks.h>

void
test01()
{
#ifdef _GLIBCXX_USE_WCHAR_T
  std::codecvt_utf16<wchar_t> conv;
  const wchar_t wc = 0x6557;
  char bytes[2] = {0};
  const wchar_t* wcnext;
  std::mbstate_t st{};
  char* next = nullptr;
  auto res = conv.out(st, &wc, &wc+ 1, wcnext, bytes, std::end(bytes), next);
  VERIFY( res == std::codecvt_base::ok );
  VERIFY( wcnext == &wc + 1 );
  VERIFY( next == std::end(bytes) );
  VERIFY( bytes[0] == 0x65 );
  VERIFY( bytes[1] == 0x57 );
  VERIFY( conv.length(st, bytes, next, 1) == (next - bytes) );

  wchar_t w;
  wchar_t* wnext;
  const char* cnext;
  st = {};
  res = conv.in(st, bytes, next, cnext, &w, &w + 1, wnext);
  VERIFY( res == std::codecvt_base::ok );
  VERIFY( wnext == &w + 1 );
  VERIFY( cnext == next );
  VERIFY( w == wc );
#endif
}

void
test02()
{
#ifdef _GLIBCXX_USE_WCHAR_T
  std::codecvt_utf16<wchar_t, 0x10FFFF, std::little_endian> conv;
  wchar_t wc = 0x6557;
  char bytes[2] = {0};
  const wchar_t* wcnext;
  std::mbstate_t st{};
  char* next = nullptr;
  auto res = conv.out(st, &wc, &wc+ 1, wcnext, bytes, std::end(bytes), next);
  VERIFY( res == std::codecvt_base::ok );
  VERIFY( wcnext == &wc + 1 );
  VERIFY( next == std::end(bytes) );
  VERIFY( bytes[0] == 0x57 );
  VERIFY( bytes[1] == 0x65 );
  VERIFY( conv.length(st, bytes, next, 1) == (next - bytes) );

  wchar_t w;
  wchar_t* wnext;
  const char* cnext;
  st = {};
  res = conv.in(st, bytes, next, cnext, &w, &w + 1, wnext);
  VERIFY( res == std::codecvt_base::ok );
  VERIFY( wnext == &w + 1 );
  VERIFY( cnext == next );
  VERIFY( w == wc );
#endif
}

int main()
{
  test01();
  test02();
}
