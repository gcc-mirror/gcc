// Copyright (C) 2018-2024 Free Software Foundation, Inc.
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

// { dg-do run { target *-*-mingw* } }
// { dg-require-fileio "" }

#include <fstream>
#include <testsuite_hooks.h>

void test01()
{
  const wchar_t* name = L"ofstream_name.txt";
  std::ofstream ofs(name);
  VERIFY( ofs.is_open() );
}

void test02()
{
  const wchar_t name[] = L"ofstream_name.txt";
  std::wofstream wofs(name, std::wios::out);
  VERIFY( wofs.is_open() );
}

#if __cplusplus >= 201103L
using std::is_constructible;
using std::ofstream;
using std::wofstream;
using std::ios;
using std::wios;
static_assert(is_constructible<ofstream, const wchar_t*>::value);
static_assert(is_constructible<ofstream, wchar_t*>::value);
static_assert(is_constructible<ofstream, const wchar_t(&)[1]>::value);
static_assert(is_constructible<ofstream, wchar_t(&)[1]>::value);
static_assert(is_constructible<ofstream, const wchar_t*, ios::openmode>::value);
static_assert(is_constructible<ofstream, wchar_t*, ios::openmode>::value);
static_assert(is_constructible<ofstream, const wchar_t(&)[1], ios::openmode>::value);
static_assert(is_constructible<ofstream, wchar_t(&)[1], ios::openmode>::value);
static_assert(is_constructible<wofstream, const wchar_t*>::value);
static_assert(is_constructible<wofstream, wchar_t*>::value);
static_assert(is_constructible<wofstream, const wchar_t(&)[1]>::value);
static_assert(is_constructible<wofstream, wchar_t(&)[1]>::value);
static_assert(is_constructible<wofstream, const wchar_t*, wios::openmode>::value);
static_assert(is_constructible<wofstream, wchar_t*, wios::openmode>::value);
static_assert(is_constructible<wofstream, const wchar_t(&)[1], wios::openmode>::value);
static_assert(is_constructible<wofstream, wchar_t(&)[1], wios::openmode>::value);
#endif

int
main()
{
  test01();
  test02();
}
