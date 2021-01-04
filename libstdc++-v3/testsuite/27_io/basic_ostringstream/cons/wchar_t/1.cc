// Copyright (C) 2020-2021 Free Software Foundation, Inc.
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

// C++20 29.8.2.2  basic_stringbuf constructors  [stringbuf.cons

// { dg-options "-std=gnu++2a" }
// { dg-do run { target c++2a } }
// { dg-require-effective-target cxx11-abi }

#include <sstream>
#include <string>
#include <testsuite_allocator.h>
#include <testsuite_hooks.h>

void
test01()
{
  std::wostringstream::allocator_type a;
  std::wostringstream stm(std::ios_base::out, a);
}

auto const cstr = L"This is a test";

void
test02()
{
  std::wstring s1(cstr);
  std::wostringstream stm(std::move(s1));
  VERIFY( s1.empty() );

  std::wstring s2(cstr);
  VERIFY( stm.str() == s2 );
}

void
test03()
{
  using alloc_type = __gnu_test::tracker_allocator<wchar_t>;
  using str_type = std::basic_string<wchar_t, std::char_traits<wchar_t>, alloc_type>;

  auto const mode = std::ios_base::out;
  str_type s1(cstr);

  {
    std::wostringstream::allocator_type a;
    std::wostringstream sbuf(s1, mode, a);
    std::wstring s2(cstr);
    VERIFY( sbuf.str() == s2 );
  }

  {
    std::wostringstream sbuf(s1, mode);
    std::wstring s2(cstr);
    VERIFY( sbuf.str() == s2 );
  }

  {
    std::wostringstream sbuf(s1);
    std::wstring s2(cstr);
    VERIFY( sbuf.str() == s2 );
  }
}

int
main()
{
  test01();
  test02();
  test03();
  return 0;
}
