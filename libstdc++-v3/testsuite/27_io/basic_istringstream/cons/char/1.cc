// Copyright (C) 2020 Free Software Foundation, Inc.
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
  std::istringstream::allocator_type a;
  std::istringstream stm(std::ios_base::in, a);
}

auto const cstr = "This is a test";

void
test02()
{
  std::string s1(cstr);
  std::istringstream stm(std::move(s1));
  VERIFY( s1.empty() );

  std::string s2(cstr);
  VERIFY( stm.str() == s2 );
}

void
test03()
{
  using alloc_type = __gnu_test::tracker_allocator<char>;
  using str_type = std::basic_string<char, std::char_traits<char>, alloc_type>;

  auto const mode = std::ios_base::in;
  str_type s1(cstr);

  {
    std::istringstream::allocator_type a;
    std::istringstream sbuf(s1, mode, a);
    std::string s2(cstr);
    VERIFY( sbuf.str() == s2 );
  }

  {
    std::istringstream sbuf(s1, mode);
    std::string s2(cstr);
    VERIFY( sbuf.str() == s2 );
  }

  {
    std::istringstream sbuf(s1);
    std::string s2(cstr);
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
