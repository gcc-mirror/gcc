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

// 29.8.5.4  basic_stringstream member functions  [stringstream.members]

// { dg-options "-std=gnu++2a" }
// { dg-do run { target c++2a } }
// { dg-require-effective-target cxx11-abi }

#include <sstream>
#include <testsuite_hooks.h>
#include <testsuite_allocator.h>

void test01()
{
  const std::string s0 = "this is not a short string";
  std::stringstream ss;
  ss.str(s0);
  VERIFY( ss.str() == s0 );
  VERIFY( ss.str() == s0 );

  using Alloc = __gnu_test::uneq_allocator<char>;
  const Alloc a1(1);
  std::basic_string<char, std::char_traits<char>, Alloc> s1 = ss.str(a1);
  VERIFY( s1.get_allocator() == a1 );
  VERIFY( ss.str(a1).get_allocator() == a1 );
  VERIFY( ss.str(a1) == s1 );
  VERIFY( std::move(ss).str(a1) == s1 );
  VERIFY( std::move(ss).str(a1) == s1 );

  const Alloc a2(2);
  VERIFY( ss.str(a2).get_allocator() == a2 );
  VERIFY( ss.str(a2) == s1 );

  VERIFY( std::move(ss).str() == s0 );
  VERIFY( std::move(ss).str().empty() );
  VERIFY( ss.str().empty() );
  VERIFY( ss.str(a1).empty() );
}

void test02()
{
  std::stringstream ss("123");
  std::string str = "ABCDEF";
  ss << str;
  VERIFY( ss.str() == str );
  VERIFY( std::move(ss).str() == str );
  VERIFY( std::move(ss).str().empty() );
}

void test03()
{
  std::stringstream ss;
  using Alloc = __gnu_test::tracker_allocator<char>;
  using Str = std::basic_string<char, std::char_traits<char>, Alloc>;
  Str s1 = "string that is not short, quite long even";
  auto count1 = __gnu_test::tracker_allocator_counter::get_allocation_count();
  ss.str(s1);
  auto count2 = __gnu_test::tracker_allocator_counter::get_allocation_count();
  VERIFY( count1 == count2 );
  VERIFY( ss.str() == s1.c_str() );
}

void test04()
{
  std::stringstream ss;
  const std::string str = "Another quite long string, not at all short";
  std::string str2 = str;
  ss.str(std::move(str2));
  VERIFY( str2.empty() );
  VERIFY( ss.str() == str );
}

int main()
{
  test01();
  test02();
  test03();
  test04();
}
