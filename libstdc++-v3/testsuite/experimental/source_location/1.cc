// Copyright (C) 2017-2018 Free Software Foundation, Inc.
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

// { dg-do run { target c++14 } }

#include <experimental/source_location>
#include <experimental/string_view>
#include <testsuite_hooks.h>

using std::experimental::source_location;
using std::experimental::string_view;

void
test01()
{
  constexpr source_location loc = source_location::current();
  static_assert( loc.line() == 30 );
  // static_assert( loc.column() == 35 );
  VERIFY( loc.file_name() == __FILE__ );
  VERIFY( loc.function_name() == string_view(__FUNCTION__) );
}

struct S {
  string_view func;
  source_location loc = source_location::current();

  S(source_location loc = source_location::current())
  : func(__FUNCTION__), loc(loc) // values of loc will be from call-site
  {}

  S(int)
  : func(__FUNCTION__) // values of loc should be hereabouts
  {}
};

void test02()
{
  S s0;
  VERIFY( s0.loc.line() == 52 );
  // static_assert( s0.loc.column() == 7 );
  VERIFY( s0.loc.file_name() == __FILE__ );
  VERIFY( s0.loc.function_name() == string_view(__FUNCTION__) );

  S s1(1);
  VERIFY( s1.loc.line() == 46 );
  VERIFY( s1.loc.file_name() == __FILE__ );
  VERIFY( s1.loc.function_name() == s1.func );
}

source_location f(source_location a = source_location::current()) {
  return a;
}

source_location g(string_view& func) {
  source_location a = source_location::current();
  func = __FUNCTION__;
  return a;
}

void test03()
{
  auto loc = f(); // f's first argument corresponds to this line of code
  VERIFY( loc.line() == 76 );
  // static_assert( loc.column() == 16 );
  VERIFY( loc.file_name() == __FILE__ );
  VERIFY( loc.function_name() == string_view(__FUNCTION__) );

  source_location c = source_location::current();
  loc = f(c); // f's first argument gets the same values as c, above
  VERIFY( loc.line() == 82 );
  // static_assert( loc.column() == 23 );
  VERIFY( loc.file_name() == __FILE__ );
  VERIFY( loc.function_name() == string_view(__FUNCTION__) );

  string_view func;
  loc = g(func);
  VERIFY( loc.line() == 69 );
  // static_assert( loc.column() == 23 );
  VERIFY( loc.file_name() == __FILE__ );
  VERIFY( loc.function_name() == func );
}

void
test04()
{
  using std::is_same;
  using std::uint_least32_t;
  auto loc = source_location::current();
  static_assert(is_same<decltype(loc), source_location>::value, "");
  static_assert(is_same<decltype(loc.line()), uint_least32_t>::value, "");
  static_assert(is_same<decltype(loc.column()), uint_least32_t>::value, "");
  static_assert(is_same<decltype(loc.file_name()), const char*>::value, "");
  static_assert(is_same<decltype(loc.function_name()), const char*>::value, "");
}

int
main()
{
  test01();
  test02();
  test03();
  test04();
}
