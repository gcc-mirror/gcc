// Copyright (C) 2018-2023 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++2a" }
// { dg-do run { target c++2a } }
// { dg-timeout-factor 2 }

#include <regex>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

using __gnu_test::test_container;
using __gnu_test::bidirectional_iterator_wrapper;

template<typename C> struct traits : std::char_traits<C> { };

void
test01()
{
  const std::basic_string<char, traits<char>> s0, s1 = "1";
  const std::ssub_match sm, sm2;

  VERIFY( sm.compare(sm) == 0 );
  VERIFY( sm.compare(sm2) == 0 );
  VERIFY( sm.compare(sm.str()) == 0 );
  VERIFY( sm.compare(sm.str().c_str()) == 0 );
  VERIFY( sm.compare(sm2.str()) == 0 );
  VERIFY( sm.compare(sm2.str().c_str()) == 0 );
  VERIFY( sm.compare(std::string(s1.c_str())) == -1 );
  VERIFY( sm.compare(s1.c_str()) == -1 );


  VERIFY( sm == sm2 );
  VERIFY( !(sm != sm2) );
  VERIFY( !(sm < sm2) );
  VERIFY( !(sm > sm2) );
  VERIFY( sm <= sm2 );
  VERIFY( sm >= sm2 );
  VERIFY( std::is_eq(sm <=> sm2) );

  VERIFY( sm == s0 );
  VERIFY( !(sm != s0) );
  VERIFY( !(sm < s0) );
  VERIFY( !(sm > s0) );
  VERIFY( sm <= s0 );
  VERIFY( sm >= s0 );
  VERIFY( std::is_eq(sm <=> s0) );

  VERIFY( s0 == sm );
  VERIFY( !(s0 != sm) );
  VERIFY( !(s0 < sm) );
  VERIFY( !(s0 > sm) );
  VERIFY( s0 <= sm );
  VERIFY( s0 >= sm );
  VERIFY( std::is_eq(s0 <=> sm) );

  VERIFY( sm == s0.c_str() );
  VERIFY( !(sm != s0.c_str()) );
  VERIFY( !(sm < s0.c_str()) );
  VERIFY( !(sm > s0.c_str()) );
  VERIFY( sm <= s0.c_str() );
  VERIFY( sm >= s0.c_str() );
  VERIFY( std::is_eq(sm <=> s0.c_str()) );

  VERIFY( s0.c_str() == sm );
  VERIFY( !(s0.c_str() != sm) );
  VERIFY( !(s0.c_str() < sm) );
  VERIFY( !(s0.c_str() > sm) );
  VERIFY( s0.c_str() <= sm );
  VERIFY( s0.c_str() >= sm );
  VERIFY( std::is_eq(s0.c_str() <=> sm) );

  VERIFY( !(sm == s1) );
  VERIFY( sm != s1 );
  VERIFY( sm < s1 );
  VERIFY( !(sm > s1) );
  VERIFY( sm <= s1 );
  VERIFY( !(sm >= s1) );
  VERIFY( std::is_lt(sm <=> s1) );

  VERIFY( !(sm == s1.c_str()) );
  VERIFY( sm != s1.c_str() );
  VERIFY( sm < s1.c_str() );
  VERIFY( !(sm > s1.c_str()) );
  VERIFY( sm <= s1.c_str() );
  VERIFY( !(sm >= s1.c_str()) );
  VERIFY( std::is_lt(sm <=> s1.c_str()) );

  VERIFY( !(s1.c_str() == sm) );
  VERIFY( s1.c_str() != sm );
  VERIFY( !(s1.c_str() < sm) );
  VERIFY( s1.c_str() > sm );
  VERIFY( !(s1.c_str() <= sm) );
  VERIFY( s1.c_str() >= sm );
  VERIFY( std::is_gt(s1.c_str() <=> sm) );

  VERIFY( !(sm == s1[0]) );
  VERIFY( sm != s1[0] );
  VERIFY( sm < s1[0] );
  VERIFY( !(sm > s1[0]) );
  VERIFY( sm <= s1[0] );
  VERIFY( !(sm >= s1[0]) );
  VERIFY( std::is_lt(sm <=> s1[0]) );

  VERIFY( !(s1[0] == sm) );
  VERIFY( s1[0] != sm );
  VERIFY( !(s1[0] < sm) );
  VERIFY( s1[0] > sm );
  VERIFY( !(s1[0] <= sm) );
  VERIFY( s1[0] >= sm );
  VERIFY( std::is_gt(s1[0] <=> sm) );
}

void
test02()
{
  const std::basic_string<char, traits<char>> s0, s1 = "1";
  std::csub_match sm;
  const std::csub_match sm2;
  const char c[] = "1";
  sm.matched = true;
  sm.first = c;
  sm.second = c+1;

  VERIFY( sm.compare(sm) == 0 );
  VERIFY( sm.compare(sm2) == 1 );
  VERIFY( sm.compare(sm.str()) == 0 );
  VERIFY( sm.compare(sm.str().c_str()) == 0 );
  VERIFY( sm.compare(sm2.str()) == 1 );
  VERIFY( sm.compare(sm2.str().c_str()) == 1 );
  VERIFY( sm.compare(std::string(s1.c_str())) == 0 );
  VERIFY( sm.compare(s1.c_str()) == 0 );

  VERIFY( !(sm == sm2) );
  VERIFY( sm != sm2 );
  VERIFY( !(sm < sm2) );
  VERIFY( sm > sm2 );
  VERIFY( !(sm <= sm2) );
  VERIFY( sm >= sm2 );
  VERIFY( std::is_gt(sm <=> sm2) );

  VERIFY( !(sm2 == sm) );
  VERIFY( sm2 != sm );
  VERIFY( sm2 < sm );
  VERIFY( !(sm2 > sm) );
  VERIFY( sm2 <= sm );
  VERIFY( !(sm2 >= sm) );
  VERIFY( std::is_lt(sm2 <=> sm) );

  VERIFY( !(sm == s0) );
  VERIFY( sm != s0 );
  VERIFY( !(sm < s0) );
  VERIFY( sm > s0 );
  VERIFY( !(sm <= s0) );
  VERIFY( sm >= s0 );
  VERIFY( std::is_gt(sm <=> s0) );

  VERIFY( !(sm == s0.c_str()) );
  VERIFY( sm != s0.c_str() );
  VERIFY( !(sm < s0.c_str()) );
  VERIFY( sm > s0.c_str() );
  VERIFY( !(sm <= s0.c_str()) );
  VERIFY( sm >= s0.c_str() );
  VERIFY( std::is_gt(sm <=> s0.c_str()) );

  VERIFY( !(s0.c_str() == sm) );
  VERIFY( s0.c_str() != sm );
  VERIFY( s0.c_str() < sm );
  VERIFY( !(s0.c_str() > sm) );
  VERIFY( s0.c_str() <= sm );
  VERIFY( !(s0.c_str() >= sm) );
  VERIFY( std::is_lt(s0.c_str() <=> sm) );

  VERIFY( s1 == sm );
  VERIFY( !(s1 != sm) );
  VERIFY( !(s1 < sm) );
  VERIFY( !(s1 > sm) );
  VERIFY( s1 <= sm );
  VERIFY( s1 >= sm );
  VERIFY( std::is_eq(s1 <=> sm) );

  VERIFY( sm == s1.c_str() );
  VERIFY( !(sm != s1.c_str()) );
  VERIFY( !(sm < s1.c_str()) );
  VERIFY( !(sm > s1.c_str()) );
  VERIFY( sm <= s1.c_str() );
  VERIFY( sm >= s1.c_str() );
  VERIFY( std::is_eq(sm <=> s1.c_str()) );

  VERIFY( s1.c_str() == sm );
  VERIFY( !(s1.c_str() != sm) );
  VERIFY( !(s1.c_str() < sm) );
  VERIFY( !(s1.c_str() > sm) );
  VERIFY( s1.c_str() <= sm );
  VERIFY( s1.c_str() >= sm );
  VERIFY( std::is_eq(s1.c_str() <=> sm) );

  VERIFY( sm == s1[0] );
  VERIFY( !(sm != s1[0]) );
  VERIFY( !(sm < s1[0]) );
  VERIFY( !(sm > s1[0]) );
  VERIFY( sm <= s1[0] );
  VERIFY( sm >= s1[0] );
  VERIFY( std::is_eq(sm <=> s1[0]) );

  VERIFY( s1[0] == sm );
  VERIFY( !(s1[0] != sm) );
  VERIFY( !(s1[0] < sm) );
  VERIFY( !(s1[0] > sm) );
  VERIFY( s1[0] <= sm );
  VERIFY( s1[0] >= sm );
  VERIFY( std::is_eq(s1[0] <=> sm) );
}

void
test03()
{
  const std::basic_string<char, traits<char>> s0, s1 = "1";
  const char c[] = "1";
  test_container<const char, bidirectional_iterator_wrapper> tc(c, c+1);
  std::sub_match<bidirectional_iterator_wrapper<const char>> sm;
  const std::sub_match<bidirectional_iterator_wrapper<const char>> sm2;
  sm.matched = true;
  sm.first = tc.begin();
  sm.second = tc.end();

  VERIFY( sm.compare(sm) == 0 );
  VERIFY( sm.compare(sm2) == 1 );
  VERIFY( sm.compare(sm.str()) == 0 );
  VERIFY( sm.compare(sm.str().c_str()) == 0 );
  VERIFY( sm.compare(sm2.str()) == 1 );
  VERIFY( sm.compare(sm2.str().c_str()) == 1 );
  VERIFY( sm.compare(std::string(s1.c_str())) == 0 );
  VERIFY( sm.compare(s1.c_str()) == 0 );

  VERIFY( !(sm == sm2) );
  VERIFY( sm != sm2 );
  VERIFY( !(sm < sm2) );
  VERIFY( sm > sm2 );
  VERIFY( !(sm <= sm2) );
  VERIFY( sm >= sm2 );
  VERIFY( std::is_gt(sm <=> sm2) );

  VERIFY( !(sm2 == sm) );
  VERIFY( sm2 != sm );
  VERIFY( sm2 < sm );
  VERIFY( !(sm2 > sm) );
  VERIFY( sm2 <= sm );
  VERIFY( !(sm2 >= sm) );
  VERIFY( std::is_lt(sm2 <=> sm) );

  VERIFY( !(sm == s0) );
  VERIFY( sm != s0 );
  VERIFY( !(sm < s0) );
  VERIFY( sm > s0 );
  VERIFY( !(sm <= s0) );
  VERIFY( sm >= s0 );
  VERIFY( std::is_gt(sm <=> s0) );

  VERIFY( !(sm == s0.c_str()) );
  VERIFY( sm != s0.c_str() );
  VERIFY( !(sm < s0.c_str()) );
  VERIFY( sm > s0.c_str() );
  VERIFY( !(sm <= s0.c_str()) );
  VERIFY( sm >= s0.c_str() );
  VERIFY( std::is_gt(sm <=> s0.c_str()) );

  VERIFY( !(s0.c_str() == sm) );
  VERIFY( s0.c_str() != sm );
  VERIFY( s0.c_str() < sm );
  VERIFY( !(s0.c_str() > sm) );
  VERIFY( s0.c_str() <= sm );
  VERIFY( !(s0.c_str() >= sm) );
  VERIFY( std::is_lt(s0.c_str() <=> sm) );

  VERIFY( s1 == sm );
  VERIFY( !(s1 != sm) );
  VERIFY( !(s1 < sm) );
  VERIFY( !(s1 > sm) );
  VERIFY( s1 <= sm );
  VERIFY( s1 >= sm );
  VERIFY( std::is_eq(s1 <=> sm) );

  VERIFY( sm == s1.c_str() );
  VERIFY( !(sm != s1.c_str()) );
  VERIFY( !(sm < s1.c_str()) );
  VERIFY( !(sm > s1.c_str()) );
  VERIFY( sm <= s1.c_str() );
  VERIFY( sm >= s1.c_str() );
  VERIFY( std::is_eq(sm <=> s1.c_str()) );

  VERIFY( s1.c_str() == sm );
  VERIFY( !(s1.c_str() != sm) );
  VERIFY( !(s1.c_str() < sm) );
  VERIFY( !(s1.c_str() > sm) );
  VERIFY( s1.c_str() <= sm );
  VERIFY( s1.c_str() >= sm );
  VERIFY( std::is_eq(s1.c_str() <=> sm) );

  VERIFY( sm == s1[0] );
  VERIFY( !(sm != s1[0]) );
  VERIFY( !(sm < s1[0]) );
  VERIFY( !(sm > s1[0]) );
  VERIFY( sm <= s1[0] );
  VERIFY( sm >= s1[0] );
  VERIFY( std::is_eq(sm <=> s1[0]) );

  VERIFY( s1[0] == sm );
  VERIFY( !(s1[0] != sm) );
  VERIFY( !(s1[0] < sm) );
  VERIFY( !(s1[0] > sm) );
  VERIFY( s1[0] <= sm );
  VERIFY( s1[0] >= sm );
  VERIFY( std::is_eq(s1[0] <=> sm) );
}

int main()
{
  test01();
  test02();
  test03();
}
