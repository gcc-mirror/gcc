// { dg-options "-std=gnu++0x" }

// Copyright (C) 2011-2013 Free Software Foundation, Inc.
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

#include <tuple>
#include <type_traits>
#include <testsuite_hooks.h>

template<typename T>
  typename std::decay<T>::type copy(T&& x)
  { return std::forward<T>(x); }

template<typename... Args1, typename... Args2>
  void
  check_tuple_cat(std::tuple<Args1...> t1, std::tuple<Args2...> t2)
  {
    bool test __attribute__((unused)) = true;

    typedef std::tuple<Args1..., Args2...> concatenated;
  
    auto cat1 = std::tuple_cat(     t1,       t2 );
    auto cat2 = std::tuple_cat(copy(t1),      t2 );
    auto cat3 = std::tuple_cat(     t1,  copy(t2));
    auto cat4 = std::tuple_cat(copy(t1), copy(t2));
  
    static_assert( std::is_same<decltype(cat1), concatenated>::value, "" );
    static_assert( std::is_same<decltype(cat2), concatenated>::value, "" );
    static_assert( std::is_same<decltype(cat3), concatenated>::value, "" );
    static_assert( std::is_same<decltype(cat4), concatenated>::value, "" );
  
    VERIFY( cat1 == cat2 );
    VERIFY( cat1 == cat3 );
    VERIFY( cat1 == cat4 );
  }

// libstdc++/48476
void test01()
{
  int i = 0;
  std::tuple<> t0;
  std::tuple<int&> t1(i);
  std::tuple<int&, int> t2(i, 0);
  std::tuple<int const&, int, double> t3(i, 0, 0);
  
  check_tuple_cat(t0, t0);
  check_tuple_cat(t0, t1);
  check_tuple_cat(t0, t2);
  check_tuple_cat(t0, t3);
  
  check_tuple_cat(t1, t0);
  check_tuple_cat(t1, t1);
  check_tuple_cat(t1, t2);
  check_tuple_cat(t1, t3);
  
  check_tuple_cat(t2, t0);
  check_tuple_cat(t2, t1);
  check_tuple_cat(t2, t2);
  check_tuple_cat(t2, t3);
  
  check_tuple_cat(t3, t0);
  check_tuple_cat(t3, t1);
  check_tuple_cat(t3, t2);
  check_tuple_cat(t3, t3);
}

int main()
{
  test01();
  return 0;
}
