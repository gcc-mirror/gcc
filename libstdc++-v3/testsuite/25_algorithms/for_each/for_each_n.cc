// { dg-options "-std=gnu++17" }
// { dg-do run { target c++17 } }

// Copyright (C) 2019-2020 Free Software Foundation, Inc.
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

#include <algorithm>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

void test01()
{
  using __gnu_test::test_container;
  using __gnu_test::input_iterator_wrapper;
  int array[5] = { 1, 2, 3, 4, 5 };
  test_container<int, input_iterator_wrapper> con(array);

  int sum = 0;
  struct Func
  {
    Func(int& i) : i(i) { }
    Func(Func&&) = default;
    Func& operator=(Func&&) = delete;
    void operator()(int n) const { i += n; }
    int& i;
  };

  struct Size
  {
    Size(short v) : val(v) { }
    operator short() const { return val; }
    short val;
  };
  auto res = std::for_each_n(con.begin(), Size(con.size()), Func(sum));

  VERIFY( res == con.end() );
  VERIFY( sum == 15 );
}

void
test02()
{
  using __gnu_test::test_container;
  using __gnu_test::random_access_iterator_wrapper;
  int array[5] = { 2, 4, 6, 8, 10 };
  test_container<int, random_access_iterator_wrapper> con(array);

  int prod = 1;
  struct Func
  {
    Func(int& i) : i(i) { }
    Func(Func&&) = default;
    Func& operator=(Func&&) = delete;
    void operator()(int n) const { i *= n; }
    int& i;
  };

  struct Size
  {
    Size(short v) : val(v) { }
    operator short() const { return val; }
    short val;
  };
  auto res = std::for_each_n(con.begin(), Size(con.size()), Func(prod));

  VERIFY( res == con.end() );
  VERIFY( prod == 3840 );
}

int main()
{
  test01();
  test02();
}
