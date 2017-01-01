// { dg-options "-std=gnu++17" }
// { dg-do run }

// Copyright (C) 2016-2017 Free Software Foundation, Inc.
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

#include <any>
#include <testsuite_hooks.h>
#include <vector>
#include <tuple>

struct combined {
  std::vector<int> v;
  std::tuple<int, int> t;
  template<class... Args>
  combined(std::initializer_list<int> il, Args&&... args)
    : v(il), t(std::forward<Args>(args)...)
  {
  }
};

int main()
{
  const int i = 42;
  std::any o(std::in_place_type<int>, i);
  int& i2 = std::any_cast<int&>(o);
  VERIFY( i2 == 42 );
  VERIFY( &i2 != &i );
  std::any o2(std::in_place_type<std::tuple<int, int>>, 1, 2);
  std::tuple<int, int>& t = std::any_cast<std::tuple<int, int>&>(o2);
  VERIFY( std::get<0>(t) == 1 && std::get<1>(t) == 2);
  std::any o3(std::in_place_type<std::vector<int>>, {42, 666});
  std::vector<int>& v = std::any_cast<std::vector<int>&>(o3);
  VERIFY(v[0] == 42 && v[1] == 666);
  std::any o4(std::in_place_type<combined>, {42, 666});
  combined& c = std::any_cast<combined&>(o4);
  VERIFY(c.v[0] == 42 && c.v[1] == 666
	 && std::get<0>(c.t) == 0 && std::get<1>(c.t) == 0 );
  std::any o5(std::in_place_type<combined>, {1, 2}, 3, 4);
  combined& c2 = std::any_cast<combined&>(o5);
  VERIFY(c2.v[0] == 1 && c2.v[1] == 2
	 && std::get<0>(c2.t) == 3 && std::get<1>(c2.t) == 4 );
  std::any o6(std::in_place_type<int&>, i);
  VERIFY(o6.type() == o.type());
  std::any o7(std::in_place_type<void()>, nullptr);
  std::any o8(std::in_place_type<void(*)()>, nullptr);
  VERIFY(o7.type() == o8.type());
  std::any o9(std::in_place_type<char(&)[42]>, nullptr);
  std::any o10(std::in_place_type<char*>, nullptr);
  VERIFY(o9.type() == o10.type());
}
