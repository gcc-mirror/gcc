// { dg-options "-std=gnu++2a" }
// { dg-do run { target c++2a } }

// Copyright (C) 2020-2021 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include <variant>
#include <testsuite_hooks.h>

struct aggressive_aggregate
{
    int a;
    int b;
};

struct dumbo
{
  dumbo() = delete;
};

void test_emplace()
{
  std::variant<aggressive_aggregate, dumbo> x;
  x.emplace<aggressive_aggregate>(1, 2);
  VERIFY(x.index() == 0);
  VERIFY(std::get<0>(x).a == 1);
  VERIFY(std::get<0>(x).b == 2);
  x.emplace<aggressive_aggregate>(1);
  VERIFY(x.index() == 0);
  VERIFY(std::get<0>(x).a == 1);
  VERIFY(std::get<0>(x).b == 0);
  x.emplace<aggressive_aggregate>();
  VERIFY(x.index() == 0);
  VERIFY(std::get<0>(x).a == 0);
  VERIFY(std::get<0>(x).b == 0);
}

void test_in_place_type_construct()
{
  using Var = std::variant<aggressive_aggregate, dumbo>;
  Var x{std::in_place_type<aggressive_aggregate>, 1,2};
  VERIFY(x.index() == 0);
  VERIFY(std::get<0>(x).a == 1);
  VERIFY(std::get<0>(x).b == 2);
  Var y{std::in_place_type<aggressive_aggregate>, 1};
  VERIFY(y.index() == 0);
  VERIFY(std::get<0>(y).a == 1);
  VERIFY(std::get<0>(y).b == 0);
  Var z{std::in_place_type<aggressive_aggregate>};
  VERIFY(z.index() == 0);
  VERIFY(std::get<0>(z).a == 0);
  VERIFY(std::get<0>(z).b == 0);
}

void test_in_place_index_construct()
{
  using Var = std::variant<aggressive_aggregate, dumbo>;
  Var x{std::in_place_index<0>, 1,2};
  VERIFY(x.index() == 0);
  VERIFY(std::get<0>(x).a == 1);
  VERIFY(std::get<0>(x).b == 2);
  Var y{std::in_place_index<0>, 1};
  VERIFY(y.index() == 0);
  VERIFY(std::get<0>(y).a == 1);
  VERIFY(std::get<0>(y).b == 0);
  Var z{std::in_place_index<0>};
  VERIFY(z.index() == 0);
  VERIFY(std::get<0>(z).a == 0);
  VERIFY(std::get<0>(z).b == 0);
}

int main()
{
  test_emplace();
  test_in_place_type_construct();
  test_in_place_index_construct();
}
