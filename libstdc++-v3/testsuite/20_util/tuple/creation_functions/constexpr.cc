// { dg-do compile { target c++11 } }

// Copyright (C) 2011-2018 Free Software Foundation, Inc.
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


// NOTE: This makes use of the fact that we know how moveable
// is implemented on pair, and also vector. If the implementation
// changes this test may begin to fail.

#include <tuple>

// make_tuple
void
test_make_tuple()
{
  {
    typedef std::tuple<int, float> tuple_type;
    constexpr tuple_type p1 __attribute__((unused))
      = std::make_tuple(22, 22.222);
  }

  {
    typedef std::tuple<int, float, int> tuple_type;
    constexpr tuple_type p1 __attribute__((unused))
      = std::make_tuple(22, 22.222, 77799);
  }
}

// forward_as_tuple
void
test_forward_as_tuple()
{
  {
    static int i(22);
    static float f(22.222);
    typedef std::tuple<int&, float&&> tuple_type;
    constexpr tuple_type p1 __attribute__((unused))
      = std::forward_as_tuple(i, std::move(f));
  }

  {
    static int i(22);
    static float f(22.222);
    static int ii(77799);

    typedef std::tuple<int&, float&, int&&> tuple_type;
    constexpr tuple_type p1 __attribute__((unused))
      = std::forward_as_tuple(i, f, std::move(ii));
  }
}

// tie
void
test_tie()
{
  {
    static int i(22);
    static float f(22.222);
    typedef std::tuple<int&, float&> tuple_type;
    constexpr tuple_type p1 __attribute__((unused))
      = std::tie(i, f);
  }

  {
    static int i(22);
    static float f(22.222);
    static const int ii(77799);

    typedef std::tuple<int&, float&, const int&> tuple_type;
    constexpr tuple_type p1 __attribute__((unused))
      = std::tie(i, f, ii);
  }
}

// get
void
test_get()
{
  {
    typedef std::tuple<int, float> tuple_type;
    constexpr tuple_type t1 { 55, 77.77 };
    constexpr auto var __attribute__((unused))
      = std::get<1>(t1);
  }

  {
    typedef std::tuple<int, float, int> tuple_type;
    constexpr tuple_type t1 { 55, 77.77, 99 };
    constexpr auto var __attribute__((unused))
      = std::get<2>(t1);
  }
}

// tuple_cat
void
test_tuple_cat()
{
  typedef std::tuple<int, float> 	tuple_type1;
  typedef std::tuple<int, int, float> tuple_type2;

  constexpr tuple_type1 t1 { 55, 77.77 };
  constexpr tuple_type2 t2 { 55, 99, 77.77 };
  constexpr auto cat1 __attribute__((unused)) = std::tuple_cat(t1, t2);
}

namespace {

template<class T>
constexpr int zero_from_anything(T)
{
  return 0;
}

}

// ignore, see LWG 2773
void
test_ignore()
{
  constexpr auto ign1 __attribute__((unused)) = std::ignore;
  constexpr auto ign2 __attribute__((unused)) = std::make_tuple(std::ignore);
  constexpr int ign3 __attribute__((unused)) = zero_from_anything(std::ignore);
}

int
main()
{
  test_make_tuple();
  test_forward_as_tuple();
  test_tie();
  test_get();
  test_tuple_cat();
  test_ignore();

  return 0;
}
