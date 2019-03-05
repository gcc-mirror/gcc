// Copyright (C) 2018-2019 Free Software Foundation, Inc.
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

// { dg-do compile { target c++11 } }

#include <functional>

// LWG 2993. reference_wrapper<T> conversion from T&&

static_assert(std::is_convertible<std::reference_wrapper<int>,
				  std::reference_wrapper<const int>>::value,
	      "LWG 2993 enables qualification conversions");

// The comments below are taken from the issue discussion and describe the
// behaviour before the resolution of LWG 2993. There should be no errors now.

struct convertible_from_int { convertible_from_int(int) { } };

void
test01()
{

  void meow(std::reference_wrapper<int>); //#1
  void meow(convertible_from_int); //#2
  // error, ambiguous; would unambiguously call #2 if #1 instead took int&
  meow(0);
}

void
test02()
{
  std::reference_wrapper<int> purr();

  // error, ambiguous: ICS exists from int prvalue to
  // reference_wrapper<int> and from reference_wrapper<int> to int
  auto x = true ? purr() : 0;

  // error: no member 'type' because the conditional
  // expression is ill-formed
  using t = typename std::common_type<std::reference_wrapper<int>, int>::type;
}
