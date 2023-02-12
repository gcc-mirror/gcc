// Copyright (C) 2020-2023 Free Software Foundation, Inc.
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
// { dg-do compile { target c++2a } }

#include <iterator>

using std::projected;
using std::identity;
using std::indirect_unary_predicate;

template<typename T,
	 indirect_unary_predicate<projected<T*, identity>> Pred>
  constexpr void
  all_of(T*, Pred)
  { }

void
test01()
{
  // PR libstdc++/92894
  struct X { };
  X x;
  all_of(&x, [](X&) { return false; });
}

template<class R, class Proj = identity,
	 indirect_unary_predicate<projected<R, Proj>> Pred>
  constexpr void
  find_if(R, Pred, Proj = {})
  { }

void
test02()
{
  // PR 94241
  struct s { int m; };
  s r[] = { s{0}, s{1}, s{2}, s{3} };
  find_if(r, [](auto const) { return true; });
}
