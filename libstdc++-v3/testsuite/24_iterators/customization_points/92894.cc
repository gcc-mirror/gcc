// Copyright (C) 2020-2024 Free Software Foundation, Inc.
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

// { dg-do compile { target c++20 } }

#include <iterator>

using namespace std;

// Define our own of version of indirectly_readable_impl here,
// to check the use of iter_move even if the real concept in
// <bits/iterator_concepts.h> no longer uses iter_move.
template<class In>
concept indirectly_readable_impl
  = requires(const In in)
      {
	typename iter_value_t<In>;
	typename iter_reference_t<In>;
	typename iter_rvalue_reference_t<In>;
	{ *in } -> same_as<iter_reference_t<In>>;
	{ ranges::iter_move(in) } -> same_as<iter_rvalue_reference_t<In>>;
      };

template<class T> requires indirectly_readable_impl<projected<T*, identity>>
  void algo(T)
  { }

void
test01()
{
  // PR libstdc++/92894
  // Verify that the use of range::iter_move above doesn't cause odr-use of
  // projected<local-class-type, identity>::operator* (which is not defined).
  struct X { };
  X a;
  algo(a);
}
