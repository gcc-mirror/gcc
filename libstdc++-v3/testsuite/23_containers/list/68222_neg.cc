// Copyright (C) 2018-2025 Free Software Foundation, Inc.
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

#include <list>

void
test01()
{
  // A list of int.
  const std::list<int> nums = { 1, 2, 3, 4 };

  // Grab the iterator type.
  using list_itr_type = decltype( std::begin( nums ) );

  // Confirm cend returns the same type.
  static_assert( std::is_same< decltype( std::end( nums ) ), list_itr_type >::value, "" );

  // The list's iterator type provides a well-formed non-member operator-() with valid return type (long int)
  using substraction_type
    = decltype( std::declval<list_itr_type>() - std::declval<list_itr_type>() ); // { dg-error "no match for 'operator-'" }
}
