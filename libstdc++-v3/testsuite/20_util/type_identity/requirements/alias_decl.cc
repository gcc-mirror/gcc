// Copyright (C) 2018-2019 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++2a" }
// { dg-do compile { target c++2a } }
//
#include <type_traits>

using namespace std;

template<typename T, typename = std::type_identity_t<T>>
  struct test; // undefined

template<typename T>
  struct test<T, T> : std::true_type { };

static_assert( test<const int>{}, "type_identity_t<const int>" );
static_assert( test<volatile int>{}, "type_identity_t<volatile int>" );
static_assert( test<unsigned>{}, "type_identity_t<unsigned>" );
static_assert( test<char>{}, "type_identity_t<char>" );
static_assert( test<signed char>{}, "type_identity_t<signed char>" );
