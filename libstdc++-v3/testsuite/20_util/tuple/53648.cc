// { dg-do compile { target c++11 } }

// Copyright (C) 2012-2016 Free Software Foundation, Inc.
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

// libstdc++/53648

#include <tuple>
#include <type_traits>

using std::tuple;

struct A { };

template class std::tuple<tuple<>>;
template class std::tuple<tuple<tuple<>>>;
template class std::tuple<A, tuple<A, tuple<A, tuple<A>>>>;
template class std::tuple<tuple<tuple<A, A>, A>, A>;

// Verify the following QoI properties are preserved

static_assert( std::is_empty<tuple<>>::value, "tuple<> is empty" );

static_assert( std::is_empty<tuple<tuple<>>>::value,
               "tuple<tuple<>> is empty" );

static_assert( sizeof(tuple<char, tuple<>>) == sizeof(char),
               "tuple<> is eligible for EBO" );

