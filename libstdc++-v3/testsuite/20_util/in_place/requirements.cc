// { dg-options "-std=gnu++17" }
// { dg-do compile }

// Copyright (C) 2016-2018 Free Software Foundation, Inc.
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

#include <utility>
#include <type_traits>
#include <testsuite_hooks.h>

using std::in_place_t;
using std::in_place_type_t;
using std::in_place_index_t;

float f(in_place_type_t<float>);
double f(in_place_type_t<double>);
char f(in_place_index_t<0>);
unsigned int f(in_place_index_t<1>);
int f(in_place_t);

static_assert(std::is_same<decltype(f(std::in_place)), int>::value);
static_assert(std::is_same<decltype(f(std::in_place_type<float>)),
	      float>::value);
static_assert(std::is_same<decltype(f(std::in_place_type<double>)),
	      double>::value);
static_assert(std::is_same<decltype(f(std::in_place_index<0>)), char>::value);
static_assert(std::is_same<decltype(f(std::in_place_index<1>)),
	      unsigned int>::value);

template <class T, class... Args> float h(in_place_type_t<T>, Args&&...);
template <size_t N, class... Args> int h(in_place_index_t<N>, Args&&...);
template <class T> double h(in_place_t, T&&);

static_assert(std::is_same<decltype(h(std::in_place, 1)), double>::value);
static_assert(std::is_same<decltype(h(std::in_place_type<float>, 1)),
	      float>::value);
static_assert(std::is_same<decltype(h(std::in_place_index<0>, 1)),
	      int>::value);
