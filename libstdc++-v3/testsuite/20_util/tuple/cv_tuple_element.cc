// { dg-do compile { target c++11 } }

// 2011-05-19  Paolo Carlini  <paolo.carlini@oracle.com>
//
// Copyright (C) 2011-2019 Free Software Foundation, Inc.
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

// Tuple

#include <tuple>

using namespace std;

static_assert(is_same<tuple_element<0, const tuple<double, void, int>>::type,
	      const double>::value, "Error");
static_assert(is_same<tuple_element<1, volatile tuple<short, void>>::type,
	      volatile void>::value, "Error");
static_assert(is_same<tuple_element<2, const volatile tuple<float,
	      char, int>>::type, const volatile int>::value, "Error");
