// { dg-options "-std=gnu++17" }
// { dg-do compile }

// Copyright (C) 2014-2018 Free Software Foundation, Inc.
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

#include <ratio>

using namespace std;

// These tests are rather simple, the front-end tests already test
// variable templates, and the library tests for the underlying
// traits are more elaborate. These are just simple sanity tests.

static_assert(ratio_equal_v<ratio<1, 3>, ratio<2, 6>>
	      && ratio_equal<ratio<1, 3>, ratio<2, 6>>::value, "");

static_assert(ratio_not_equal_v<ratio<1, 3>, ratio<2, 5>>
	      && ratio_not_equal<ratio<1, 3>, ratio<2, 5>>::value, "");

static_assert(ratio_less_v<ratio<1, 4>, ratio<1, 3>>
	      && ratio_less<ratio<1, 4>, ratio<1, 3>>::value, "");

static_assert(ratio_less_equal_v<ratio<1, 4>, ratio<1, 4>>
	      && ratio_less_equal_v<ratio<1, 4>, ratio<1, 3>>
	      && ratio_less_equal<ratio<1, 4>, ratio<1, 4>>::value
	      && ratio_less_equal<ratio<1, 4>, ratio<1, 3>>::value, "");

static_assert(ratio_greater_v<ratio<1, 3>, ratio<1, 4>>
	      && ratio_greater<ratio<1, 3>, ratio<1, 4>>::value, "");

static_assert(ratio_greater_equal_v<ratio<1, 4>, ratio<1, 4>>
	      && ratio_greater_equal_v<ratio<1, 3>, ratio<1, 4>>
	      && ratio_greater_equal<ratio<1, 4>, ratio<1, 4>>::value
	      && ratio_greater_equal<ratio<1, 3>, ratio<1, 4>>::value, "");
