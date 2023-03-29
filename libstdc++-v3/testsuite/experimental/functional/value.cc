// Copyright (C) 2014-2023 Free Software Foundation, Inc.
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

// { dg-do compile { target c++14 } }

#include <experimental/functional>

// These tests are rather simple, the front-end tests already test
// variable templates, and the library tests for the underlying
// traits are more elaborate. These are just simple sanity tests.

int f(int);

using B = decltype(std::bind(f, std::placeholders::_1));

static_assert(!std::experimental::is_bind_expression_v<int>
	      && !std::is_bind_expression<int>::value, "");

static_assert(std::experimental::is_bind_expression_v<B>
	      && std::is_bind_expression<B>::value, "");

using PH = decltype(std::placeholders::_1);

static_assert(!std::experimental::is_placeholder_v<int>
	      && !std::is_placeholder<int>::value, "");

static_assert(std::experimental::is_placeholder_v<PH>
	      && std::is_placeholder<PH>::value, "");
