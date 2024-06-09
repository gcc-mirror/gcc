// { dg-do compile { target c++14 } }

// Copyright (C) 2014-2024 Free Software Foundation, Inc.
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

#include <experimental/system_error>
#include <future>
using std::is_error_code_enum;
using std::is_error_condition_enum;
using std::experimental::is_error_code_enum_v;
using std::experimental::is_error_condition_enum_v;

// These tests are rather simple, the front-end tests already test
// variable templates, and the library tests for the underlying
// traits are more elaborate. These are just simple sanity tests.

static_assert(is_error_code_enum_v<std::future_errc>
	      && is_error_code_enum<std::future_errc>::value, "");

static_assert(!is_error_code_enum_v<int>
	      && !is_error_code_enum<int>::value, "");

static_assert(is_error_condition_enum_v<std::errc>
	      && is_error_condition_enum<std::errc>::value, "");

static_assert(!is_error_condition_enum_v<int>
	      && !is_error_condition_enum<int>::value, "");
