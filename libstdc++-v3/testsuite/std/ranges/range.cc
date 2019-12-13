// Copyright (C) 2019 Free Software Foundation, Inc.
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

#include <ranges>
#include <testsuite_iterators.h>

static_assert( std::ranges::range<int(&)[1]> );
static_assert( std::ranges::range<const int(&)[1]> );
static_assert( std::ranges::range<int[1]> );
static_assert( !std::ranges::range<int*> );

using namespace __gnu_test;

static_assert( std::ranges::range<test_contiguous_range<int>> );
static_assert( std::ranges::range<test_contiguous_range<int>&> );
static_assert( std::ranges::range<test_random_access_range<int>> );
static_assert( std::ranges::range<test_random_access_range<int>&> );
static_assert( std::ranges::range<test_bidirectional_range<int>> );
static_assert( std::ranges::range<test_bidirectional_range<int>&> );
static_assert( std::ranges::range<test_forward_range<int>> );
static_assert( std::ranges::range<test_forward_range<int>&> );
static_assert( std::ranges::range<test_input_range<int>> );
static_assert( std::ranges::range<test_input_range<int>&> );
static_assert( std::ranges::range<test_output_range<int>> );
static_assert( std::ranges::range<test_output_range<int>&> );

static_assert( std::ranges::range<test_contiguous_sized_range<int>> );
static_assert( std::ranges::range<test_contiguous_sized_range<int>&> );
static_assert( std::ranges::range<test_random_access_sized_range<int>> );
static_assert( std::ranges::range<test_random_access_sized_range<int>&> );
static_assert( std::ranges::range<test_bidirectional_sized_range<int>> );
static_assert( std::ranges::range<test_bidirectional_sized_range<int>&> );
static_assert( std::ranges::range<test_forward_sized_range<int>> );
static_assert( std::ranges::range<test_forward_sized_range<int>&> );
static_assert( std::ranges::range<test_input_sized_range<int>> );
static_assert( std::ranges::range<test_input_sized_range<int>&> );
static_assert( std::ranges::range<test_output_sized_range<int>> );
static_assert( std::ranges::range<test_output_sized_range<int>&> );

using std::same_as;

using C = test_contiguous_range<char>;
using I = test_input_range<char>;
using O = test_output_range<char>;

static_assert( same_as<std::ranges::iterator_t<C>,
		       contiguous_iterator_wrapper<char>> );
static_assert( same_as<std::ranges::iterator_t<O>,
		       decltype(std::declval<O&>().begin())> );

static_assert( same_as<std::ranges::sentinel_t<C>,
		       contiguous_iterator_wrapper<char>> );
static_assert( same_as<std::ranges::sentinel_t<O>,
		       decltype(std::declval<O&>().end())> );

static_assert( same_as<std::ranges::range_difference_t<C>,
		       std::ptrdiff_t> );
static_assert( same_as<std::ranges::range_difference_t<O>,
		       std::ptrdiff_t> );

static_assert( same_as<std::ranges::range_value_t<O>,
		       char> );

static_assert( same_as<std::ranges::range_reference_t<I>,
		       char&> );
static_assert( same_as<std::ranges::range_reference_t<O>,
		       WritableObject<char>> );

static_assert( same_as<std::ranges::range_rvalue_reference_t<I>,
		       char&&> );
static_assert( same_as<std::ranges::range_rvalue_reference_t<O>,
		      WritableObject<char>> );
