// Copyright (C) 2019-2023 Free Software Foundation, Inc.
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

static_assert( std::ranges::output_range<int(&)[1], int> );
static_assert( ! std::ranges::output_range<const int(&)[1], int> );
static_assert( std::ranges::output_range<int[1], int> );
static_assert( ! std::ranges::output_range<int[1], int*> );

static_assert( std::ranges::input_range<int(&)[1]> );
static_assert( std::ranges::input_range<const int(&)[1]> );
static_assert( std::ranges::input_range<int[1]> );

static_assert( std::ranges::contiguous_range<int(&)[1]> );
static_assert( std::ranges::contiguous_range<const int(&)[1]> );
static_assert( std::ranges::contiguous_range<int[1]> );

using namespace __gnu_test;

static_assert( std::ranges::output_range<test_contiguous_range<int>, int> );
static_assert( std::ranges::output_range<test_random_access_range<int>, int> );
static_assert( std::ranges::output_range<test_bidirectional_range<int>, int> );
static_assert( std::ranges::output_range<test_forward_range<int>, int> );
static_assert( ! std::ranges::output_range<test_input_range<int>, int> );
static_assert( std::ranges::output_range<test_output_range<int>, int> );

static_assert( std::ranges::input_range<test_contiguous_range<int>> );
static_assert( std::ranges::input_range<test_random_access_range<int>> );
static_assert( std::ranges::input_range<test_bidirectional_range<int>> );
static_assert( std::ranges::input_range<test_forward_range<int>> );
static_assert( std::ranges::input_range<test_input_range<int>> );
static_assert( ! std::ranges::input_range<test_output_range<int>> );

static_assert( std::ranges::forward_range<test_contiguous_range<int>> );
static_assert( std::ranges::forward_range<test_random_access_range<int>> );
static_assert( std::ranges::forward_range<test_bidirectional_range<int>> );
static_assert( std::ranges::forward_range<test_forward_range<int>> );
static_assert( ! std::ranges::forward_range<test_input_range<int>> );
static_assert( ! std::ranges::forward_range<test_output_range<int>> );

static_assert( std::ranges::bidirectional_range<test_contiguous_range<int>> );
static_assert( std::ranges::bidirectional_range<test_random_access_range<int>>);
static_assert( std::ranges::bidirectional_range<test_bidirectional_range<int>>);
static_assert( ! std::ranges::bidirectional_range<test_forward_range<int>> );
static_assert( ! std::ranges::bidirectional_range<test_input_range<int>> );
static_assert( ! std::ranges::bidirectional_range<test_output_range<int>> );

static_assert( std::ranges::random_access_range<test_contiguous_range<int>> );
static_assert( std::ranges::random_access_range<test_random_access_range<int>>);
static_assert( ! std::ranges::random_access_range<test_bidirectional_range<int>>);
static_assert( ! std::ranges::random_access_range<test_forward_range<int>> );
static_assert( ! std::ranges::random_access_range<test_input_range<int>> );
static_assert( ! std::ranges::random_access_range<test_output_range<int>> );

static_assert( std::ranges::contiguous_range<test_contiguous_range<int>> );
static_assert( ! std::ranges::contiguous_range<test_random_access_range<int>>);
static_assert( ! std::ranges::contiguous_range<test_bidirectional_range<int>>);
static_assert( ! std::ranges::contiguous_range<test_forward_range<int>> );
static_assert( ! std::ranges::contiguous_range<test_input_range<int>> );
static_assert( ! std::ranges::contiguous_range<test_output_range<int>> );
