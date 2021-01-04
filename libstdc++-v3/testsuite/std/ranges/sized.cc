// Copyright (C) 2019-2021 Free Software Foundation, Inc.
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

static_assert( std::ranges::sized_range<int(&)[1]> );
static_assert( std::ranges::sized_range<const int(&)[1]> );
static_assert( std::ranges::sized_range<int[1]> );
static_assert( !std::ranges::sized_range<int*> );

using namespace __gnu_test;

// ranges::size(r) uses (end(r) - begin(r))
static_assert( std::ranges::sized_range<test_contiguous_range<int>> );
static_assert( std::ranges::sized_range<test_contiguous_range<int>&> );
static_assert( std::ranges::sized_range<test_random_access_range<int>> );
static_assert( std::ranges::sized_range<test_random_access_range<int>&> );
// ranges::size(r) is invalid, (end(r) - begin(r)) requires sized sentinel
static_assert(!std::ranges::sized_range<test_bidirectional_range<int>> );
static_assert(!std::ranges::sized_range<test_bidirectional_range<int>&> );
static_assert(!std::ranges::sized_range<test_forward_range<int>> );
static_assert(!std::ranges::sized_range<test_forward_range<int>&> );
static_assert(!std::ranges::sized_range<test_input_range<int>> );
static_assert(!std::ranges::sized_range<test_input_range<int>&> );
static_assert(!std::ranges::sized_range<test_output_range<int>> );
static_assert(!std::ranges::sized_range<test_output_range<int>&> );

// ranges::size(r) uses r.size()
static_assert( std::ranges::sized_range<test_contiguous_sized_range<int>> );
static_assert( std::ranges::sized_range<test_contiguous_sized_range<int>&> );
static_assert( std::ranges::sized_range<test_random_access_sized_range<int>> );
static_assert( std::ranges::sized_range<test_random_access_sized_range<int>&> );
static_assert( std::ranges::sized_range<test_bidirectional_sized_range<int>> );
static_assert( std::ranges::sized_range<test_bidirectional_sized_range<int>&> );
static_assert( std::ranges::sized_range<test_forward_sized_range<int>> );
static_assert( std::ranges::sized_range<test_forward_sized_range<int>&> );
static_assert( std::ranges::sized_range<test_input_sized_range<int>> );
static_assert( std::ranges::sized_range<test_input_sized_range<int>&> );
static_assert( std::ranges::sized_range<test_output_sized_range<int>> );
static_assert( std::ranges::sized_range<test_output_sized_range<int>&> );

using long_range = __gnu_test::test_random_access_sized_range<long>;
template<> constexpr bool std::ranges::disable_sized_range<long_range> = true;

// Despite being disabled, this is still a sized_range because ranges::size(r)
// works, by using (ranges::end(r) - ranges::begin(r)).
static_assert( std::ranges::sized_range<long_range> );
static_assert( std::ranges::sized_range<long_range&> );

using short_range = __gnu_test::test_bidirectional_sized_range<short>;
template<> constexpr bool std::ranges::disable_sized_range<short_range> = true;

// This is not a sized range because ranges::size(r) cannot use member size,
// or ADL size, and (ranges::end(r) - ranges::begin(r)) is ill-formed for
// bidirectional iterators.
static_assert( !std::ranges::sized_range<short_range> );
static_assert( !std::ranges::sized_range<short_range&> );
