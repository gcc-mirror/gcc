// Copyright (C) 2019-2020 Free Software Foundation, Inc.
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

static_assert( std::ranges::borrowed_range<int(&)[1]> );
static_assert( std::ranges::borrowed_range<const int(&)[1]> );
static_assert( !std::ranges::borrowed_range<int[1]> );
static_assert( !std::ranges::borrowed_range<int*> );

using __gnu_test::test_contiguous_range;

static_assert( !std::ranges::borrowed_range<test_contiguous_range<int>> );
static_assert( std::ranges::borrowed_range<test_contiguous_range<int>&> );
static_assert( !std::ranges::borrowed_range<test_contiguous_range<int>&&> );

template<>
constexpr bool
  std::ranges::enable_borrowed_range<test_contiguous_range<long>> = true;

static_assert( std::ranges::borrowed_range<test_contiguous_range<long>> );
static_assert( std::ranges::borrowed_range<test_contiguous_range<long>&> );
static_assert( std::ranges::borrowed_range<test_contiguous_range<long>&&> );
