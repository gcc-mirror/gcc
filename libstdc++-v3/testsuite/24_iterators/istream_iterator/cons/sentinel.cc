// { dg-do compile { target c++20 } }

// Copyright (C) 2020-2025 Free Software Foundation, Inc.
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

#include <iterator>
#include <istream>

// C++20 doesn't require this to be non-throwing.
static_assert( std::is_nothrow_constructible_v<std::istream_iterator<int>,
					       std::default_sentinel_t> );

constexpr std::istream_iterator<int> i = std::default_sentinel;

struct X { X() noexcept(false); };
std::istream& operator<<(std::istream&, X&);

static_assert( std::is_constructible_v<std::istream_iterator<X>,
				       std::default_sentinel_t> );
static_assert( ! std::is_nothrow_constructible_v<std::istream_iterator<X>,
						 std::default_sentinel_t> );
