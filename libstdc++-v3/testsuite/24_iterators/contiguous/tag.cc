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

#include <iterator>

using std::contiguous_iterator_tag;
using std::random_access_iterator_tag;
using std::iterator_traits;

static_assert( std::is_empty_v<std::contiguous_iterator_tag> );
static_assert( std::is_trivially_copy_constructible_v<std::contiguous_iterator_tag> );

static_assert( std::is_base_of_v<std::random_access_iterator_tag,
				 std::contiguous_iterator_tag> );
static_assert( std::is_convertible_v<std::contiguous_iterator_tag*,
				     std::random_access_iterator_tag*> );

static_assert( ! std::is_same_v<std::iterator_traits<int*>::iterator_category,
				std::contiguous_iterator_tag> );
static_assert( std::is_same_v<std::iterator_traits<int*>::iterator_concept,
			      std::contiguous_iterator_tag> );
