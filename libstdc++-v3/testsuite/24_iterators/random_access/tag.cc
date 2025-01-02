// Copyright (C) 2019-2025 Free Software Foundation, Inc.
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

// { dg-do compile { target c++11 } }

#include <iterator>

using std::random_access_iterator_tag;
using std::bidirectional_iterator_tag;
using std::iterator_traits;

static_assert( std::is_empty<random_access_iterator_tag>::value );
static_assert( std::is_trivially_copy_constructible<random_access_iterator_tag>::value );

static_assert( std::is_base_of<bidirectional_iterator_tag,
			       random_access_iterator_tag>::value );
static_assert( std::is_convertible<random_access_iterator_tag*,
				   bidirectional_iterator_tag*>::value );

static_assert( std::is_same<iterator_traits<int*>::iterator_category,
			    random_access_iterator_tag>::value );
