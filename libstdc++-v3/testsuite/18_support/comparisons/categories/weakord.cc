// Copyright (C) 2020-2023 Free Software Foundation, Inc.
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

#include <compare>

using std::weak_ordering;

static_assert( weak_ordering::less == weak_ordering::less );
static_assert( weak_ordering::less != weak_ordering::equivalent );
static_assert( weak_ordering::less != weak_ordering::greater );
static_assert( weak_ordering::equivalent == weak_ordering::equivalent );
static_assert( weak_ordering::equivalent != weak_ordering::greater );
static_assert( weak_ordering::greater == weak_ordering::greater );

static_assert( ! (weak_ordering::less == 0)	);
static_assert(    weak_ordering::less <  0	);
static_assert( ! (weak_ordering::less >  0)	);
static_assert(    weak_ordering::less <= 0	);
static_assert( ! (weak_ordering::less >= 0)	);
static_assert( ! (0 == weak_ordering::less)	);
static_assert( ! (0 <  weak_ordering::less)	);
static_assert(    0 >  weak_ordering::less	);
static_assert( ! (0 <= weak_ordering::less)	);
static_assert(    0 >= weak_ordering::less	);
static_assert( (weak_ordering::less <=> 0) == weak_ordering::less );
static_assert( (0 <=> weak_ordering::less) == weak_ordering::greater );

static_assert(   (weak_ordering::equivalent == 0)	);
static_assert( ! (weak_ordering::equivalent <  0)	);
static_assert( ! (weak_ordering::equivalent >  0)	);
static_assert(    weak_ordering::equivalent <= 0	);
static_assert(    weak_ordering::equivalent >= 0	);
static_assert(    0 == weak_ordering::equivalent	);
static_assert( ! (0 <  weak_ordering::equivalent)	);
static_assert( ! (0 >  weak_ordering::equivalent)	);
static_assert(    0 <= weak_ordering::equivalent	);
static_assert(    0 >= weak_ordering::equivalent	);
static_assert( (weak_ordering::equivalent <=> 0) == weak_ordering::equivalent );
static_assert( (0 <=> weak_ordering::equivalent) == weak_ordering::equivalent );

static_assert( ! (weak_ordering::greater == 0)	);
static_assert( ! (weak_ordering::greater <  0)	);
static_assert(    weak_ordering::greater >  0	);
static_assert( ! (weak_ordering::greater <= 0)	);
static_assert(    weak_ordering::greater >= 0	);
static_assert( ! (0 == weak_ordering::greater)	);
static_assert(    0 <  weak_ordering::greater	);
static_assert( ! (0 >  weak_ordering::greater)	);
static_assert(    0 <= weak_ordering::greater	);
static_assert( ! (0 >= weak_ordering::greater)	);
static_assert( (weak_ordering::greater <=> 0) == weak_ordering::greater );
static_assert( (0 <=> weak_ordering::greater) == weak_ordering::less );

// Conversions
using std::partial_ordering;
static_assert( partial_ordering(weak_ordering::less) == partial_ordering::less );
static_assert( partial_ordering(weak_ordering::equivalent) == partial_ordering::equivalent );
static_assert( partial_ordering(weak_ordering::greater) == partial_ordering::greater );
