// Copyright (C) 2020-2024 Free Software Foundation, Inc.
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

// { dg-do compile { target c++20 } }

#include <compare>

using std::strong_ordering;

static_assert( strong_ordering::less == strong_ordering::less );
static_assert( strong_ordering::less != strong_ordering::equal );
static_assert( strong_ordering::less != strong_ordering::equivalent );
static_assert( strong_ordering::less != strong_ordering::greater );
static_assert( strong_ordering::equivalent == strong_ordering::equivalent );
static_assert( strong_ordering::equivalent == strong_ordering::equal );
static_assert( strong_ordering::equivalent != strong_ordering::greater );
static_assert( strong_ordering::equal == strong_ordering::equal );
static_assert( strong_ordering::equal != strong_ordering::greater );
static_assert( strong_ordering::greater == strong_ordering::greater );

static_assert( ! (strong_ordering::less == 0)	);
static_assert(    strong_ordering::less <  0	);
static_assert( ! (strong_ordering::less >  0)	);
static_assert(    strong_ordering::less <= 0	);
static_assert( ! (strong_ordering::less >= 0)	);
static_assert( ! (0 == strong_ordering::less)	);
static_assert( ! (0 <  strong_ordering::less)	);
static_assert(    0 >  strong_ordering::less	);
static_assert( ! (0 <= strong_ordering::less)	);
static_assert(    0 >= strong_ordering::less	);
static_assert( (strong_ordering::less <=> 0) == strong_ordering::less );
static_assert( (0 <=> strong_ordering::less) == strong_ordering::greater );

static_assert(   (strong_ordering::equal == 0)	);
static_assert( ! (strong_ordering::equal <  0)	);
static_assert( ! (strong_ordering::equal >  0)	);
static_assert(    strong_ordering::equal <= 0	);
static_assert(    strong_ordering::equal >= 0	);
static_assert(    0 == strong_ordering::equal	);
static_assert( ! (0 <  strong_ordering::equal)	);
static_assert( ! (0 >  strong_ordering::equal)	);
static_assert(    0 <= strong_ordering::equal	);
static_assert(    0 >= strong_ordering::equal	);
static_assert( (strong_ordering::equal <=> 0) == strong_ordering::equal );
static_assert( (0 <=> strong_ordering::equal) == strong_ordering::equal );

static_assert(   (strong_ordering::equivalent == 0)	);
static_assert( ! (strong_ordering::equivalent <  0)	);
static_assert( ! (strong_ordering::equivalent >  0)	);
static_assert(    strong_ordering::equivalent <= 0	);
static_assert(    strong_ordering::equivalent >= 0	);
static_assert(    0 == strong_ordering::equivalent	);
static_assert( ! (0 <  strong_ordering::equivalent)	);
static_assert( ! (0 >  strong_ordering::equivalent)	);
static_assert(    0 <= strong_ordering::equivalent	);
static_assert(    0 >= strong_ordering::equivalent	);
static_assert( (strong_ordering::equivalent <=> 0) == strong_ordering::equivalent );
static_assert( (0 <=> strong_ordering::equivalent) == strong_ordering::equivalent );

static_assert( ! (strong_ordering::greater == 0)	);
static_assert( ! (strong_ordering::greater <  0)	);
static_assert(    strong_ordering::greater >  0	);
static_assert( ! (strong_ordering::greater <= 0)	);
static_assert(    strong_ordering::greater >= 0	);
static_assert( ! (0 == strong_ordering::greater)	);
static_assert(    0 <  strong_ordering::greater	);
static_assert( ! (0 >  strong_ordering::greater)	);
static_assert(    0 <= strong_ordering::greater	);
static_assert( ! (0 >= strong_ordering::greater)	);
static_assert( (strong_ordering::greater <=> 0) == strong_ordering::greater );
static_assert( (0 <=> strong_ordering::greater) == strong_ordering::less );

// Conversions
using std::partial_ordering;
static_assert( partial_ordering(strong_ordering::less) == partial_ordering::less );
static_assert( partial_ordering(strong_ordering::equal) == partial_ordering::equivalent );
static_assert( partial_ordering(strong_ordering::equivalent) == partial_ordering::equivalent );
static_assert( partial_ordering(strong_ordering::greater) == partial_ordering::greater );
using std::weak_ordering;
static_assert( weak_ordering(strong_ordering::less) == weak_ordering::less );
static_assert( partial_ordering(strong_ordering::equal) == weak_ordering::equivalent );
static_assert( partial_ordering(strong_ordering::equivalent) == weak_ordering::equivalent );
static_assert( weak_ordering(strong_ordering::greater) == weak_ordering::greater );
