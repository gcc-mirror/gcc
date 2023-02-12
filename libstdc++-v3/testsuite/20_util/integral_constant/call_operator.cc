// { dg-do compile { target c++14 } }
//
// Copyright (C) 2013-2023 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include <type_traits>
#include <cassert> //testsuite_hooks.h>

typedef std::integral_constant<int, 1>       ic_one;
typedef std::integral_constant<int, 0>       ic_zero;
typedef std::integral_constant<int, -1>      ic_minus_one;

typedef std::integral_constant<bool, true>   ic_true;
typedef std::integral_constant<bool, false>  ic_false;

static_assert( ic_one{}() == 1, "1" );
static_assert( ic_zero{}() == 0, "0" );
static_assert( ic_minus_one{}() == -1, "-1" );

static_assert( ic_true{}() == true, "true" );
static_assert( ic_false{}() == false, "false" );
