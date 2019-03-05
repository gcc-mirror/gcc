// { dg-do compile { target c++14 } }

// Copyright (C) 2014-2019 Free Software Foundation, Inc.
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

// NB: This file is for testing type_traits with NO OTHER INCLUDES.

#include <type_traits>

using namespace std;

static_assert (is_same<typename remove_cv<int>::type,
	               remove_cv_t<int>>(),
               "remove_cv_t" );

static_assert (is_same<typename remove_cv<const volatile long>::type,
                       remove_cv_t<const volatile long>>(),
	       "remove_cv_t" );
