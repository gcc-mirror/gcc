// Copyright (C) 2020-2026 Free Software Foundation, Inc.
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
// { dg-require-effective-target gthreads { target { ! *-*-linux* } } }
// { dg-require-effective-target hosted }
// { dg-add-options no_pch }

#include <semaphore>

#ifndef __cpp_lib_semaphore
# error "Feature-test macro for semaphore missing in <semaphore>"
#elif __cpp_lib_semaphore != 201907L
# error "Feature-test macro for semaphore has wrong value in <semaphore>"
#endif

static_assert(std::is_same_v<std::counting_semaphore<1>,
			     std::binary_semaphore>);

static_assert(! std::is_same_v<std::counting_semaphore<2>,
			       std::binary_semaphore>);

static_assert(! std::is_same_v<std::counting_semaphore<>,
			       std::binary_semaphore>);

// The standard permits max() to be greater than the template argument,
// but for the current libstdc++ implementation it's always equal to it.
static_assert(std::binary_semaphore::max() == 1);
static_assert(std::counting_semaphore<0>::max() == 0);
static_assert(std::counting_semaphore<2>::max() == 2);

#include <limits.h>

static_assert(std::counting_semaphore<INT_MAX>::max() == INT_MAX);
static_assert(std::counting_semaphore<INT_MAX-1>::max() == INT_MAX-1);
static_assert(std::counting_semaphore<PTRDIFF_MAX>::max() == PTRDIFF_MAX);
static_assert(std::counting_semaphore<PTRDIFF_MAX-3>::max() == PTRDIFF_MAX-3);
