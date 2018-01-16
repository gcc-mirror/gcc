// Copyright (C) 2016-2018 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++17" }
// { dg-do compile }

#include <atomic>

struct S { int s[64]; };

constexpr bool b1 = std::atomic<S>::is_always_lock_free;
constexpr const bool* cktype1 = &std::atomic<S>::is_always_lock_free;

constexpr bool b2 = std::atomic<int*>::is_always_lock_free;
constexpr const bool* cktype2 = &std::atomic<int*>::is_always_lock_free;

static_assert( std::atomic<int*>::is_always_lock_free
                == (ATOMIC_POINTER_LOCK_FREE == 2) );
