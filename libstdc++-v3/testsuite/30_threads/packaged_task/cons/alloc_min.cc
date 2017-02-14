// { dg-do compile { target c++11 } }
// { dg-require-cstdint "" }
// { dg-require-gthreads "" }

// Copyright (C) 2011-2017 Free Software Foundation, Inc.
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

// Test that packaged_task can use a minimal C++11 allocator
// and doesn't rely on C++98 allocator interface.

#include <future>
#include <testsuite_allocator.h>

using std::packaged_task;
using std::allocator_arg;

__gnu_test::SimpleAllocator<int> a;
packaged_task<int()> p(allocator_arg, a, []() { return 1; });
