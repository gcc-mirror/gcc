// { dg-do compile { target c++11 } }
// { dg-require-gthreads "" }

// Copyright (C) 2011-2018 Free Software Foundation, Inc.
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

// Test that packaged_task can be default-constructed or move-constructed
// in a context that does uses-allocator construction.

#include <future>
#include <memory>
#include <tuple>

using std::packaged_task;
using std::allocator_arg;
using std::allocator;
using std::tuple;

typedef packaged_task<void()> task;
allocator<task> a;

tuple<task> t1{ allocator_arg, a };
tuple<task> t2{ allocator_arg, a, task{} };
