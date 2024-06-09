// { dg-do compile { target c++17 } }
// { dg-require-gthreads "" }
// { dg-add-options no_pch }

// Copyright (C) 2017-2024 Free Software Foundation, Inc.
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

// NB: This file is for testing with NO OTHER INCLUDES.

#include <mutex>

#ifndef __cpp_lib_scoped_lock
# error "Feature-test macro for scoped_lock missing"
#elif __cpp_lib_scoped_lock != 201703
# error "Feature-test macro for scoped_lock has wrong value"
#endif

void test01()
{
  // Check for required typedefs
  typedef std::scoped_lock<std::mutex> test_type;
  typedef test_type::mutex_type mutex_type;
}
