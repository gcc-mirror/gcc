// { dg-do compile { target c++17 } }
// { dg-add-options no_pch }

// Copyright (C) 2017-2025 Free Software Foundation, Inc.
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

struct BasicLockable
{
  BasicLockable() = default;
  ~BasicLockable() = default;
  void lock() { }
  void unlock() { }
};

void test01()
{
  // Check for required typedef.
  using test_type = std::scoped_lock<BasicLockable>;
  static_assert(std::is_same_v<test_type::mutex_type, BasicLockable>);
}

template<typename T, typename = void>
constexpr bool has_mutex_type = false;

template<typename T>
constexpr bool has_mutex_type<T, std::void_t<typename T::mutex_type>> = true;

void test02()
{
  // Check that typedef is absent as required.
  using test_type = std::scoped_lock<BasicLockable, BasicLockable>;
  static_assert(!has_mutex_type<test_type>);
}
