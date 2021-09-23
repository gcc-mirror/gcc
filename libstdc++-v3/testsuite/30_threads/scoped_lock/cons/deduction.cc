// Copyright (C) 2017-2021 Free Software Foundation, Inc.
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

// { dg-do compile { target c++17 } }

#include <mutex>

template<typename T, typename U> struct require_same;
template<typename T> struct require_same<T, T> { using type = void; };

template<typename T, typename U>
  typename require_same<T, U>::type
  check_type(U&) { }

void
test01()
{
  std::scoped_lock l0;
  check_type<std::scoped_lock<>>(l0);

  struct BasicLockable {
    void lock() { }
    void unlock() { }
  } m1;

  std::scoped_lock l1(m1);
  check_type<std::scoped_lock<BasicLockable>>(l1);

  struct Lockable {
    void lock() { }
    void unlock() { }
    bool try_lock() { return true; }
  } m2;

  std::mutex m3;
  std::scoped_lock l2(m2, m3);
  check_type<std::scoped_lock<Lockable, std::mutex>>(l2);
}

void
test02()
{
  std::scoped_lock l0(std::adopt_lock);
  check_type<std::scoped_lock<>>(l0);

  struct BasicLockable {
    void lock() { }
    void unlock() { }
  } m1;

  std::scoped_lock l1(std::adopt_lock, m1);
  check_type<std::scoped_lock<BasicLockable>>(l1);

  struct Lockable {
    void lock() { }
    void unlock() { }
    bool try_lock() { return true; }
  } m2;

  std::mutex m3;
  std::scoped_lock l2(std::adopt_lock, m2, m3);
  check_type<std::scoped_lock<Lockable, std::mutex>>(l2);
}
