// Copyright (C) 2013-2018 Free Software Foundation, Inc.
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

// 20.7.12 specialized algorithms

// { dg-do compile { target c++11 } }

#include <memory>

// libstdc++/58982

// trivial class that is not assignable
struct T
{
  T() = default;
  ~T() = default;

  T& operator=(const T&) = delete;
};

void
test01(T* result)
{
  T t[1];
  std::uninitialized_copy_n(t, 1, result);
}
