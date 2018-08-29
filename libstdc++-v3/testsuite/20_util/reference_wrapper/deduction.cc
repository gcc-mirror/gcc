// Copyright (C) 2018 Free Software Foundation, Inc.
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
// { dg-do compile { target c++17 } }

#include <functional>

void
test01()
{
  int i = 0;
  std::reference_wrapper rw0(i);
  [[maybe_unused]] std::reference_wrapper<int>* p0 = &rw0;
  [[maybe_unused]] int& r0 = rw0;

  std::reference_wrapper rw1(rw0);
  [[maybe_unused]] std::reference_wrapper<int>* p1 = &rw1;
  [[maybe_unused]] int& r1 = rw1;
}

void
test02()
{
  const int i = 0;
  std::reference_wrapper rw0(i);
  [[maybe_unused]] std::reference_wrapper<const int>* p0 = &rw0;

  std::reference_wrapper rw1(rw0);
  [[maybe_unused]] std::reference_wrapper<const int>* p1 = &rw1;
}
