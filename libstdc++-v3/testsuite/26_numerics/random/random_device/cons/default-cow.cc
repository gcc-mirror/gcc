// { dg-options "-D_GLIBCXX_USE_CXX11_ABI=0" }
// { dg-do run { target c++11 } }
// { dg-require-cstdint "" }
//
// Copyright (C) 2019-2024 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// C++11 26.5.6 class random_device [rand.device]

#include <random>
#include <testsuite_hooks.h>

void
test01()
{
  std::random_device x;
  auto n [[gnu::unused]] = x();
}

int main()
{
  test01();
}
