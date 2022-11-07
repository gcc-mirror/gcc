// { dg-do compile { target c++11 } }
// { dg-require-effective-target hosted }

// Copyright (C) 2011-2022 Free Software Foundation, Inc.
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

// libstdc++/60594

#include <functional>
#include <type_traits>
struct bar;
using F = std::function<bar()>;
// check for copy constructible and assignable while 'bar' is incomplete
constexpr int c = std::is_copy_constructible<F>::value;
constexpr int a = std::is_copy_assignable<F>::value;
struct bar { };
bar func();
void test()
{
  F g{ &func };
  g = func;
}
