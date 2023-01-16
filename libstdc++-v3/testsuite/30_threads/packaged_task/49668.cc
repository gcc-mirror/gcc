// { dg-do run }
// { dg-additional-options "-pthread" { target pthread } }
// { dg-require-effective-target c++11 }
// { dg-require-gthreads "" }

// Copyright (C) 2011-2023 Free Software Foundation, Inc.
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


#include <future>
#include <functional>
#include <testsuite_hooks.h>

struct moveable
{
  moveable() = default;
  moveable(moveable&&) = default;
};

typedef decltype(std::placeholders::_1) placeholder_type;

bool f(moveable, placeholder_type) { return true; }

void test01()
{
  std::packaged_task<bool(moveable, placeholder_type)> p(f);
  p(moveable(), std::placeholders::_1);
  bool test = p.get_future().get();
  VERIFY( test );
}

int main()
{
  test01();
}
