// { dg-do run { target c++11 } }

// Copyright (C) 2010-2023 Free Software Foundation, Inc.
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

#include <exception>
#include <testsuite_hooks.h>

// https://gcc.gnu.org/ml/libstdc++/2016-10/msg00139.html

struct E {
  void* operator new(std::size_t) = delete;
};

void test01()
{
  E e;
  std::exception_ptr p = std::make_exception_ptr(e);

  VERIFY( p );
}

int main()
{
  test01();

  return 0;
}
