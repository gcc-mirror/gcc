// Copyright (C) 2017-2019 Free Software Foundation, Inc.
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

// { dg-do run { target c++11 } }

#include <memory>
#include <testsuite_hooks.h>

struct Del {
  template<typename T> void operator()(T* p) const noexcept { delete p; }
};

Del* (*f1)(const std::shared_ptr<int>&) = std::get_deleter<Del, int>;

void
test01()
{
  std::shared_ptr<int> p;
  VERIFY( std::get_deleter<Del>(p) == nullptr );
  p = std::shared_ptr<int>(new int, Del());
  VERIFY( std::get_deleter<Del>(p) != nullptr );
  p = std::shared_ptr<int>(new int);
  VERIFY( std::get_deleter<Del>(p) == nullptr );
}

int
main()
{
  test01();
}
