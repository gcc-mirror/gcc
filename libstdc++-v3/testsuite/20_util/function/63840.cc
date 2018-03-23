// Copyright (C) 2014-2018 Free Software Foundation, Inc.
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

#include <functional>
#include <stdexcept>
#include <testsuite_hooks.h>

struct functor
{
  functor() = default;

  functor(const functor&)
  {
    throw std::runtime_error("test");
  }

  functor(functor&& f) = default;

  void operator()() const { }
};


void
test01()
{
  std::function<void()> f = functor{};
  try {
    auto g = f;
  } catch (const std::runtime_error& e) {
    return;
  }
  VERIFY(false);
}

int
main()
{
  test01();
}
