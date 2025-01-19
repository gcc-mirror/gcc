// Copyright (C) 2014-2025 Free Software Foundation, Inc.
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

#include <tr1/functional>
#include <stdexcept>
#include <testsuite_hooks.h>

struct functor
{
  functor() : copies(0) { }

  functor(const functor& f)
  : copies(f.copies + 1)
  {
    if (copies > 1)
      throw std::runtime_error("functor");
  }

  void operator()() const { }

  int copies;
};


void
test01()
{
  std::tr1::function<void()> f = functor();
  try {
    std::tr1::function<void()> g = f;
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
