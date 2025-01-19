// { dg-do run { target c++11 } }
// { dg-require-effective-target hosted }

// Copyright (C) 2009-2025 Free Software Foundation, Inc.
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

#include <functional>

struct moveable
{
  moveable() = default;
  ~moveable() = default;
  moveable(const moveable& c) = delete;
  moveable& operator=(const moveable&) = delete;
  moveable(moveable&&) { }
};

void f1(moveable) { }
void f2(moveable&&) { }
struct { void operator()(moveable&&) { } } f3;

void test01()
{
  std::function<void (moveable)> fo1a(f1);
  fo1a(moveable());

  std::function<void (moveable)> fo2a(f2);
  fo2a(moveable());

  std::function<void (moveable)> fo3a(f3);
  fo3a(moveable());

  std::function<void (moveable&&)> fo1b(f1);
  fo1b(moveable());

  std::function<void (moveable&&)> fo2b(f2);
  fo2b(moveable());

  std::function<void (moveable&&)> fo3b(f3);
  fo3b(moveable());
}

int main()
{
  test01();

  return 0;
}
