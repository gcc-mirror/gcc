// { dg-do run }
// { dg-additional-options "-pthread" { target pthread } }
// { dg-require-effective-target c++11 }
// { dg-require-gthreads "" }

// Copyright (C) 2009-2021 Free Software Foundation, Inc.
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


#include <thread>
#include <utility>
#include <testsuite_hooks.h>

struct moveable
{
  moveable() = default;
  ~moveable() = default;
  moveable(const moveable& c) = delete;
  moveable& operator=(const moveable&) = delete;
  moveable(moveable&&) { }

  void operator()() const { }
};


void test01()
{
  moveable m;
  std::thread b(std::move(m));
  std::thread::id id_initial = b.get_id();
  VERIFY( b.joinable() );
  VERIFY( id_initial != std::thread::id() );

  // copy move construct
  // copied new thread old id, original thread default id
  std::thread c(std::move(b));
  VERIFY( c.joinable() );
  VERIFY( c.get_id() == id_initial );
  VERIFY( !b.joinable() );
  VERIFY( b.get_id() == std::thread::id() );

  // copy move assign
  std::thread d;
  VERIFY( !d.joinable() );
  VERIFY( d.get_id() == std::thread::id() );
  d = std::move(c);
  VERIFY( d.joinable() );
  VERIFY( d.get_id() == id_initial );
  VERIFY( !c.joinable() );
  VERIFY( c.get_id() == std::thread::id() );
  
  d.join();
}

int main(void)
{
  test01();
  return 0;
}
