// { dg-do run { target *-*-freebsd* *-*-netbsd* *-*-linux* *-*-solaris* *-*-cygwin *-*-darwin* alpha*-*-osf* mips-sgi-irix6* } }
// { dg-options " -std=gnu++0x -pthread" { target *-*-freebsd* *-*-netbsd* *-*-linux* alpha*-*-osf* mips-sgi-irix6* } }
// { dg-options " -std=gnu++0x -pthreads" { target *-*-solaris* } }
// { dg-options " -std=gnu++0x " { target *-*-cygwin *-*-darwin* } }
// { dg-require-cstdint "" }
// { dg-require-gthreads "" }

// Copyright (C) 2009, 2010, 2011 Free Software Foundation, Inc.
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
  bool test __attribute__((unused)) = true;

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
