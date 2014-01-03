// { dg-do run { target *-*-freebsd* *-*-netbsd* *-*-linux* *-*-gnu* *-*-solaris* *-*-cygwin *-*-darwin* powerpc-ibm-aix* } }
// { dg-options " -std=gnu++0x -pthread" { target *-*-freebsd* *-*-netbsd* *-*-linux* *-*-gnu* powerpc-ibm-aix* } }
// { dg-options " -std=gnu++0x -pthreads" { target *-*-solaris* } }
// { dg-options " -std=gnu++0x " { target *-*-cygwin *-*-darwin* } }
// { dg-require-cstdint "" }
// { dg-require-gthreads "" }
// { dg-require-atomic-builtins "" }

// Copyright (C) 2011-2014 Free Software Foundation, Inc.
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
  moveable(const moveable&) = delete;
};

using std::launch;
namespace ph = std::placeholders;

typedef decltype(ph::_1) placeholder_type;

bool f(moveable, placeholder_type) { return true; }

void test01()
{
  auto fut = std::async(launch::async, f, moveable(), ph::_1);
  bool test = fut.get();
  VERIFY( test );
}

void test02()
{
  auto fut = std::async(launch::deferred, f, moveable(), ph::_1);
  bool test = fut.get();
  VERIFY( test );
}

int main()
{
  test01();
  test02();
}
