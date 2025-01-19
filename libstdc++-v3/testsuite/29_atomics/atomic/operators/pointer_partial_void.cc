// { dg-do run { target { c++11_only || c++14_only } } }
// { dg-require-atomic-builtins "" }
// { dg-require-effective-target hosted }

// Copyright (C) 2012-2025 Free Software Foundation, Inc.
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

#include <atomic>
#include <cstdlib> //std::abs
#include <testsuite_hooks.h>

// pointer arithimetic vs. atomic<void*>.
// atomic<void*> vs. explicitly specialized w/o operators, like atomic_bool?
int main(void)
{
  using namespace std;

  typedef int 	value_type;
  const size_t n = 2;
  value_type value = 42;
  value_type* p = &value;
  void* vp = p;
  ptrdiff_t __attribute__((unused)) dist(0);

  atomic<void*> a(vp);

  // operator++
  void* vp2(a);
  a++;
  void* vp3(a);
  dist = reinterpret_cast<char*>(vp2) - reinterpret_cast<char*>(vp3);
  VERIFY ( std::abs(dist) == 1 );

  // operator--
  void* vp4(a);
  a--;
  void* vp5(a);
  dist = reinterpret_cast<char*>(vp4) - reinterpret_cast<char*>(vp5);
  VERIFY ( std::abs(dist) == 1 );

  // operator+=
  void* vp6(a);
  a+=n;
  void* vp7(a);
  dist = reinterpret_cast<char*>(vp6) - reinterpret_cast<char*>(vp7);
  VERIFY ( std::abs(dist) == n );

  // operator-=
  void* vp8(a);
  a-=n;
  void* vp9(a);
  dist = reinterpret_cast<char*>(vp8) - reinterpret_cast<char*>(vp9);
  VERIFY ( std::abs(dist) == n );

  return 0;
}
// { dg-warning "invalid application of 'sizeof' to a void type" "" { target *-*-* } 0 }
