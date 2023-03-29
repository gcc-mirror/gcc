// { dg-do run { target c++11 } }
// { dg-require-atomic-builtins "" }

// std::abs from <cstdlib> is not freestanding.
// { dg-require-effective-target hosted }

// Copyright (C) 2012-2023 Free Software Foundation, Inc.
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

// libstdc++/51811
// pointer arithimetic vs. atomic<_Tp*> specialization
int main(void)
{
  using namespace std;

  typedef int 	value_type;
  const size_t n = 2;
  value_type value = 42;
  atomic<value_type*> p, p2, p3;

  // operator++
  {
    p = &value;
    p2 = p++;
    VERIFY (p != p2);
    
    value_type* vp(p);
    value_type* vp2(p2);
    ptrdiff_t dist = reinterpret_cast<char*>(vp) - reinterpret_cast<char*>(vp2);
    VERIFY ( std::abs(dist) == sizeof(value_type));
  
    p = &value;
    p3 = ++p;
    VERIFY (p == p3);
  }

  // operator--
  {
    p = &value;
    p2 = p--;
    VERIFY (p != p2);

    value_type* vp(p);
    value_type* vp2(p2);
    ptrdiff_t dist = reinterpret_cast<char*>(vp) - reinterpret_cast<char*>(vp2);
    VERIFY ( std::abs(dist) == sizeof(value_type));

    p = &value;
    p3 = --p;
    VERIFY (p == p3);
  }

  // operator+=
  {
    p = &value;
    value_type* vp(p);
    p+=n;
    value_type* vp2(p);
    ptrdiff_t dist = reinterpret_cast<char*>(vp) - reinterpret_cast<char*>(vp2);
    VERIFY ( std::abs(dist) == sizeof(value_type) * n);
  }

  // operator-=
  {
    p = &value;
    value_type* vp(p);
    p-=n;
    value_type* vp2(p);
    ptrdiff_t dist = reinterpret_cast<char*>(vp) - reinterpret_cast<char*>(vp2);
    VERIFY ( std::abs(dist) == sizeof(value_type) * n);
  }

  return 0;
}
