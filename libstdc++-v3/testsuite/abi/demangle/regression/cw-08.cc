// 2003-02-26  Carlo Wood  <carlo@alinoe.com>

// Copyright (C) 2003-2023 Free Software Foundation, Inc.
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

// IA 64 C++ ABI - 5.1 External Names (a.k.a. Mangling)

#include <testsuite_hooks.h>

// libcwd tests
int main()
{
  using namespace __gnu_test;

/*
class A { };
typedef A const* a4_t[4];
typedef a4_t* ap4_t;
class C {
public:
  ap4_t c;
};
template<typename T1, typename T2, typename T3>
  void f(T1, T2, T3, ap4_t, ap4_t (C::*)) { }

void m(void)
{
  // Instantiation.
  A a;
  a4_t a4;
  f(a, &a, static_cast<A const*>(&a), &a4, &C::c);
}
*/
  // Equivalent, spacing differences with icc.
  // cplus-dem FAIL
  verify_demangle("_Z1fI1APS0_PKS0_EvT_T0_T1_PA4_S3_M1CS8_", "void f<A, A*, A const*>(A, A*, A const*, A const* (*) [4], A const* (* C::*) [4])");

  return 0;
}
