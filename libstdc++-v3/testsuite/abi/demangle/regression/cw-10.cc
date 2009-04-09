// 2003-02-26  Carlo Wood  <carlo@alinoe.com>

// Copyright (C) 2003, 2009 Free Software Foundation, Inc.
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

namespace std {
  template<typename T1, typename T2, typename T3>
    class D { };
  D<A*, A*&, A**> d;
  template<typename T1, typename T2, typename T3>
    D<T3, T3&, T3*> B(T1, T1, T2, T3*) { return d; }
}

void o(void)
{
  std::D<A*, A* const&, A* const*> dummy1;
  std::D<A*, A*&, A**> dummy2;
  A* dummy3;
  std::B(dummy1, dummy1, dummy2, &dummy3);
}
*/

    verify_demangle("_ZSt1BISt1DIP1ARKS2_PS3_ES0_IS2_RS2_PS2_ES2_ET0_T_SB_SA_PT1_", "std::D<A*, A*&, A**> std::B<std::D<A*, A* const&, A* const*>, std::D<A*, A*&, A**>, A*>(std::D<A*, A* const&, A* const*>, std::D<A*, A* const&, A* const*>, std::D<A*, A*&, A**>, A**)");
    return 0;
}
