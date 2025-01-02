// 2003-02-26 Benjamin Kosnik <bkoz@redhat.com>

// Copyright (C) 2003-2025 Free Software Foundation, Inc.
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

// libstdc++/7986
int main()
{
  using namespace __gnu_test;

  // ICE on figmative float template parameters?
  /*
  demangle("_Z3absILd1c1f1496f8a44219EEvv", "void abs<3.14159e-173>()");
  demangle("_Z3absILd40092acd9e83e426EEvv", "void abs<3.1459>()");
  demangl("_Z3absILe08042191a6cc56a2fe117becEEvv", "void abs<1.234e-2345l>()");
  demangle("_Z3absILf4016147bEEvv", "void abs<2.345f>()");
  demangle("_Z3absILfc1800000EEvv", " void abs<-16f>()");
  demangle("_Z3absILe804bfff8000000000000000EEvv", "void abs<-1l>()");
  */

  // template<int D> void abs(void) { };
  // template void abs<11>(void);
  // Equivalent, but formatting difference in void argument.
  // verify_demangle("_Z3absILi11EEvv", "void abs<(int)11>()");
  verify_demangle("_Z3absILi11EEvv", "void abs<11>()");

  return 0;
}
