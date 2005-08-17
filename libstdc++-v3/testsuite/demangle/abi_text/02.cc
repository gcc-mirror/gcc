// 2003-02-26 Benjamin Kosnik <bkoz@redhat.com>

// Copyright (C) 2003 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// IA 64 C++ ABI - 5.1 External Names (a.k.a. Mangling)

#include <testsuite_hooks.h>

// Specific examples mentioned in the IA64 C++ ABI text
// http://www.codesourcery.com/cxx-abi/abi.html#mangling
int main()
{
  using namespace __gnu_test;

  // template<int I> void foo (int (&)[I + 1]) { };
  // template void foo<2> (int (&)[3]);    
  // Equivalent, but formatting difference in void argument and parentheses.
  // icc, __cxa_demangle
  verify_demangle("_Z3fooILi2EEvRAplT_Li1E_i", 
		  "void foo<2>(int (&) [(2) + (1)])");
  // cplus-dem
  // verify_demangle("_Z3fooILi2EEvRAplT_Li1E_i", 
  //      "void foo<(int)2>(int (&) [((int)2)+((int)1)])");

  return 0;
}
