// 2003-02-26  Carlo Wood  <carlo@alinoe.com>

// Copyright (C) 2003-2013 Free Software Foundation, Inc.
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

// c++/3111
int main()
{
  using namespace __gnu_test;

  // cplus-dem FAIL
  // icc FAIL
  // verify_demangle("_Z1fAszL_ZZNK1N1A1fEvE3foo_0E_i", 
  //       "f(int[sizeof(N::A::f() const::foo())])");
  verify_demangle("_Z1fAszL_ZZNK1N1A1fEvE3foo_0E_i", 
		  "f(int [sizeof (N::A::f() const::foo)])");

  return 0;
}
