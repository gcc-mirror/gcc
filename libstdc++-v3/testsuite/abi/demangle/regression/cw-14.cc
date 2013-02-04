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

// libcwd tests
int main()
{
  using namespace __gnu_test;

/*
struct G {
  int m(void) const { return 0; }
  int n(void) { return 0; }
};

struct H {
  int m(void) const { return 0; }
  int n(void) { return 0; }
};

template<typename T>
  class what
  {
  };

template<typename T>
  class what2
  {
  };

void r(int (G::*)(void),
       int (G::*)(void) const,
       G, // S_ 
       int (H::*)(void), // M1HS0_
       int (G::*)(void), // S1_
       what<G const>, // what<S2_>
  what2<G const>, // what2<S8_>
    int (G::*)(void) const // S3_ 
{
  G g;
  what<G const> y;
  what2<G const> y2;
  r(&G::n, &G::m, g, &H::n, &G::n, y, y2, &G::m);
}
*/

  // cplus-dem CORE
verify_demangle("_Z1rM1GFivEMS_KFivES_M1HFivES1_4whatIKS_E5what2IS8_ES3_",
		"r(int (G::*)(), int (G::*)() const, G, int (H::*)(), int (G::*)(), what<G const>, what2<G const>, int (G::*)() const)");

  return 0;
}
