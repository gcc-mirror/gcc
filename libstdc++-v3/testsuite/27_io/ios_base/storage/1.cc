// 2000-12-19 bkoz

// Copyright (C) 2000, 2002, 2003, 2004 Free Software Foundation
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
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.

// 27.4.2.5 ios_base storage functions

// XXX This test will not work for some versions of irix6 because of
// XXX bug(s) in libc malloc for very large allocations.  However
// XXX -lmalloc seems to work.
// See http://gcc.gnu.org/ml/gcc/2002-05/msg01012.html
// { dg-options "-lmalloc" { target mips*-*-irix6* } }

#include <sstream>
#include <iostream>
#include <testsuite_hooks.h>

// http://gcc.gnu.org/ml/gcc-bugs/2000-12/msg00413.html
void test01() 
{
  bool test __attribute__((unused)) = true;
  
  using namespace std;

  ios::xalloc();
  ios::xalloc();
  ios::xalloc();
  long x4 = ios::xalloc();

  ostringstream out("the element of crime, lars von trier");
  out.pword(++x4); // should not crash
}

int main(void)
{
  __gnu_test::set_memory_limits();
  test01();
  return 0;
}
