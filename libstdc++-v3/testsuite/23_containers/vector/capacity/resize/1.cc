// 1999-05-07
// bkoz 

// Copyright (C) 1999, 2002, 2004, 2005, 2009 Free Software Foundation, Inc.
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

// 23.2.4.2 vector capacity

// XXX This test will not work for irix6 because of bug(s) in libc malloc
// XXX for very large allocations.  However -lmalloc seems to work.
// See http://gcc.gnu.org/ml/libstdc++/2002-12/msg00131.html
// { dg-options "-lmalloc" { target mips*-*-irix6* } }

// This fails on some versions of Darwin 8 because malloc doesn't return
// NULL even if an allocation fails (filed as Radar 3884894).
// { dg-do run { xfail *-*-darwin8.[0-4].* } }

#include <vector>
#include <stdexcept>
#include <testsuite_hooks.h>

void test01()
{
  bool test __attribute__((unused)) = true;
  std::vector<int> v;
  try
    {
      v.resize(v.max_size());  
      v[v.max_size() - 1] = 2002;
    }
  catch (const std::bad_alloc& error)
    {
      test = true;
    }
  catch (...)
    {
      test = false;
    }
  VERIFY( test );
}

int main()
{
  test01();
  return 0;
}
