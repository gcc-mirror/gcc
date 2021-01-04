// Copyright (C) 2020-2021 Free Software Foundation, Inc.
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

// { dg-do run { target c++11 } }
// { dg-additional-options "-pthread" { target pthread } }

// Currently std::call_once is broken for gthreads targets without futexes:
// { dg-skip-if "see PR 66146" { gthreads && { ! futex } } }

#include <mutex>
#include <cstdlib>
#include <testsuite_hooks.h>

void
test01()
{
  std::once_flag once;
  int counter = 0;
  for (int i = 0; i < 10; ++i)
  {
    try
    {
      std::call_once(once, [&]{ if (i < 3) throw i; ++counter; });
      VERIFY(i >= 3);
    }
    catch (int ex)
    {
      VERIFY(i < 3);
    }
  }
 VERIFY(counter == 1);
 std::call_once(once, []{ std::abort(); });
}

int
main()
{
  test01();
}
