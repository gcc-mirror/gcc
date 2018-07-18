// Copyright (C) 2017-2018 Free Software Foundation, Inc.
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

// { dg-require-fileio "" }

// PR libstdc++/53984

#include <fstream>
#include <testsuite_hooks.h>

void
test01()
{
  std::ifstream in(".");
  if (in)
  {
    char c;
    if (in.get(c))
    {
      // Reading a directory doesn't produce an error on this target
      // so the formatted input functions below wouldn't fail anyway
      // (see PR libstdc++/81808).
      return;
    }
    int x;
    in.clear();
    // Formatted input function should set badbit, but not throw:
    in >> x;
    VERIFY( in.bad() );

    in.clear();
    in.exceptions(std::ios::badbit);
    try
    {
      // Formatted input function should set badbit, and throw:
      in >> x;
      VERIFY( false );
    }
    catch (const std::exception&)
    {
      VERIFY( in.bad() );
    }
  }
}

int
main()
{
  test01();
}
