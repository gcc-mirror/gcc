// Copyright (C) 2003-2014 Free Software Foundation, Inc.
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

// 27.4.3  streamoff

#include <ios>
#include <limits>
#include <testsuite_hooks.h>

void check(std::streamsize n)
{
  bool test __attribute__((unused)) = true;

  VERIFY( std::streamsize(std::streamoff(n)) == n );
}

void test03()
{
  check(0);
  check(-1);
  check(1);
  check(std::numeric_limits<std::streamsize>::min());
  check(std::numeric_limits<std::streamsize>::min() + 1);
  check(std::numeric_limits<std::streamsize>::min() / 2);
  check(std::numeric_limits<std::streamsize>::max());
  check(std::numeric_limits<std::streamsize>::max() - 1);
  check(std::numeric_limits<std::streamsize>::max() / 2);
}

int main()
{
  test03();
  return 0;
}
