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
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.

#include <algorithm>
#include <testsuite_hooks.h>

int a[10] = { 1, 2, 3, 3, 4, 5, 5, 6, 7, 9 };

static int compare_count = 0;

bool compare(int a, int b)
{
  compare_count++;
  return a == b;
}

// libstdc++/11480
void test01()
{
  bool test __attribute__((unused)) = true;

  std::unique(a, a+10, compare);
  VERIFY( compare_count == 9 );
}

int
main()
{
  test01();
  return 0;
}
