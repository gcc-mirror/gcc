// 2001-12-27 pme
//
// Copyright (C) 2001 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.

// 23.2.1.1 deque constructors, copy, and assignment

#include <deque>
#include <testsuite_hooks.h>

typedef std::deque<gnu_counting_struct>   gdeque;


// basic alloc/dealloc sanity check
void
test01()
{
  assert_count (0);
  {
     gdeque   d(10);
     assert_count (10);
  }
  assert_count (0);
}

int main()
{
  test01();

  return 0;
}
