// { dg-do run { target c++11 } }
// { dg-options "-g -O0" }

// Copyright (C) 2014-2018 Free Software Foundation, Inc.
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

#include <forward_list>

int
main ()
{
  std::forward_list<int> l0, l1;

  l1.push_front (0);
  l1.push_front (1);
  l1.push_front (2);
  l1.push_front (11011);

// { dg-final { note-test l0.empty() true } }
// { dg-final { note-test l1.empty() false } }
// { dg-final { note-test l1.front() 11011 } }

// { dg-final { whatis-test l1.empty() bool } }
// { dg-final { whatis-test l1.front() int } }

  return 0;  // Mark SPOT
}

// { dg-final { gdb-test SPOT {} 1 } }
