// { dg-do run }
// { dg-options "-g -O0" }

// Copyright (C) 2014-2023 Free Software Foundation, Inc.
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

#include <deque>

const int max_deque_node_size = 512;

int
main ()
{
  std::deque<int> q0, q1, q2, q3;
  int int_size = sizeof (int);

  // The xmethod logic is exercised differently for deques of different size.
  // Let q1 be a deque requiring only 1 node. Let q2 be a deque filling up
  // exactly 2 nodes. Let q3 be of size which would require 1 node and part
  // of the second node.
  int q1_size = max_deque_node_size / int_size / 2;
  int q2_size = max_deque_node_size / int_size * 2;
  int q3_size = max_deque_node_size / int_size * 3 / 2;

  for (int i = 0; i < q1_size; i++)
    q1.push_back (100 + i);

  for (int i = 0; i < q2_size; i++)
    q2.push_back (200 + i);

  for  (int i = 0; i < q3_size; i++)
    q3.push_back (300 + i);

// { dg-final { note-test q0.empty() true } }
// { dg-final { note-test q1.empty() false } }
// { dg-final { note-test q0.size() 0 } }
// { dg-final { note-test q1.size()==q1_size true } }
// { dg-final { note-test q2.size()==q2_size true } }
// { dg-final { note-test q3.size()==q3_size true } }
// { dg-final { note-test q1.front() 100 } }
// { dg-final { note-test q2.front() 200 } }
// { dg-final { note-test q3.front() 300 } }
// { dg-final { note-test q1.back()==(100+q1_size-1) true } }
// { dg-final { note-test q2.back()==(200+q2_size-1) true } }
// { dg-final { note-test q3.back()==(300+q3_size-1) true } }
// { dg-final { note-test q3\[0\] 300 } }
// { dg-final { note-test q3\[q3_size/2\]==(300+q3_size/2) true } }
// { dg-final { note-test q3\[q3_size-1]==(300+q3_size-1) true } }

// { dg-final { whatis-test q0.empty() bool } }
// { dg-final { whatis-test q0.size() std::size_t } }
// { dg-final { whatis-test q1.front() int } }
// { dg-final { whatis-test q1.back() int } }
// { dg-final { whatis-test q3\[0\] int } }

  return 0;  // Mark SPOT
}

// { dg-final { gdb-test SPOT {} 1 } }
