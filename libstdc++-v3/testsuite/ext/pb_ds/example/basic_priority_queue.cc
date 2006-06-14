// -*- C++ -*-

// Copyright (C) 2005, 2006 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the terms
// of the GNU General Public License as published by the Free Software
// Foundation; either version 2, or (at your option) any later
// version.

// This library is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this library; see the file COPYING.  If not, write to
// the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
// MA 02111-1307, USA.

// As a special exception, you may use this file as part of a free
// software library without restriction.  Specifically, if other files
// instantiate templates or use macros or inline functions from this
// file, or you compile this file and link it with other files to
// produce an executable, this file does not by itself cause the
// resulting executable to be covered by the GNU General Public
// License.  This exception does not however invalidate any other
// reasons why the executable file might be covered by the GNU General
// Public License.

// Copyright (C) 2004 Ami Tavory and Vladimir Dreizin, IBM-HRL.

// Permission to use, copy, modify, sell, and distribute this software
// is hereby granted without fee, provided that the above copyright
// notice appears in all copies, and that both that copyright notice
// and this permission notice appear in supporting documentation. None
// of the above authors, nor IBM Haifa Research Laboratories, make any
// representation about the suitability of this software for any
// purpose. It is provided "as is" without express or implied
// warranty.

/**
 * @file basic_priority_queue_example.cpp
 * A basic example showing how to use priority queues.
 */

/**
 * This example shows how to use priority queues. It defines a
 * function performing a sequence of operations on
 * a generic container. It then calls this function with some containers.
 */

#include <cassert>
#include <iostream>
#include <ext/pb_ds/priority_queue.hpp>

using namespace std;
using namespace pb_ds;
using namespace pb_ds;

// The following function performs a sequence of operations on a
// priority queue object storing integers.
template<class Cntnr>
void
some_op_sequence(Cntnr& r_c)
{
  assert(r_c.empty());
  assert(r_c.size() == 0);

  for (size_t i = 0; i < 10; ++i)
    r_c.push(i);
  cout << endl << "All values in the container:" << endl;

  typedef typename Cntnr::const_iterator const_iterator;
  for (const_iterator it = r_c.begin(); it != r_c.end();  ++it)
    cout <<* it << endl;
  assert(!r_c.empty());
  assert(r_c.size() == 10);

  cout << "Popping all values: " << endl;
  while (!r_c.empty())
    {
      cout << r_c.top() << endl;
      r_c.pop();
    }
  assert(r_c.empty());
  assert(r_c.size() == 0);

  cout << endl;
}

int main()
{
  {
    // Perform operations on a pairing-heap queue.
    cout << "Pairing heap" << endl;
    pb_ds::priority_queue<int, less<int>, pairing_heap_tag> c;
    some_op_sequence(c);
  }

  {
    // Perform operations on a binomial-heap queue.
    cout << "Binomial heap" << endl;
    pb_ds::priority_queue<int, less<int>, binomial_heap_tag> c;
    some_op_sequence(c);
  }

  {
    // Perform operations on a binomial-heap queue.
    cout << "Redundant-counter binomial heap" << endl;
    pb_ds::priority_queue<int, less<int>, rc_binomial_heap_tag> c;
    some_op_sequence(c);
  }

  {
    // Perform operations on a binomial-heap queue.
    cout << "Binary heap" << endl;
    pb_ds::priority_queue<int, less<int>, binary_heap_tag> c;
    some_op_sequence(c);
  }

  {
    // Perform operations on a thin-heap queue.
    cout << "Thin heap" << endl;
    pb_ds::priority_queue<int, less<int>, thin_heap_tag> c;
    some_op_sequence(c);
  }

  return 0;
}
