// -*- C++ -*-

// Copyright (C) 2005-2019 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the terms
// of the GNU General Public License as published by the Free Software
// Foundation; either version 3, or (at your option) any later
// version.

// This library is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.


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
 * This example shows how to use priority queues. It defines a
 *    function performing a sequence of operations on
 *    a generic container. It then calls this function with some containers.
 */

#include <ext/pb_ds/priority_queue.hpp>
#include <iostream>
#include <cassert>

using namespace std;
using namespace __gnu_pbds;
using namespace __gnu_pbds;

template<typename Cntnr>
void
some_op_sequence(Cntnr& r_c)
{
  assert(r_c.empty());
  assert(r_c.size() == 0);

  for (size_t i = 0; i < 10; ++i)
    r_c.push(i);

  cout << endl << "All values in the container:" << endl;
  for (typename Cntnr::const_iterator it = r_c.begin(); it != r_c.end();
       ++it)
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

void
priority_queue_link_regression_test_0()
{
  {
    /*
     *  Perform operations on a pairing-heap queue.
     */
    cout << "Pairing heap" << endl;
    __gnu_pbds::priority_queue<int, less<int>, pairing_heap_tag> c;
    some_op_sequence(c);
  }

  {
    /*
     *  Perform operations on a binomial-heap queue.
     */
    cout << "Binomial heap" << endl;
    __gnu_pbds::priority_queue<int, less<int>, binomial_heap_tag> c;
    some_op_sequence(c);
  }

  {
    /*
     *  Perform operations on a binomial-heap queue.
     */
    cout << "Redundant-counter binomial heap" << endl;
    __gnu_pbds::priority_queue<int, less<int>, rc_binomial_heap_tag> c;
    some_op_sequence(c);
  }

  {
    /*
     *  Perform operations on a binary-heap queue.
     */
    cout << "Binary heap" << endl;
    __gnu_pbds::priority_queue<int, less<int>, binary_heap_tag> c;
    some_op_sequence(c);
  }

  {
    /*
     *  Perform operations on a thin-heap queue.
     */
    cout << "Thin heap" << endl;
    __gnu_pbds::priority_queue<int, less<int>, thin_heap_tag> c;
    some_op_sequence(c);
  }
}


void
priority_queue_link_regression_test_1()
{
  {
    /*
     *  Perform operations on a pairing-heap queue.
     */
    cout << "Pairing heap" << endl;
    __gnu_pbds::priority_queue<int, less<int>, pairing_heap_tag> c;
    some_op_sequence(c);
  }

  {
    /*
     *  Perform operations on a binomial-heap queue.
     */
    cout << "Binomial heap" << endl;
    __gnu_pbds::priority_queue<int, less<int>, binomial_heap_tag> c;
    some_op_sequence(c);
  }

  {
    /*
     *  Perform operations on a binomial-heap queue.
     */
    cout << "Redundant-counter binomial heap" << endl;
    __gnu_pbds::priority_queue<int, less<int>, rc_binomial_heap_tag> c;
    some_op_sequence(c);
  }

  {
    /*
     *  Perform operations on a binomial-heap queue.
     */
    cout << "Binary heap" << endl;
    __gnu_pbds::priority_queue<int, less<int>, binary_heap_tag> c;
    some_op_sequence(c);
  }

  {
    /*
     *  Perform operations on a thin-heap queue.
     */
    cout << "Thin heap" << endl;
    __gnu_pbds::priority_queue<int, less<int>, thin_heap_tag> c;
    some_op_sequence(c);
  }
}

int
main()
{
  priority_queue_link_regression_test_0();
  priority_queue_link_regression_test_1();
  return 0;
}
