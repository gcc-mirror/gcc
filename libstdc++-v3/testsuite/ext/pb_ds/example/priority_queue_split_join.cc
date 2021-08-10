// -*- C++ -*-

// Copyright (C) 2005-2021 Free Software Foundation, Inc.
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

// { dg-add-options using-deprecated }

/**
 * @file priority_queue_split_join_example.cpp
 * A basic example showing how to split and join priority queues.
 */

/**
 * This example shows how to split and join priority queues.
 */

#include <functional>
#include <iostream>
#include <cassert>
#include <ext/pb_ds/priority_queue.hpp>

using namespace std;
using namespace __gnu_pbds;

int
main()
{
  // Two priority queue objects.
  __gnu_pbds::priority_queue<int> even_p, odd_p;

  // First we insert some values: even ones into even_p, and odd ones
  // into odd_p.
  for (size_t i = 0; i < 10; ++i)
    {
      even_p.push(2*  i);
      odd_p.push(2*  i + 1);
    }

  // Check that each one contains the appropriate 10 values.
  assert(even_p.size() == 10);
  assert(even_p.top() == 18);

  // Print out the values.
  cout << "Initial values in even priority queue:" << endl;
  __gnu_pbds::priority_queue<int>::const_iterator it;
  for (it = even_p.begin(); it != even_p.end(); ++it)
    cout <<* it << endl;

  assert(odd_p.size() == 10);
  assert(odd_p.top() == 19);

  // Print out the values.
  cout << "Initial values in odd priority queue:" << endl;
  for (it = odd_p.begin(); it != odd_p.end(); ++it)
    cout <<* it << endl;

  // Now join the queues.
  even_p.join(odd_p);

  // Check that each one contains the appropriate values.

  assert(even_p.size() == 20);
  assert(even_p.top() == 19);

  // Print out the values.
  cout << "After-join values in even priority queue:" << endl;
  for (it = even_p.begin(); it != even_p.end(); ++it)
    cout <<* it << endl;

  assert(odd_p.size() == 0);

  // Print out the values.
  cout << "After-join values in odd priority queue:" << endl;
  for (it = odd_p.begin(); it != odd_p.end(); ++it)
    cout <<* it << endl;

  // Now split the queues.
  even_p.split(bind2nd(modulus<int>(), 2), odd_p);

  // Check that each one contains the appropriate 10 values.

  assert(even_p.size() == 10);
  assert(even_p.top() == 18);

  // Print out the values.
  cout << "After-split values in even priority queue:" << endl;
  for (it = even_p.begin(); it != even_p.end(); ++it)
    cout <<* it << endl;

  assert(odd_p.size() == 10);
  assert(odd_p.top() == 19);

  // Print out the values.
  cout << "After-split values in odd priority queue:" << endl;
  for (it = odd_p.begin(); it != odd_p.end(); ++it)
    cout <<* it << endl;

  return 0;
}

