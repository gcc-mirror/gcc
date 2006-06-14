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
using namespace pb_ds;
using namespace pb_ds;

int
main()
{
  // Two priority queue objects.
  pb_ds::priority_queue<int> even_p, odd_p;

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
  pb_ds::priority_queue<int>::const_iterator it;
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

