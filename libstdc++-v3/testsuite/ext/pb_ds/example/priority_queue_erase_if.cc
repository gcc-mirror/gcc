// -*- C++ -*-

// Copyright (C) 2005-2017 Free Software Foundation, Inc.
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
 * @file priority_queue_erase_if.cpp
 * Example showing how to conditionally erase values from a priority queue.
 */

/**
 * This example shows how to erase from a priority queue using erase_if.
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
  __gnu_pbds::priority_queue<int> p;

  // First we insert some values into the container.
  for (int i = 0; i < 1000; ++i)
    p.push(i);

  assert(p.top() == 999);

  // Now we erase all values that satisfy some predicate, in this case
  // one that returns true for all those larger than 500.
  p.erase_if(bind1st(less<int>(), 500));

  // The largest value should be now 500.
  assert(p.top() == 500);
}

