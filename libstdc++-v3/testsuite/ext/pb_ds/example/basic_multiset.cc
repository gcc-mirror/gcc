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
 * @file basic_multiset_example.cpp
 * A basic example showing how to use multisets.
 */


// This example shows how to use "multisets".

// In this example we build a very simple priority queue that also can
// be queried if an entry contains (i.e., it is slightly similar to an
// associative container as well as a priority queue). The priority
// queue adapts a "multiset".

// (Note that there are more efficient ways for implementing this than
// by adapting an associative container. This is just an example for
// "multisets".)

#include <iostream>
#include <cassert>
#include <ext/pb_ds/assoc_container.hpp>

using namespace std;
using namespace pb_ds;

// A simple priority queue that also supports an "contains" query.
class contains_pq
{
public:
  // Pushes an integer.
  void
  push(int i);

  // Pops the largest integer and returns it.
  int
  pop();

  // Returns true iff i is contained in the container.
  bool
  contains(int i) const
  { return m_tree.find(i) != m_tree.end(); }

  // Returns true iff empty.
  bool
  empty() const
  { return m_tree.empty(); }

private:
  // This is the container type we adapt - a "multiset".
  // It maps each integer to the number of times it logically appears.
  typedef
  tree<
  int,
  size_t,
  greater<
  int> >
  tree_t;

private:
  tree_t m_tree;
};

void
contains_pq::
push(int i)
{
  // To push i, we insert to the "multiset" that i appears 0 times
  // (which is a no-op if i already is contained), then increment the
  // number of times i is contained by 1.
  ++m_tree.insert(make_pair(i, 0)).first->second;
}

int
contains_pq::
pop()
{
  assert(!empty());

  // The element we need to pop must be the first one, since tree_t is
  // an ordered container.
  tree_t::iterator it = m_tree.begin();

  const int i = it->first;

  // Decrease the number of times the popped element appears in the
  // container object. If it is 0 - we erase it.
  if (--it->second == 0)
    m_tree.erase(it);

  return i;
}

int main()
{
  contains_pq cpq;

  // First we push some elements.
  cpq.push(4);
  cpq.push(3);
  cpq.push(2);
  cpq.push(1);
  cpq.push(4);

  // Note that logically, 4 appears 2 times, and each of 1, 2, and 3
  // appear once.
  assert(cpq.contains(4));
  assert(cpq.contains(3));
  assert(cpq.contains(2));
  assert(cpq.contains(1));

  // Now pop the topmost element - it should be 4.
  assert(cpq.pop() == 4);

  // Now logically, each of 1, 2, 3, and 4 appear once.
  assert(cpq.contains(4));

  // We pop the topmost element - it should be 4.
  assert(cpq.pop() == 4);

  // 4 should not be contained any more.
  assert(!cpq.contains(4));

  assert(cpq.contains(3));
  assert(cpq.contains(2));
  assert(cpq.contains(1));

  assert(cpq.pop() == 3);
  assert(cpq.pop() == 2);
  assert(cpq.pop() == 1);

  assert(cpq.empty());

  return 0;
}

