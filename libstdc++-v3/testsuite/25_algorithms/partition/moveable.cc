// { dg-options "-std=gnu++0x" }

// Copyright (C) 2005, 2006, 2007, 2008, 2009, 2010, 2011
// Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without Pred the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// 25.2.12 [lib.alg.partitions] Partitions.

#undef _GLIBCXX_CONCEPT_CHECKS

#include <algorithm>
#include <functional>
#include <testsuite_hooks.h>
#include <testsuite_rvalref.h>
#include <testsuite_iterators.h>

using __gnu_test::test_container;
using __gnu_test::forward_iterator_wrapper;
using __gnu_test::bidirectional_iterator_wrapper;
using __gnu_test::rvalstruct;

typedef test_container<rvalstruct, forward_iterator_wrapper> Fcontainer;
typedef test_container<rvalstruct, bidirectional_iterator_wrapper> Bcontainer;

bool test __attribute__((unused)) = true;

const int A[] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17};
const int B[] = {2, 4, 6, 8, 10, 12, 14, 16, 1, 3, 5, 7, 9, 11, 13, 15, 17};
const int N = sizeof(A) / sizeof(int);

struct Pred
{
    bool
    operator()(const rvalstruct& x) const
    { return (x.val % 2) == 0; }
};

// 25.2.12 partition()
void
test01()
{
  using std::partition;

  rvalstruct farray[N];   
  rvalstruct barray[N];

  std::copy(A, A + N, farray);
  std::copy(A, A + N, barray);

  Fcontainer fcon(farray, farray + N);
  Bcontainer bcon(barray, barray + N);  

  Pred pred;

  VERIFY(partition(fcon.begin(), fcon.end(), pred).ptr - farray == N/2); 
  for (const rvalstruct* i = farray; i < farray+N/2; ++i) 
    VERIFY(pred(*i));

  for (const rvalstruct* i = farray+N/2; i < farray + N; ++i) 
    VERIFY(!pred(*i));

  VERIFY(partition(bcon.begin(), bcon.end(), pred).ptr - barray == N/2); 

  for (const rvalstruct* i = barray; i < barray+N/2; ++i) 
    VERIFY(pred(*i));
  for (const rvalstruct* i = barray+N/2; i < barray + N; ++i) 
    VERIFY(!pred(*i));
}

int
main()
{
  test01();
  return 0;
}
