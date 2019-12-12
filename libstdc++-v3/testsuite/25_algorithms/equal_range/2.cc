// Copyright (C) 2001-2019 Free Software Foundation, Inc.
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

// 25.3.3 [lib.alg.binary.search] Binary search algorithms.

#include <algorithm>
#include <testsuite_hooks.h>

const int A[] = {1, 2, 3, 3, 3, 5, 8};
const int C[] = {8, 5, 3, 3, 3, 2, 1};
const int N = sizeof(A) / sizeof(int);

// A comparison, equalivalent to std::greater<int> without the
// dependency on <functional>.
struct gt
{
    bool
    operator()(const int& x, const int& y) const
    { return x > y; }
};

// Each test performs general-case, bookend, not-found condition,
// and predicate functional checks.

// 25.3.3.3 equal_range, with and without comparison predicate
void
test03()
{
    using std::equal_range;
    typedef std::pair<const int*, const int*> Ipair;
    
    const int first = A[0];
    const int last = A[N - 1];

    Ipair p = equal_range(A, A + N, 3);
    VERIFY(p.first == A + 2);
    VERIFY(p.second == A + 5);
    
    Ipair q = equal_range(A, A + N, first);
    VERIFY(q.first == A + 0);
    VERIFY(q.second == A + 1);
    
    Ipair r = equal_range(A, A + N, last);
    VERIFY(r.first == A + N - 1);
    VERIFY(r.second == A + N);
    
    Ipair s = equal_range(A, A + N, 4);
    VERIFY(s.first == A + 5);
    VERIFY(s.second == A + 5);
    
    Ipair t = equal_range(C, C + N, 3, gt());
    VERIFY(t.first == C + 2);
    VERIFY(t.second == C + 5);
    
    Ipair u = equal_range(C, C + N, first, gt());
    VERIFY(u.first == C + N - 1);
    VERIFY(u.second == C + N);
    
    Ipair v = equal_range(C, C + N, last, gt());
    VERIFY(v.first == C + 0);
    VERIFY(v.second == C + 1);
    
    Ipair w = equal_range(C, C + N, 4, gt());
    VERIFY(w.first == C + 2);
    VERIFY(w.second == C + 2);
}

int
main()
{
  test03();
  return 0;
}
