// Copyright (C) 2001-2020 Free Software Foundation, Inc.
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

// 25.3.3.1 lower_bound, with and without comparison predicate
void
test01()
{
    using std::lower_bound;

    const int first = A[0];
    const int last = A[N - 1];

    const int* p = lower_bound(A, A + N, 3);
    VERIFY(p == A + 2);

    const int* q = lower_bound(A, A + N, first);
    VERIFY(q == A + 0);

    const int* r = lower_bound(A, A + N, last);
    VERIFY(r == A + N - 1);

    const int* s = lower_bound(A, A + N, 4);
    VERIFY(s == A + 5);

    const int* t = lower_bound(C, C + N, 3, gt());
    VERIFY(t == C + 2);

    const int* u = lower_bound(C, C + N, first, gt());
    VERIFY(u == C + N - 1);

    const int* v = lower_bound(C, C + N, last, gt());
    VERIFY(v == C + 0);

    const int* w = lower_bound(C, C + N, 4, gt());
    VERIFY(w == C + 2);
}

int
main()
{
  test01();
  return 0;
}
