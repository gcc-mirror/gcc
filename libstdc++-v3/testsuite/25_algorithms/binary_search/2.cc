// Copyright (C) 2001-2025 Free Software Foundation, Inc.
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

// 25.3.3.4 binary_search, with and without comparison predicate
void
test04()
{
    using std::binary_search;
    
    const int first = A[0];
    const int last = A[N - 1];

    VERIFY(binary_search(A, A + N, 5));
    VERIFY(binary_search(A, A + N, first));
    VERIFY(binary_search(A, A + N, last));
    VERIFY(!binary_search(A, A + N, 4));

    VERIFY(binary_search(C, C + N, 5, gt()));
    VERIFY(binary_search(C, C + N, first, gt()));
    VERIFY(binary_search(C, C + N, last, gt()));
    VERIFY(!binary_search(C, C + N, 4, gt()));
}

int
main()
{
  test04();  
  return 0;
}
