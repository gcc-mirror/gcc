// Copyright (C) 2001-2016 Free Software Foundation, Inc.
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

// 25.3.6 Heap operations [lib.alg.heap.operations]

#include <algorithm>
#include <testsuite_hooks.h>

const int A[] = {1, 11, 12, 3, 10, 6, 17, 4, 8, 2, 5, 13, 9, 15, 14, 16, 7};
const int B[] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17};
const int C[] = {17, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1};
const int N = sizeof(A) / sizeof(int);

// This functor has the equivalent functionality of std::greater<>,
// but there is no dependency on <functional> and it also tracks the
// number of invocations since creation.
class Gt
{
public:
  static int count() { return _M_count; }
  static void reset() { _M_count = 0; }
  
  bool
  operator()(const int& x, const int& y)
  {
    ++_M_count;
    return x > y; 
  }

private:
  static int _M_count;
};

int Gt::_M_count = 0;

// Exercise all of the heap functions for operator<.  The intermediate
// results between push_heap and pop_heap and make_heap and sort_heap
// are not checked (they could be).
void
test01()
{
  // sort array s1 using push_heap/pop_heap
  int s1[N];
  std::copy(A, A + N, s1);
  VERIFY(std::equal(s1, s1 + N, A));
  
  for (int i = 2; i <= N; ++i)
    std::push_heap(s1, s1 + i);
  
  for (int i = N; i >= 2; --i)
    std::pop_heap(s1, s1 + i);
  
  VERIFY(std::equal(s1, s1 + N, B));

  // sort array s2 using make_heap/sort_heap
  int s2[N];
  std::copy(A, A + N, s2);
  VERIFY(std::equal(s2, s2 + N, A));
  
  std::make_heap(s2, s2 + N);
  std::sort_heap(s2, s2 + N);
  VERIFY(std::equal(s2, s2 + N, B));
}

// Perform same tests as above but with the comparison predicate
// versions, and add complexity constraint checks.
void
test02()
{
  Gt gt;

#ifndef _GLIBCXX_DEBUG
  //const int logN = static_cast<int>(std::log(static_cast<double>(N)) + 0.5);
  const int logN = 3;
#endif
  
  int s1[N];
  std::copy(A, A + N, s1);
  VERIFY(std::equal(s1, s1 + N, A));
  
  for (int i = 2; i <= N; ++i)
    {
      std::push_heap(s1, s1 + i, gt);
#ifndef _GLIBCXX_DEBUG
      VERIFY(gt.count() <= logN);
#endif
      gt.reset();
    }

  for (int i = N; i >= 2; --i)
    {
      std::pop_heap(s1, s1 + i, gt);
#ifndef _GLIBCXX_DEBUG
      VERIFY(gt.count() <= 2 * logN);
#endif
      gt.reset();
    }

  VERIFY(std::equal(s1, s1 + N, C));
  
  // sort array s2 using make_heap/sort_heap
  int s2[N];
  std::copy(A, A + N, s2);
  VERIFY(std::equal(s2, s2 + N, A));
  
  std::make_heap(s2, s2 + N, gt);
#ifndef _GLIBCXX_DEBUG
  VERIFY(gt.count() <= 3 * N);
#endif
  gt.reset();

  std::sort_heap(s2, s2 + N, gt);
#ifndef _GLIBCXX_DEBUG
  VERIFY(gt.count() <= N * logN);
#endif
  
  VERIFY(std::equal(s2, s2 + N, C));
}

int
main()
{
  test01();
  test02();
  return 0;
}
