// Copyright (C) 2013-2018 Free Software Foundation, Inc.
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

#include <iterator>
#include <vector>
#include <algorithm>
#include <testsuite_hooks.h>

const bool A[] = { true, true, false, true, false, false, true, false };
const int B[] = { false, false, false, false, true, true, true, true };
const int C[] = { true, true, true, true, false, false, false, false };
const int N = sizeof(A) / sizeof(bool);

// This functor has the equivalent functionality of std::greater<>,
// but there is no dependency on <functional> and it also tracks the
// number of invocations since creation.
class Gt
{
public:
  static int count() { return _S_count; }
  static void reset() { _S_count = 0; }
  
  bool
  operator()(bool x, bool y) const
  {
    ++_S_count;
    return x > y; 
  }

private:
  static int _S_count;
};

int Gt::_S_count = 0;

// Exercise all of the heap functions for operator<.  The intermediate
// results between push_heap and pop_heap and make_heap and sort_heap
// are not checked (they could be).
void
test01()
{
  // sort array s1 using push_heap/pop_heap
  std::vector<bool> s1;
  std::copy(A, A + N, std::back_inserter(s1));
  VERIFY( std::equal(s1.begin(), s1.begin() + N, A) );
  
  for (int i = 2; i <= N; ++i)
    std::push_heap(s1.begin(), s1.begin() + i);
  
  for (int i = N; i >= 2; --i)
    std::pop_heap(s1.begin(), s1.begin() + i);
  
  VERIFY( std::equal(s1.begin(), s1.begin() + N, B) );

  // sort array s2 using make_heap/sort_heap
  std::vector<bool> s2;
  std::copy(A, A + N, std::back_inserter(s2));
  VERIFY( std::equal(s2.begin(), s2.begin() + N, A) );
  
  std::make_heap(s2.begin(), s2.begin() + N);
  std::sort_heap(s2.begin(), s2.begin() + N);
  VERIFY( std::equal(s2.begin(), s2.begin() + N, B) );
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
  
  std::vector<bool> s1;
  std::copy(A, A + N, std::back_inserter(s1));
  VERIFY(std::equal(s1.begin(), s1.begin() + N, A));
  
  for (int i = 2; i <= N; ++i)
    {
      std::push_heap(s1.begin(), s1.begin() + i, gt);
#ifndef _GLIBCXX_DEBUG
      VERIFY(gt.count() <= logN);
#endif
      gt.reset();
    }

  for (int i = N; i >= 2; --i)
    {
      std::pop_heap(s1.begin(), s1.begin() + i, gt);
#ifndef _GLIBCXX_DEBUG
      VERIFY(gt.count() <= 2 * logN);
#endif
      gt.reset();
    }

  VERIFY(std::equal(s1.begin(), s1.begin() + N, C));
  
  // sort array s2 using make_heap/sort_heap
  std::vector<bool> s2;
  std::copy(A, A + N, std::back_inserter(s2));
  VERIFY(std::equal(s2.begin(), s2.begin() + N, A));
  
  std::make_heap(s2.begin(), s2.begin() + N, gt);
#ifndef _GLIBCXX_DEBUG
  VERIFY(gt.count() <= 3 * N);
#endif
  gt.reset();

  std::sort_heap(s2.begin(), s2.begin() + N, gt);
#ifndef _GLIBCXX_DEBUG
  VERIFY(gt.count() <= N * logN);
#endif
  
  VERIFY(std::equal(s2.begin(), s2.begin() + N, C));
}

int
main()
{
  test01();
  test02();
  return 0;
}
