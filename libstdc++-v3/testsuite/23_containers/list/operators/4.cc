// Copyright (C) 2001, 2004 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without Pred the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.

// 23.2.2.4 list operations [lib.list.ops]

#include <list>
#include <testsuite_hooks.h>

bool test __attribute__((unused)) = true;

// A comparison predicate to order by rightmost digit.  Tracks call counts for
// performance checks.
struct CompLastLt
{
  bool operator()(const int x, const int y) 
  { ++itsCount; return x % 10 < y % 10; }
  static int count() { return itsCount; }
  static void reset() { itsCount = 0; }
  static int itsCount;
};

int CompLastLt::itsCount;

struct CompLastEq
{
  bool operator()(const int x, const int y) 
  { ++itsCount; return x % 10 == y % 10; }
  static int count() { return itsCount; }
  static void reset() { itsCount = 0; }
  static int itsCount;
};

int CompLastEq::itsCount;

// sort(pred) + merge(pred) + unique(pred)
// also checks performance requirements
void
test04()
{
  const int A[] = {1, 2, 3, 4, 5, 6};
  const int B[] = {12, 15, 13, 14, 11};
  const int C[] = {11, 12, 13, 14, 15};
  const int D[] = {1, 11, 2, 12, 3, 13, 4, 14, 5, 15, 6};
  const int N = sizeof(A) / sizeof(int);
  const int M = sizeof(B) / sizeof(int);
  const int Q = sizeof(D) / sizeof(int);

  std::list<int> list0401(A, A + N);
  std::list<int> list0402(B, B + M);
  std::list<int> list0403(C, C + M);
  std::list<int> list0404(D, D + Q);
  std::list<int> list0405(A, A + N);

  // sort B
  CompLastLt lt;

  CompLastLt::reset();
  list0402.sort(lt);
  VERIFY(list0402 == list0403);

  CompLastLt::reset();
  list0401.merge(list0402, lt);
  VERIFY(list0401 == list0404);
#ifndef _GLIBCXX_DEBUG
  VERIFY(lt.count() <= (N + M - 1));
#endif

  CompLastEq eq;

  CompLastEq::reset();
  list0401.unique(eq);
  VERIFY(list0401 == list0405);
#ifndef _GLIBCXX_DEBUG
  VERIFY(eq.count() == (N + M - 1));
#endif
}

#if !__GXX_WEAK__ && _MT_ALLOCATOR_H
// Explicitly instantiate for systems with no COMDAT or weak support.
template class __gnu_cxx::__mt_alloc<std::_List_node<int> >;
#endif

int main()
{
  test04();
  return 0;
}

