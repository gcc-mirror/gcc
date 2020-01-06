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

// 23.2.2.4 list operations [lib.list.ops]

#include <testsuite_hooks.h>

// A comparison predicate to order by rightmost digit.  Tracks call
// counts for performance checks.
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
template<typename _Tp>
void
operations04()
{
  typedef _Tp list_type;

  const int A[] = {1, 2, 3, 4, 5, 6};
  const int B[] = {12, 15, 13, 14, 11};
  const int C[] = {11, 12, 13, 14, 15};
  const int D[] = {1, 11, 2, 12, 3, 13, 4, 14, 5, 15, 6};
  const int N = sizeof(A) / sizeof(int);
  const int M = sizeof(B) / sizeof(int);
  const int Q = sizeof(D) / sizeof(int);

  list_type list0401(A, A + N);
  list_type list0402(B, B + M);
  list_type list0403(C, C + M);
  list_type list0404(D, D + Q);
  list_type list0405(A, A + N);

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

