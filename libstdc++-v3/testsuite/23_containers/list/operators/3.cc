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

// splice(p, x, f, l) + sort + merge + unique
void
test03()
{
  const int A[] = {103, 203, 603, 303, 403, 503};
  const int B[] = {417, 417, 417, 417, 417};
  const int E[] = {103, 417, 417, 203, 603, 303, 403, 503};
  const int F[] = {103, 203, 303, 403, 417, 417, 503, 603};
  const int C[] = {103, 203, 303, 403, 417, 417, 417, 417, 417, 503, 603};
  const int D[] = {103, 203, 303, 403, 417, 503, 603};
  const int N = sizeof(A) / sizeof(int);
  const int M = sizeof(B) / sizeof(int);
  const int P = sizeof(C) / sizeof(int);
  const int Q = sizeof(D) / sizeof(int);
  const int R = sizeof(E) / sizeof(int);

  std::list<int> list0301(A, A + N);
  std::list<int> list0302(B, B + M);
  std::list<int> list0303(C, C + P);
  std::list<int> list0304(D, D + Q);
  std::list<int> list0305(E, E + R);
  std::list<int> list0306(F, F + R);
  std::list<int>::iterator p = list0301.begin();
  std::list<int>::iterator q = list0302.begin();

  ++p; ++q; ++q;
  list0301.splice(p, list0302, list0302.begin(), q);
  VERIFY(list0301 == list0305);
  VERIFY(list0301.size() == N + 2);
  VERIFY(list0302.size() == M - 2);

  list0301.sort();
  VERIFY(list0301 == list0306);

  list0301.merge(list0302);
  VERIFY(list0301.size() == N + M);
  VERIFY(list0302.size() == 0);
  VERIFY(list0301 == list0303);

  list0301.unique();
  VERIFY(list0301 == list0304);
}

#if !__GXX_WEAK__ && _MT_ALLOCATOR_H
// Explicitly instantiate for systems with no COMDAT or weak support.
template class __gnu_cxx::__mt_alloc<std::_List_node<int> >;
#endif

int main(void)
{
  test03();
  return 0;
}
