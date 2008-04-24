// Copyright (C) 2001, 2004, 2005 Free Software Foundation, Inc.
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
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// 23.2.2.4 list operations [lib.list.ops]

#include <list>
#include <testsuite_hooks.h>

bool test __attribute__((unused)) = true;

// splice(p, x, i) + remove_if + operator==
void
test02()
{
  const int A[] = {1, 2, 3, 4, 5};
  const int B[] = {2, 1, 3, 4, 5};
  const int C[] = {1, 3, 4, 5, 2};
  const int N = sizeof(A) / sizeof(int);
  std::list<int> list0201(A, A + N);
  std::list<int> list0202(A, A + N);
  std::list<int> list0203(B, B + N);
  std::list<int> list0204(C, C + N);
  std::list<int>::iterator i = list0201.begin();

  // result should be unchanged
  list0201.splice(list0201.begin(), list0201, i);
  VERIFY(list0201 == list0202);

  // result should be [2 1 3 4 5]
  ++i;
  list0201.splice(list0201.begin(), list0201, i);
  VERIFY(list0201 != list0202);
  VERIFY(list0201 == list0203);

  // result should be [1 3 4 5 2]
  list0201.splice(list0201.end(), list0201, i);
  VERIFY(list0201 == list0204);
}

int main()
{
  test02();
  return 0;
}
