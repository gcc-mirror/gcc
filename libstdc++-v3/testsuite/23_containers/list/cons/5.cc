// Copyright (C) 2001, 2004, 2005 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// 23.2.2.1 list constructors, copy, and assignment

#include <list>
#include <testsuite_hooks.h>

bool test __attribute__((unused)) = true;

// Copy constructor
//
// This test verifies the following.
// 23.2.2.1     list(const list& x)
// 23.2.2       reverse_iterator rbegin() 
// 23.2.2       reverse_iterator rend()
// 23.2.2       size_type size() const
//
void
test04()
{
  const int A[] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17};
  const std::size_t N = sizeof(A) / sizeof(int);
  int count;
  std::list<int>::reverse_iterator i;
  std::list<int> list0401(A, A + N);

  std::list<int> list0402(list0401);
  for (i = list0401.rbegin(), count = N - 1;
       i != list0401.rend();
       ++i, --count)
    VERIFY(*i == A[count]);
  VERIFY(count == -1);
  VERIFY(list0401.size() == N);
}

int main()
{
  test04();
  return 0;
}

