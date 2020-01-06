// 1999-06-29 bkoz

// Copyright (C) 1999-2020 Free Software Foundation, Inc.
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

// 23.2.4.1 vector constructors, copy, and assignment

#include <vector>
#include <string>
#include <testsuite_allocator.h>
#include <testsuite_hooks.h>

// test range constructors and range-fill constructor
void
test03()
{
  const int A[] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17};
  const int B[] = {7, 7, 7, 7, 7};
  const int N = sizeof(A) / sizeof(int);
  const int M = sizeof(B) / sizeof(int);
  
  std::vector<int> v3(A, A + N);
  VERIFY(std::equal(v3.begin(), v3.end(), A));
  
  std::vector<int> v4(v3.begin(), v3.end());
  VERIFY(std::equal(v4.begin(), v4.end(), A));
  
  std::vector<int> v5(M, 7);
  VERIFY(std::equal(v5.begin(), v5.end(), B));
  VERIFY(std::equal(B, B + M, v5.begin()));
}

int main()
{
  test03();
  return 0;
}
