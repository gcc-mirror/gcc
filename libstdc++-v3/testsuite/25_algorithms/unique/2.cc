// 2003-10-14  Paolo Carlini  <pcarlini@unitus.it>

// Copyright (C) 2003-2021 Free Software Foundation, Inc.
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

// 25.2.8 [lib.alg.unique] Unique

#include <list>
#include <algorithm>
#include <functional>
#include <testsuite_hooks.h>

const int T1[] = {1, 4, 4, 6, 1, 2, 2, 3, 1, 6, 6, 6, 5, 7, 5, 4, 4};
const int T2[] = {1, 1, 1, 2, 2, 1, 1, 7, 6, 6, 7, 8, 8, 8, 8, 9, 9};
const int N = sizeof(T1) / sizeof(int);

const int A1[] = {1, 4, 6, 1, 2, 3, 1, 6, 5, 7, 5, 4};
const int A2[] = {1, 4, 4, 6, 6, 6, 6, 7};
const int A3[] = {1, 1, 1};

const int B1[] = {1, 2, 1, 7, 6, 7, 8, 9};
const int B2[] = {1, 1, 1, 2, 2, 7, 7, 8, 8, 8, 8, 9, 9};
const int B3[] = {9, 9, 8, 8, 8, 8, 7, 6, 6, 1, 1, 1, 1, 1};

void test01()
{
  using namespace std;

  list<int>::iterator pos;

  list<int> coll(T1, T1 + N);
  pos = unique(coll.begin(), coll.end());
  VERIFY( equal(coll.begin(), pos, A1) );

  list<int> coll2(T2, T2 + N);
  pos = unique(coll2.begin(), coll2.end());
  VERIFY( equal(coll2.begin(), pos, B1) );
}

void test02()
{
  using namespace std;

  list<int>::iterator pos;

  list<int> coll(T1, T1 + N);
  pos = unique(coll.begin(), coll.end(), greater<int>());
  VERIFY( equal(coll.begin(), pos, A2) );

  list<int> coll2(T2, T2 + N);
  pos = unique(coll2.begin(), coll2.end(), greater<int>());
  VERIFY( equal(coll2.begin(), pos, B2) );
}

void test03()
{
  using namespace std;

  list<int>::iterator pos;

  list<int> coll(T1, T1 + N);
  pos = unique(coll.begin(), coll.end(), less<int>());
  VERIFY( equal(coll.begin(), pos, A3) );

  list<int> coll2(T2, T2 + N);
  reverse(coll2.begin(), coll2.end());
  pos = unique(coll2.begin(), coll2.end(), less<int>());
  VERIFY( equal(coll2.begin(), pos, B3) );
}

int main()
{
  test01();
  test02();
  test03();
  return 0;
}
