// Copyright (C) 2019-2023 Free Software Foundation, Inc.
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

#include <algorithm>
#include <vector>
#include <deque>

#include <ext/new_allocator.h>
#include <ext/malloc_allocator.h>

#include <testsuite_hooks.h>

void test01()
{
  using namespace std;

  deque<int> d;
  for (int i = 0; i != _GLIBCXX_STD_C::__deque_buf_size(sizeof(int)); ++i)
    d.push_back(i);

  const deque<int>& cd = d;

  VERIFY( equal(cd.begin(), cd.end(), cd.begin()) );
  VERIFY( equal(cd.begin(), cd.end(), d.begin()) );
  VERIFY( equal(d.begin(), d.end(), d.begin()) );
  VERIFY( equal(d.begin(), d.end(), cd.begin()) );
}

void test02()
{
  using namespace std;

  deque<int> d;
  for (int i = 0; i != 1024; ++i)
    d.push_back(i % 10);

  VERIFY( equal(d.begin(), d.begin() + 10, d.begin() + 20) );
  VERIFY( equal(d.begin() + 10, d.end() - 10, d.begin()) );

  const deque<int>& cd = d;

  VERIFY( equal(cd.begin(), cd.begin() + 10, cd.begin() + 20) );
  VERIFY( equal(cd.begin() + 10, cd.end() - 10, d.begin()) );
  VERIFY( equal(d.begin() + 10, d.end() - 10, cd.begin()) );
}

void test03()
{
  using namespace std;

  deque<int> d1;
  for (int i = 0; i != 1024; ++i)
    d1.push_back(i % 10);

  deque<int> d2(d1);
  for (int i = 0; i != 10; ++i)
    d2.pop_front();

  VERIFY( equal(d1.begin(), d1.begin() + 10, d2.begin()) );
  VERIFY( equal(d1.begin() + 10, d1.end() - 10, d2.begin()) );

  const deque<int>& cd1 = d1;
  const deque<int>& cd2 = d2;

  VERIFY( equal(cd1.begin(), cd1.begin() + 10, cd2.begin() + 20) );
  VERIFY( equal(cd1.begin() + 10, cd1.end() - 10, d2.begin()) );
  VERIFY( equal(cd2.begin() + 10, cd2.end() - 10, cd1.begin()) );
}

void test04()
{
  using namespace std;

  deque<int> d;
  for (int i = 0; i != 1024; ++i)
    d.push_back(i);

  vector<int> v(d.begin(), d.end());

  VERIFY( equal(d.begin(), d.end(), v.begin()) );
  VERIFY( equal(v.begin(), v.end(), d.begin()) );

  const deque<int>& cd = d;

  VERIFY( equal(cd.begin(), cd.end(), v.begin()) );
  VERIFY( equal(v.begin(), v.end(), cd.begin()) );
}

void test05()
{
  using namespace std;

  int a[] = { 0, 1, 2, 3, 4 };
  deque<int, __gnu_cxx::new_allocator<int> > d1(a, a + 5);
  deque<int, __gnu_cxx::malloc_allocator<int> > d2(a, a + 5);

  VERIFY( equal(d1.begin(), d1.end(), d2.begin()) );
}

int main()
{
  test01();
  test02();
  test03();
  test04();
  test05();
  return 0;
}
