// Copyright (C) 2020-2025 Free Software Foundation, Inc.
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

// { dg-timeout-factor 2 { target debug_mode } }

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

  VERIFY( !lexicographical_compare(cd.begin(), cd.end(), cd.begin(), cd.end()) );
  VERIFY( !lexicographical_compare(cd.begin(), cd.end(), d.begin(), d.end()) );
  VERIFY( !lexicographical_compare(d.begin(), d.end(), d.begin(), d.end()) );
  VERIFY( !lexicographical_compare(d.begin(), d.end(), cd.begin(), cd.end()) );

  const deque<int>::iterator first = d.begin(), last = d.end();
  VERIFY( lexicographical_compare(first, last - 1, first, last) );
  VERIFY( !lexicographical_compare(first, last, first, last - 1) );
  VERIFY( lexicographical_compare(first, last, first + 1, last) );
  VERIFY( !lexicographical_compare(first + 1, last, first, last) );
}

void test02()
{
  using namespace std;

  deque<int> d;
  for (int i = 0; i != 1000; ++i)
    d.push_back(i % 10);

  VERIFY( lexicographical_compare(d.begin(), d.begin() + 10,
				  d.begin() + 21, d.begin() + 31) );
  VERIFY( lexicographical_compare(d.begin(), d.begin() + 10,
				  d.begin() + 20, d.begin() + 31) );
  VERIFY( ! lexicographical_compare(d.begin() + 1, d.begin() + 10,
				    d.begin() + 21, d.begin() + 30) );
  VERIFY( !lexicographical_compare(d.begin(), d.begin() + 10,
				   d.begin() + 20, d.begin() + 30) );
  VERIFY( !lexicographical_compare(d.begin() + 1, d.begin() + 10,
				   d.begin() + 1 + 20, d.begin() + 30) );
  VERIFY( lexicographical_compare(d.begin(), d.begin() + 10,
				  d.begin() + 20, d.begin() + 31) );
  VERIFY( !lexicographical_compare(d.begin() + 10, d.end() - 10,
				   d.begin(), d.end() - 20) );

  const deque<int>& cd = d;

  VERIFY( lexicographical_compare(cd.begin(), cd.begin() + 10,
				  cd.begin() + 21, cd.begin() + 31) );
  VERIFY( lexicographical_compare(cd.begin() + 1, cd.begin() + 10,
				  cd.begin() + 21, cd.begin() + 32) );
  VERIFY( !lexicographical_compare(cd.begin(), cd.begin() + 10,
				   cd.begin() + 20, cd.begin() + 30) );
  VERIFY( !lexicographical_compare(cd.begin() + 1, cd.begin() + 10,
				   cd.begin() + 21, cd.begin() + 30) );
  VERIFY( !lexicographical_compare(cd.begin() + 10, cd.end() - 10,
				   d.begin(), d.end() - 20) );
  VERIFY( !lexicographical_compare(d.begin() + 10, d.end() - 10,
				   cd.begin(), cd.end() - 20) );
}

void test03()
{
  using namespace std;

  deque<int> d1;
  for (int i = 0; i != 1000; ++i)
    d1.push_back(i % 10);

  deque<int> d2(d1);
  for (int i = 0; i != 10; ++i)
    d2.pop_front();

  VERIFY( !lexicographical_compare(d1.begin(), d1.begin() + 10,
				   d2.begin(), d2.begin() + 10) );
  VERIFY( !lexicographical_compare(d1.begin() + 10, d1.end() - 10,
				   d2.begin(), d2.end() - 10) );

  const deque<int>& cd1 = d1;
  const deque<int>& cd2 = d2;

  VERIFY( !lexicographical_compare(cd1.begin(), cd1.begin() + 10,
				   cd2.begin() + 20, cd2.begin() + 30) );
  VERIFY( !lexicographical_compare(cd1.begin() + 10, cd1.end() - 10,
				   d2.begin(), d2.end() - 10) );
  VERIFY( lexicographical_compare(cd2.begin() + 10, cd2.end() - 10,
				  cd1.begin(), cd1.end() - 20) );
}

void test04()
{
  using namespace std;

  deque<int> d;
  for (int i = 0; i != 1024; ++i)
    d.push_back(i);

  vector<int> v(d.begin(), d.end());

  VERIFY( lexicographical_compare(d.begin() + 5, d.end() - 1, v.begin() + 5, v.end()) );
  VERIFY( !lexicographical_compare(v.begin(), v.end(), d.begin(), d.end()) );

  const deque<int>& cd = d;

  VERIFY( !lexicographical_compare(cd.begin(), cd.end(), v.begin(), v.end()) );
  VERIFY( !lexicographical_compare(v.begin(), v.end(), cd.begin(), cd.end()) );
}

void test05()
{
  using namespace std;

  int a[] = { 0, 1, 2, 3, 4 };
  deque<int, __gnu_cxx::new_allocator<int> > d1(a, a + 5);
  deque<int, __gnu_cxx::malloc_allocator<int> > d2(a, a + 5);

  VERIFY( !lexicographical_compare(d1.begin(), d1.end(), d2.begin(), d2.end()) );
}

void
test06()
{
  using namespace std;

  deque<int> d;
  int i = 0;
  VERIFY( lexicographical_compare(d.begin(), d.end(), &i, &i + 1) );
  VERIFY( !lexicographical_compare(&i, &i + 1, d.begin(), d.end()) );
}

void test07()
{
  using namespace std;

  deque<unsigned char> d;
  for (int i = 0; i != _GLIBCXX_STD_C::__deque_buf_size(sizeof(int)); ++i)
    d.push_back(i);

  const deque<unsigned char>& cd = d;

  VERIFY( !lexicographical_compare(cd.begin(), cd.end(), cd.begin(), cd.end()) );
  VERIFY( !lexicographical_compare(cd.begin(), cd.end(), d.begin(), d.end()) );
  VERIFY( !lexicographical_compare(d.begin(), d.end(), d.begin(), d.end()) );
  VERIFY( !lexicographical_compare(d.begin(), d.end(), cd.begin(), cd.end()) );

  const deque<unsigned char>::iterator first = d.begin(), last = d.end();
  VERIFY( lexicographical_compare(first, last - 1, first, last) );
  VERIFY( !lexicographical_compare(first, last, first, last - 1) );
  VERIFY( lexicographical_compare(first, last, first + 1, last) );
  VERIFY( !lexicographical_compare(first + 1, last, first, last) );
}

void test08()
{
  using namespace std;

  deque<unsigned char> d;
  for (int i = 0; i != 1000; ++i)
    d.push_back(i % 10);

  VERIFY( lexicographical_compare(d.begin(), d.begin() + 10,
				  d.begin() + 21, d.begin() + 31) );
  VERIFY( lexicographical_compare(d.begin(), d.begin() + 10,
				  d.begin() + 20, d.begin() + 31) );
  VERIFY( ! lexicographical_compare(d.begin() + 1, d.begin() + 10,
				    d.begin() + 21, d.begin() + 30) );
  VERIFY( !lexicographical_compare(d.begin(), d.begin() + 10,
				   d.begin() + 20, d.begin() + 30) );
  VERIFY( !lexicographical_compare(d.begin() + 1, d.begin() + 10,
				   d.begin() + 1 + 20, d.begin() + 30) );
  VERIFY( lexicographical_compare(d.begin(), d.begin() + 10,
				  d.begin() + 20, d.begin() + 31) );
  VERIFY( !lexicographical_compare(d.begin() + 10, d.end() - 10,
				   d.begin(), d.end() - 20) );

  const deque<unsigned char>& cd = d;

  VERIFY( lexicographical_compare(cd.begin(), cd.begin() + 10,
				  cd.begin() + 21, cd.begin() + 31) );
  VERIFY( lexicographical_compare(cd.begin() + 1, cd.begin() + 10,
				  cd.begin() + 21, cd.begin() + 32) );
  VERIFY( !lexicographical_compare(cd.begin(), cd.begin() + 10,
				   cd.begin() + 20, cd.begin() + 30) );
  VERIFY( !lexicographical_compare(cd.begin() + 1, cd.begin() + 10,
				   cd.begin() + 21, cd.begin() + 30) );
  VERIFY( !lexicographical_compare(cd.begin() + 10, cd.end() - 10,
				   d.begin(), d.end() - 20) );
  VERIFY( !lexicographical_compare(d.begin() + 10, d.end() - 10,
				   cd.begin(), cd.end() - 20) );
}

void test09()
{
  using namespace std;

  deque<unsigned char> d1;
  for (int i = 0; i != 1000; ++i)
    d1.push_back(i % 10);

  deque<unsigned char> d2(d1);
  for (int i = 0; i != 10; ++i)
    d2.pop_front();

  VERIFY( !lexicographical_compare(d1.begin(), d1.begin() + 10,
				   d2.begin(), d2.begin() + 10) );
  VERIFY( !lexicographical_compare(d1.begin() + 10, d1.end() - 10,
				   d2.begin(), d2.end() - 10) );

  const deque<unsigned char>& cd1 = d1;
  const deque<unsigned char>& cd2 = d2;

  VERIFY( !lexicographical_compare(cd1.begin(), cd1.begin() + 10,
				   cd2.begin() + 20, cd2.begin() + 30) );
  VERIFY( !lexicographical_compare(cd1.begin() + 10, cd1.end() - 10,
				   d2.begin(), d2.end() - 10) );
  VERIFY( lexicographical_compare(cd2.begin() + 10, cd2.end() - 10,
				  cd1.begin(), cd1.end() - 20) );
}

void test10()
{
  using namespace std;

  deque<unsigned char> d;
  for (int i = 0; i != 1024; ++i)
    d.push_back(i);

  vector<unsigned char> v(d.begin(), d.end());

  VERIFY( lexicographical_compare(d.begin() + 5, d.end() - 1, v.begin() + 5, v.end()) );
  VERIFY( !lexicographical_compare(v.begin(), v.end(), d.begin(), d.end()) );

  const deque<unsigned char>& cd = d;

  VERIFY( !lexicographical_compare(cd.begin(), cd.end(), v.begin(), v.end()) );
  VERIFY( !lexicographical_compare(v.begin(), v.end(), cd.begin(), cd.end()) );
}

void test11()
{
  using namespace std;

  int a[] = { 0, 1, 2, 3, 4 };
  deque<unsigned char, __gnu_cxx::new_allocator<unsigned char> > d1(a, a + 5);
  deque<unsigned char, __gnu_cxx::malloc_allocator<unsigned char> > d2(a, a + 5);

  VERIFY( !lexicographical_compare(d1.begin(), d1.end(), d2.begin(), d2.end()) );
}

void
test12()
{
  using namespace std;

  deque<unsigned char> d;
  int i = 0;
  VERIFY( lexicographical_compare(d.begin(), d.end(), &i, &i + 1) );
  VERIFY( !lexicographical_compare(&i, &i + 1, d.begin(), d.end()) );
}

int main()
{
  test01();
  test02();
  test03();
  test04();
  test05();
  test06();
  test07();
  test08();
  test09();
  test10();
  test11();
  test12();
}
