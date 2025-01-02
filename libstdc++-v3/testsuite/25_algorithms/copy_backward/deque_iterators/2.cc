// Copyright (C) 2019-2025 Free Software Foundation, Inc.
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
#include <list>
#include <deque>

#include <testsuite_hooks.h>

void test01()
{
  using namespace std;

  deque<int> d;
  for (int i = 0; i != _GLIBCXX_STD_C::__deque_buf_size(sizeof(int)); ++i)
    d.push_back(i);

  deque<int> dest(d.size(), 0);

  copy_backward(d.begin(), d.end(), dest.end());

  VERIFY( equal(dest.begin(), dest.end(), d.begin()) );
}

void test02()
{
  using namespace std;

  deque<int> d;
  for (int i = 0; i != 4 * _GLIBCXX_STD_C::__deque_buf_size(sizeof(int)); ++i)
    d.push_back(i);

  deque<int> dest(d.size(), 0);

  const deque<int>& cd = d;
  copy_backward(cd.begin(), cd.end(), dest.end());

  VERIFY( equal(dest.begin(), dest.end(), cd.begin()) );
}

void test03()
{
  using namespace std;

  std::deque<int> d;
  for (int i = 0; i != 1024; ++i)
    d.push_back(i);

  d.pop_front();
  d.pop_back();

  std::vector<int> dest(d.size(), 0);

  std::copy_backward(d.begin(), d.end(), dest.end());
  VERIFY( std::equal(dest.begin(), dest.end(), d.begin()) );
}

void test04()
{
  using namespace std;

  std::vector<int> v;
  for (int i = 0; i != 1024; ++i)
    v.push_back(i);

  std::deque<int> dest(v.size() - 10, 0);

  std::copy_backward(v.begin() + 5, v.end() - 5, dest.end());
  VERIFY( std::equal(v.begin() + 5, v.end() - 5, dest.begin()) );
}

void test05()
{
  using namespace std;

  std::list<int> l;
  for (int i = 0; i != 1024; ++i)
    l.push_back(i);

  std::deque<int> dest(l.size(), 0);

  std::copy_backward(l.begin(), l.end(), dest.end());
  VERIFY( std::equal(dest.begin(), dest.end(), l.begin()) );
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
