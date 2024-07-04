// Copyright (C) 2008-2024 Free Software Foundation, Inc.
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

// NB: This issue affected only debug-mode.

#include <list>
#include <functional>

// libstdc++/35969
void test01()
{
  {
    std::list<int> list1;
    std::list<int> list2;

    for(int i = 0; i < 10; ++i)
      {
	list1.push_back(i);
	list2.push_back(10 - i);
      }

    list1.sort();
    list2.sort();

    std::list<int>::iterator node_of_interest = list2.begin();

    list1.splice(list1.begin(), list2, node_of_interest);
    list2.splice(list2.begin(), list1, node_of_interest);

    list1.merge(list2);

    list2.splice(list2.begin(), list1, node_of_interest);
  }

  {
    std::list<int> list1;
    std::list<int> list2;

    for(int i = 0; i < 10; ++i)
      {
	list1.push_back(i);
	list2.push_back(10 - i);
      }

    list1.sort();
    list2.sort();

    std::list<int>::iterator node_of_interest = list2.begin();

    list1.splice(list1.begin(), list2, node_of_interest);
    list2.splice(list2.begin(), list1, node_of_interest);

    list1.merge(list2, std::less<int>());

    list2.splice(list2.begin(), list1, node_of_interest);
  }
}

int main()
{
  test01();
  return 0;
}
