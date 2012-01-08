// { dg-do run }
// { dg-options "-std=gnu++11 -g" }

// Copyright (C) 2011 Free Software Foundation, Inc.
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

#include <forward_list>
#include <unordered_map>
#include <unordered_set>
#include <string>
#include <iostream>

template<class T>
void
placeholder(const T &s)
{
  std::cout << s;
}

template<class T, class S>
void
placeholder(const std::pair<T,S> &s)
{
  std::cout << s.first;
}

template<class T>
void
use(const T &container)
{
  for (typename T::const_iterator i = container.begin();
       i != container.end();
       ++i)
    placeholder(*i);
}

int
main()
{
  std::forward_list<int> efl;
// { dg-final { note-test efl "empty std::forward_list" } }

  std::forward_list<int> fl;
  fl.push_front(2);
  fl.push_front(1);
// { dg-final { note-test fl {std::forward_list = {[0] = 1, [1] = 2}} } }

  std::unordered_map<int, std::string> eum;
// { dg-final { note-test eum "std::unordered_map with 0 elements" } }
  std::unordered_multimap<int, std::string> eumm;
// { dg-final { note-test eumm "std::unordered_multimap with 0 elements" } }
  std::unordered_set<int> eus;
// { dg-final { note-test eus "std::unordered_set with 0 elements" } }
  std::unordered_multiset<int> eums;
// { dg-final { note-test eums "std::unordered_multiset with 0 elements" } }

  placeholder(""); // Mark SPOT
  use(efl);
  use(fl);
  use(eum);
  use(eumm);
  use(eus);
  use(eums);

  return 0;
}

// { dg-final { gdb-test SPOT } }
