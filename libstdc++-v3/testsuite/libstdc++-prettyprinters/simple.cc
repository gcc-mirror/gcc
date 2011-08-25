// { dg-do run }
// { dg-options "-g" }

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

#include <string>
#include <deque>
#include <bitset>
#include <iostream>
#include <list>
#include <map>

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
  std::string str = "zardoz";
// { dg-final { note-test str "\"zardoz\"" } }

  std::bitset<10> bs;
  bs[0] = 1;
  bs[5] = 1;
  bs[7] = 1;
// { dg-final { note-test bs {std::bitset = {[0] = 1, [5] = 1, [7] = 1}} } }

  std::deque<std::string> deq;
  deq.push_back("one");
  deq.push_back("two");
// { dg-final { note-test deq {std::deque with 2 elements = {"one", "two"}} } }

  std::list<std::string> lst;
  lst.push_back("one");
  lst.push_back("two");
// { dg-final { note-test lst {std::list = {[0] = "one", [1] = "two"}} } }

  std::map<std::string, int> mp;
  mp["zardoz"] = 23;
// { dg-final { note-test mp {std::map with 1 elements = {["zardoz"] = 23}} } }

  placeholder(str); // Mark SPOT
  std::cout << bs;
  use(deq);
  use(lst);
  use(mp);

  return 0;
}

// { dg-final { gdb-test SPOT } }
