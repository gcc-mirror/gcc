// { dg-do run }
// { dg-options "-g -O0 -std=gnu++98" }

// Copyright (C) 2011-2021 Free Software Foundation, Inc.
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

#ifndef _GLIBCXX_DEBUG
# define _GLIBCXX_DEBUG
#endif

#include <string>
#include <deque>
#include <bitset>
#include <iostream>
#include <list>
#include <map>
#include <set>
#include <vector>
#include <ext/slist>

int
main()
{
  std::string tem;
  std::string str = "zardoz";
// { dg-final { note-test str "\"zardoz\"" } }

  std::bitset<10> bs;
  bs[0] = 1;
  bs[5] = 1;
  bs[7] = 1;
// { dg-final { note-test bs {std::__debug::bitset = {[0] = 1, [5] = 1, [7] = 1}} } }

  std::deque<std::string> deq;
  deq.push_back("one");
  deq.push_back("two");
// { dg-final { note-test deq {std::__debug::deque with 2 elements = {"one", "two"}} } }

  std::deque<std::string>::iterator deqiter = deq.begin();
// { dg-final { note-test deqiter {"one"} } }

  std::list<std::string> lst;
  lst.push_back("one");
  lst.push_back("two");
// { dg-final { note-test lst {std::__debug::list = {[0] = "one", [1] = "two"}} } }

  std::list<std::string>::iterator lstiter = lst.begin();
  tem = *lstiter;
// { dg-final { note-test lstiter {"one"}} }

  std::list<std::string>::const_iterator lstciter = lst.begin();
  tem = *lstciter;
// { dg-final { note-test lstciter {"one"}} }

  std::map<std::string, int> mp;
  mp["zardoz"] = 23;
// { dg-final { note-test mp {std::__debug::map with 1 element = {["zardoz"] = 23}} } }

  std::map<std::string, int>::iterator mpiter = mp.begin();
// { dg-final { note-test mpiter {{first = "zardoz", second = 23}} } }

  // PR 67440
  std::set<int> intset;
  intset.insert(2);
  intset.insert(3);
  const std::set<int> const_intset = intset;
// { dg-final { note-test const_intset {std::__debug::set with 2 elements = {[0] = 2, [1] = 3}} } }

  std::set<std::string> sp;
  sp.insert("clownfish");
  sp.insert("barrel");
// { dg-final { note-test sp {std::__debug::set with 2 elements = {[0] = "barrel", [1] = "clownfish"}} } }

  std::set<std::string>::const_iterator spciter = sp.begin();
// { dg-final { note-test spciter {"barrel"} } }

  __gnu_cxx::slist<int> sll;
  sll.push_front(23);
  sll.push_front(47);
// { dg-final { note-test sll {__gnu_cxx::slist = {[0] = 47, [1] = 23}} } }

  std::vector<int> v;
  v.push_back(1);
  v.push_back(2);
  std::vector<int>::iterator viter0;
// { dg-final { note-test viter0 {non-dereferenceable iterator for std::vector} } }
  std::vector<int>::iterator viter1 = v.begin();
  std::vector<int>::iterator viter2 = viter1 + 1;
  v.erase(viter1);
// { dg-final { note-test v {std::__debug::vector of length 1, capacity 2 = {2}} } }
// { dg-final { note-test viter1 {invalid iterator} } }
// { dg-final { note-test viter2 {invalid iterator} } }
  std::vector<int>::iterator viter3 = v.begin();
// { dg-final { note-test viter3 {2} } }

  __gnu_cxx::slist<int>::iterator slliter = sll.begin();
// { dg-final { note-test slliter {47} } }

  std::cout << "\n";
  return 0;			// Mark SPOT
}

// { dg-final { gdb-test SPOT } }
