// { dg-do run }
// { dg-options "-g -O0" }

// Copyright (C) 2011-2013 Free Software Foundation, Inc.
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

#define _GLIBCXX_DEBUG

#include <string>
#include <deque>
#include <bitset>
#include <iostream>
#include <list>
#include <map>
#include <set>
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
// { dg-final { note-test mp {std::__debug::map with 1 elements = {["zardoz"] = 23}} } }

  std::map<std::string, int>::iterator mpiter = mp.begin();
// { dg-final { note-test mpiter {{first = "zardoz", second = 23}} } }

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

  __gnu_cxx::slist<int>::iterator slliter = sll.begin();
// { dg-final { note-test slliter {47} } }

  return 0;			// Mark SPOT
}

// { dg-final { gdb-test SPOT } }
