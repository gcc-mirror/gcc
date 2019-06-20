// If you modify this, please update simple.cc and debug.cc as well.

// { dg-do run { target c++11 } }
// { dg-options "-g -O0" }

// Copyright (C) 2011-2019 Free Software Foundation, Inc.
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

// Type printers only recognize the old std::string for now.
#define _GLIBCXX_USE_CXX11_ABI 0

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
// { dg-final { regexp-test bs {std::(__debug::)?bitset = {\[0\] = 1, \[5\] = 1, \[7\] = 1}} } }

  std::deque<std::string> deq;
  deq.push_back("one");
  deq.push_back("two");
// { dg-final { regexp-test deq {std::(__debug::)?deque with 2 elements = {"one", "two"}} } }

  std::deque<std::string>::iterator deqiter = deq.begin();
// { dg-final { note-test deqiter {"one"} } }

  std::deque<int>::iterator deqiter0;
// { dg-final { note-test deqiter0 {non-dereferenceable iterator for std::deque} } }

  std::list<std::string> lst;
  lst.push_back("one");
  lst.push_back("two");
// { dg-final { regexp-test lst {std::(__debug::)?list = {\[0\] = "one", \[1\] = "two"}} } }

  std::list<std::string>::iterator lstiter = lst.begin();
  tem = *lstiter;
// { dg-final { note-test lstiter {"one"}} }

  std::list<std::string>::const_iterator lstciter = lst.begin();
  tem = *lstciter;
// { dg-final { note-test lstciter {"one"}} }

  std::list<int>::iterator lstiter0;
// { dg-final { note-test lstiter0 {non-dereferenceable iterator for std::list} } }

  std::map<std::string, int> mp;
  mp["zardoz"] = 23;
// { dg-final { regexp-test mp {std::(__debug::)?map with 1 element = {\["zardoz"\] = 23}} } }

  std::map<std::string, int>::iterator mpiter = mp.begin();
// { dg-final { note-test mpiter {{first = "zardoz", second = 23}} } }

  std::map<std::string, int>::iterator mpiter0;
// { dg-final { note-test mpiter0 {non-dereferenceable iterator for associative container} } }

  // PR 67440
  const std::set<int> const_intset = {2, 3};
// { dg-final { regexp-test const_intset {std::(__debug::)?set with 2 elements = {\[0\] = 2, \[1\] = 3}} } }

  std::set<std::string> sp;
  sp.insert("clownfish");
  sp.insert("barrel");
// { dg-final { regexp-test sp {std::(__debug::)?set with 2 elements = {\[0\] = "barrel", \[1\] = "clownfish"}} } }

  std::set<std::string>::const_iterator spciter = sp.begin();
// { dg-final { note-test spciter {"barrel"} } }

  std::set<int>::iterator spiter0;
// { dg-final { note-test spiter0 {non-dereferenceable iterator for associative container} } }

  std::vector<int> v;
  v.push_back(1);
  v.push_back(2);
  v.erase(v.begin());
// { dg-final { regexp-test v {std::(__debug::)?vector of length 1, capacity 2 = \\{2\\}} } }
  std::vector<int>::iterator viter3 = v.begin();
// { dg-final { note-test viter3 {2} } }

  std::vector<int>::iterator viter0;
// { dg-final { note-test viter0 {non-dereferenceable iterator for std::vector} } }

  std::vector<bool> vb;
  vb.reserve(100);
  vb.push_back(true);
  vb.push_back(true);
  vb.push_back(true);
  vb.push_back(false);
  vb.push_back(false);
  vb.push_back(true);
  vb.erase(vb.begin());
// { dg-final { regexp-test vb {std::(__debug::)?vector<bool> of length 5, capacity 128 = \\{true, true, false, false, true\\}} } }

  __gnu_cxx::slist<int> sll;
  sll.push_front(23);
  sll.push_front(47);
// { dg-final { note-test sll {__gnu_cxx::slist = {[0] = 47, [1] = 23}} } }

  __gnu_cxx::slist<int>::iterator slliter = sll.begin();
// { dg-final { note-test slliter {47} } }

  __gnu_cxx::slist<int>::iterator slliter0;
// { dg-final { note-test slliter0 {non-dereferenceable iterator for __gnu_cxx::slist} } }

  std::cout << "\n";
  return 0;			// Mark SPOT
}

// { dg-final { gdb-test SPOT } }
