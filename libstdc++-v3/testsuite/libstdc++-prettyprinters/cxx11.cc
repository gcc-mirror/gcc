// { dg-do run }
// { dg-options "-std=gnu++11 -g -O0" }

// Copyright (C) 2011-2014 Free Software Foundation, Inc.
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
#include <memory>
#include <iostream>

typedef std::tuple<int, int> ExTuple;

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

struct datum
{
  std::string s;
  int i;
};

std::unique_ptr<datum> global;

int
main()
{
  std::forward_list<int> efl;
// { dg-final { note-test efl "empty std::forward_list" } }

  std::forward_list<int> &refl = efl;
// { dg-final { note-test refl "empty std::forward_list" } }

  std::forward_list<int> fl;
  fl.push_front(2);
  fl.push_front(1);
// { dg-final { note-test fl {std::forward_list = {[0] = 1, [1] = 2}} } }

  std::forward_list<int> &rfl = fl;
// { dg-final { note-test rfl {std::forward_list = {[0] = 1, [1] = 2}} } }

  std::unordered_map<int, std::string> eum;
// { dg-final { note-test eum "std::unordered_map with 0 elements" } }
  std::unordered_map<int, std::string> &reum = eum;
// { dg-final { note-test reum "std::unordered_map with 0 elements" } }

  std::unordered_multimap<int, std::string> eumm;
// { dg-final { note-test eumm "std::unordered_multimap with 0 elements" } }
  std::unordered_multimap<int, std::string> &reumm = eumm;
// { dg-final { note-test reumm "std::unordered_multimap with 0 elements" } }

  std::unordered_set<int> eus;
// { dg-final { note-test eus "std::unordered_set with 0 elements" } }
  std::unordered_set<int> &reus = eus;
// { dg-final { note-test reus "std::unordered_set with 0 elements" } }

  std::unordered_multiset<int> eums;
// { dg-final { note-test eums "std::unordered_multiset with 0 elements" } }
  std::unordered_multiset<int> &reums = eums;
// { dg-final { note-test reums "std::unordered_multiset with 0 elements" } }

  std::unordered_map<int, std::string> uom;
  uom[5] = "three";
  uom[3] = "seven";
// { dg-final { note-test uom {std::unordered_map with 2 elements = {[3] = "seven", [5] = "three"}} } }

  std::unordered_map<int, std::string> &ruom = uom;
// { dg-final { note-test ruom {std::unordered_map with 2 elements = {[3] = "seven", [5] = "three"}} } }

  std::unordered_multimap<int, std::string> uomm;
  uomm.insert(std::pair<int, std::string> (5, "three"));
  uomm.insert(std::pair<int, std::string> (5, "seven"));
// { dg-final { note-test uomm {std::unordered_multimap with 2 elements = {[5] = "seven", [5] = "three"}} } }
  std::unordered_multimap<int, std::string> &ruomm = uomm;
// { dg-final { note-test ruomm {std::unordered_multimap with 2 elements = {[5] = "seven", [5] = "three"}} } }

  std::unordered_set<int> uos;
  uos.insert(5);
// { dg-final { note-test uos {std::unordered_set with 1 elements = {[0] = 5}} } }
  std::unordered_set<int> &ruos = uos;
// { dg-final { note-test ruos {std::unordered_set with 1 elements = {[0] = 5}} } }

  std::unordered_multiset<int> uoms;
  uoms.insert(5);
// { dg-final { note-test uoms {std::unordered_multiset with 1 elements = {[0] = 5}} } }
  std::unordered_multiset<int> &ruoms = uoms;
// { dg-final { note-test ruoms {std::unordered_multiset with 1 elements = {[0] = 5}} } }

  std::unique_ptr<datum> uptr (new datum);
  uptr->s = "hi bob";
  uptr->i = 23;
// { dg-final { regexp-test uptr {std::unique_ptr.datum. containing 0x.*} } }
  std::unique_ptr<datum> &ruptr = uptr;
// { dg-final { regexp-test ruptr {std::unique_ptr.datum. containing 0x.*} } }

  ExTuple tpl(6,7);
// { dg-final { note-test tpl {std::tuple containing = {[1] = 6, [2] = 7}} } }  
  ExTuple &rtpl = tpl;
// { dg-final { note-test rtpl {std::tuple containing = {[1] = 6, [2] = 7}} } }   
  placeholder(""); // Mark SPOT
  use(efl);
  use(fl);
  use(eum);
  use(eumm);
  use(eus);
  use(eums);
  use(uoms);
  use(uptr->s);

  return 0;
}

// { dg-final { gdb-test SPOT } }
