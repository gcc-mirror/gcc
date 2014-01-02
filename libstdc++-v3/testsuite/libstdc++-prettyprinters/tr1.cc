// { dg-do run }
// { dg-options "-g" }

// Copyright (C) 2013-2014 Free Software Foundation, Inc.
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

#include <tr1/unordered_map>
#include <tr1/unordered_set>
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
  std::tr1::unordered_map<int, std::string> eum;
// { dg-final { note-test eum "std::tr1::unordered_map with 0 elements" } }
  std::tr1::unordered_multimap<int, std::string> eumm;
// { dg-final { note-test eumm "std::tr1::unordered_multimap with 0 elements" } }
  std::tr1::unordered_set<int> eus;
// { dg-final { note-test eus "std::tr1::unordered_set with 0 elements" } }
  std::tr1::unordered_multiset<int> eums;
// { dg-final { note-test eums "std::tr1::unordered_multiset with 0 elements" } }

  std::tr1::unordered_map<int, std::string> uom;
  uom[5] = "three";
  uom[3] = "seven";
// { dg-final { note-test uom {std::tr1::unordered_map with 2 elements = {[3] = "seven", [5] = "three"}} } }

  std::tr1::unordered_multimap<int, std::string> uomm;
  uomm.insert(std::pair<int, std::string> (5, "three"));
  uomm.insert(std::pair<int, std::string> (5, "seven"));
// { dg-final { note-test uomm {std::tr1::unordered_multimap with 2 elements = {[5] = "three", [5] = "seven"}} } }

  std::tr1::unordered_set<int> uos;
  uos.insert(5);
// { dg-final { note-test uos {std::tr1::unordered_set with 1 elements = {[0] = 5}} } }

  std::tr1::unordered_multiset<int> uoms;
  uoms.insert(5);
// { dg-final { note-test uoms {std::tr1::unordered_multiset with 1 elements = {[0] = 5}} } }

  placeholder(""); // Mark SPOT
  use(eum);
  use(eumm);
  use(eus);
  use(eums);
  use(uoms);

  return 0;
}

// { dg-final { gdb-test SPOT } }
