// { dg-do run { target c++11 } }
// { dg-options "-g -O0" }

// Copyright (C) 2018-2023 Free Software Foundation, Inc.
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

#include <iostream>
#include <list>
#include <memory>
#include <set>
#include <string>
#include <vector>

template<class T>
void
placeholder(const T *s)
{
  std::cout << (void *) s;
}

int
main()
{
  using namespace std;
  // Ensure debug info for std::string is issued in the local
  // translation unit, so that GDB won't pick up any alternate
  // std::string notion that might be present in libstdc++.so.
  string bah = "hi";
  (void)bah;
  unique_ptr<vector<unique_ptr<vector<int>*>>> p1;
  unique_ptr<vector<unique_ptr<set<int>*>>[]> p2;
  unique_ptr<set<unique_ptr<vector<int>*>>[10]> p3;
  unique_ptr<vector<unique_ptr<list<std::string>[]>>[99]> p4;
  // { dg-final { whatis-regexp-test p1 "std::unique_ptr<std::(__debug::)?vector<std::unique_ptr<std::(__debug::)?vector<int>\\*>>>" } }
  // { dg-final { whatis-regexp-test p2 "std::unique_ptr<std::(__debug::)?vector<std::unique_ptr<std::(__debug::)?set<int>\\*>>\\\[\\\]>" } }
  // { dg-final { whatis-regexp-test p3 "std::unique_ptr<std::(__debug::)?set<std::unique_ptr<std::(__debug::)?vector<int>\\*>>\\\[10\\\]>" } }
  // { dg-final { whatis-regexp-test p4 "std::unique_ptr<std::(__debug::)?vector<std::unique_ptr<std::(__debug::)?list<std::string>\\\[\\\]>>\\\[99\\\]>" } }

  placeholder(&p1);		// Mark SPOT
  placeholder(&p2);
  placeholder(&p3);
  placeholder(&p4);

  std::cout << "\n";
  return 0;
}

// { dg-final { gdb-test SPOT } }
