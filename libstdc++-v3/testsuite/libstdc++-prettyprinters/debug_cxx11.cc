// { dg-do run { target c++11 } }
// { dg-options "-g -O0" }

// Copyright (C) 2016-2025 Free Software Foundation, Inc.
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

#include <forward_list>
#include <unordered_map>
#include <unordered_set>
#include <iostream>

int
main()
{
  std::forward_list<std::string> flst;
  std::forward_list<std::string>::iterator flstiter0;
// { dg-final { note-test flstiter0 {non-dereferenceable iterator for std::forward_list}} }
  flst.push_front("dum");
  std::forward_list<std::string>::iterator flstiter1 = flst.begin();
// { dg-final { note-test *flstiter1 {"dum"}} }
  flst.push_front("dee");
  std::forward_list<std::string>::iterator flstiter2 = flst.begin();
// { dg-final { note-test *flstiter2 {"dee"}} }
// { dg-final { note-test flst {std::__debug::forward_list = {[0] = "dee", [1] = "dum"}} } }

  std::forward_list<std::string>::const_iterator flstciter = flst.begin();
// { dg-final { note-test *flstciter {"dee"}} }

  std::unordered_map<std::string, int> um{ {"zardoz", 23} };
// { dg-final { note-test um {std::__debug::unordered_map with 1 element = {["zardoz"] = 23}} } }

  std::unordered_map<std::string, int>::iterator umiter = um.begin();
// { dg-final { note-test umiter->first {"zardoz"} } }

  std::unordered_set<std::string> us{"barrel"};
// { dg-final { note-test us {std::__debug::unordered_set with 1 element = {[0] = "barrel"}} } }

  std::unordered_set<std::string>::const_iterator usciter = us.begin();
// { dg-final { note-test *usciter {"barrel"} } }

  // N.B. printers.py does not define printers for the iterator types
  // that belong to C++11 containers, so tests above dereference the
  // iterators, and to make that work we need to ensure the operator
  // definitions are in the debug info:
  std::string tem;
  tem = *flstiter1;
  tem = *flstciter;
  tem = umiter->first;
  tem = *usciter;

  std::cout << "\n";
  return 0;			// Mark SPOT
}

// { dg-final { gdb-test SPOT } }
