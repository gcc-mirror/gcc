// { dg-do compile }

// Copyright (C) 2000, 2001, 2002, 2004, 2005 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// 23.3.4 template class multiset negative tests
// 2000-09-07 bgarcia@laurelnetworks.com

#include <set>
#include <string>

// libstdc++/86: map & set iterator comparisons are not type-safe
void test01()
{
  bool test __attribute__((unused)) = true;

  std::set<unsigned int> setByIndex;
  std::set<std::string> setByName;
  
  std::set<unsigned int>::iterator itr(setByIndex.begin());
  
  // NB: it's not setByIndex!!
  test &= itr != setByName.end(); // { dg-error "no" } 
  test &= itr == setByName.end(); // { dg-error "no" } 
}

// { dg-error "candidates are" "" { target *-*-* } 285 }
// { dg-error "candidates are" "" { target *-*-* } 289 }
