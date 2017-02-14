// { dg-do run { target c++11 } }

// Copyright (C) 2008-2017 Free Software Foundation, Inc.
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


#include <vector>
#include <testsuite_hooks.h>
#include <ext/extptr_allocator.h>

// Ensures equivalence of iterators based on low-level comparison
// between const / non-const Pointer types.
void
test01()
{
  std::vector<int, __gnu_cxx::_ExtPtr_allocator<int> > v(7);
  VERIFY( v.cbegin() == v.begin() );
  VERIFY( v.cend() == v.end() );
  VERIFY( v.crbegin() == v.rbegin() );
  VERIFY( v.crend() == v.rend() );
  VERIFY( v.cbegin() != v.cend() );
  VERIFY( v.crbegin() != v.crend() );  
}

int main()
{
  test01();
  return 0;
}
