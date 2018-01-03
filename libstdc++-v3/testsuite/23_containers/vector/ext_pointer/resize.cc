
// Copyright (C) 2008-2018 Free Software Foundation, Inc.
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
#include <stdexcept>
#include <testsuite_allocator.h>
#include <testsuite_hooks.h>
#include <ext/extptr_allocator.h>


void test01()
{
  // non POD types
  std::vector<int, __gnu_cxx::_ExtPtr_allocator<int> > vec01;
  typedef std::vector<int, __gnu_cxx::_ExtPtr_allocator<int> >::size_type size_type;

  VERIFY(vec01.empty());

  const int A[] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };    

  // Test resize of the vector based on reserve
  size_type sz01 = vec01.capacity();
  vec01.reserve(100);
  size_type sz02 = vec01.capacity();
  VERIFY(sz02 >= sz01);

  // grow/shrink
  vec01.assign( A, A+10 );
  sz01 = vec01.size() + 100;
  vec01.resize(sz01);
  sz02 = vec01.size();
  VERIFY(sz01 == sz02);
  VERIFY(std::equal(vec01.begin(), vec01.begin()+10, A));
  
  sz01 = vec01.size() - 100;
  vec01.resize(sz01);
  sz02 = vec01.size();
  VERIFY(sz01 == sz02);
  VERIFY(std::equal(vec01.begin(), vec01.end(), A));
}

int main()
{
  test01();
  return 0;
}
