// 1999-05-07
// bkoz 

// Copyright (C) 1999-2018 Free Software Foundation, Inc.
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

// 23.2.4.2 vector capacity

#include <vector>
#include <stdexcept>
#include <testsuite_allocator.h>
#include <testsuite_hooks.h>
 
template<typename T>
  struct A { };

struct B { };

void test01()
{
  // non POD types
  std::vector< A<B> > vec01;
  typedef std::vector< A<B> >::size_type size_type;

  size_type sz01 = vec01.capacity();
  vec01.reserve(100);
  size_type sz02 = vec01.capacity();
  VERIFY( sz02 >= sz01 );
  
  sz01 = vec01.size() + 5;
  vec01.resize(sz01);
  sz02 = vec01.size();
  VERIFY( sz01 == sz02 );

  sz01 = vec01.size() - 5;
  vec01.resize(sz01);
  sz02 = vec01.size();
  VERIFY( sz01 == sz02 );
}

int main()
{
  test01();
  return 0;
}
