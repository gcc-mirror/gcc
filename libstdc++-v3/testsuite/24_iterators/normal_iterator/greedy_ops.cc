// { dg-do compile }
// Copyright (C) 2010-2014 Free Software Foundation, Inc.
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

#include <iterator>
#include <testsuite_greedy_ops.h>

namespace greedy_ops
{
  struct C
  {
    typedef X* pointer;
  };
}

void test01()
{
  typedef __gnu_cxx::__normal_iterator<greedy_ops::X*,
				       greedy_ops::C> iterator_type;

  iterator_type it(0);
  
  it == it;
  it != it;
  it < it;
  it <= it;
  it > it;
  it >= it;
  it - it;
  it + 1;
  1 + it;
}

int main() 
{ 
  test01();
  return 0;
}
