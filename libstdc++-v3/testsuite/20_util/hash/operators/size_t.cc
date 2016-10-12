// { dg-do run { target c++11 } }
// 2007-08-20  <benjamin@redhat.com>
//
// Copyright (C) 2007-2016 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include <functional>
#include <system_error>
#include <testsuite_hooks.h>

template<typename T>
  void
  do_test()
  {
    typedef T 				value_type;
    typedef std::hash<value_type> 	hash_type;
    using std::size_t;

    value_type v; // default initialized is fine, same value all that matters.
    hash_type h1;
    size_t r1 = size_t(h1(v));
    
    hash_type h2;
    size_t r2 = size_t(h2(v));

    VERIFY( r1 == r2 );
  }
  
void test01()
{
  do_test<std::error_code>();
}

int main()
{
  test01();
  return 0;
}
