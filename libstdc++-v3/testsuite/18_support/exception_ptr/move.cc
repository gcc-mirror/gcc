// { dg-do run { target c++11 } }

// Copyright (C) 2009-2019 Free Software Foundation, Inc.
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

#include <exception>
#include <utility>
#include <testsuite_hooks.h>

// Verify move construction and assignment are efficient and do not copy.
// This behaviour is a GNU extension provided for efficiency.
void test01()
{
  bool test = true;
 
  std::exception_ptr p1 = std::make_exception_ptr(test);
  std::exception_ptr p2 = std::move(p1);
  VERIFY( p1 == 0 );
  VERIFY( !(p2 == 0) );

  p1 = std::move(p2);
  VERIFY( !(p1 == 0) );
  VERIFY( p2 == 0 );
}

int main()
{
  test01();
  return 0;
}
