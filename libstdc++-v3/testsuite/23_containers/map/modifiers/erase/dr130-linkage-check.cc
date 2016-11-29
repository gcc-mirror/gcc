// { dg-do run { target c++11 } }
// { dg-require-effective-target ia32 }
// { dg-require-sharedlib "" }
// { dg-options "-fno-inline ./testsuite_shared.so" }
// 2013-06-03  Benjamin Kosnik  <bkoz@redhat..com>
//
// Copyright (C) 2013-2016 Free Software Foundation, Inc.
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

#include <testsuite_containers.h>

int main()
{
  typedef std::map<int, int>		container_type;

  {
    container_type s { {0,0} , {1,1} , {2,2} };
    __gnu_test::linkage_check_cxx98_cxx11_erase(s);
  }

  {
    container_type s { {0,0} , {1,1} , {2,2} };
    __gnu_test::linkage_check_cxx98_cxx11_erase_iterators(s);
  }

  return 0;
}
