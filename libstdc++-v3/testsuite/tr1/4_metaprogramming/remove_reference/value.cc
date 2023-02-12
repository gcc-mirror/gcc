// 2004-12-08  Paolo Carlini  <pcarlini@suse.de>
//
// Copyright (C) 2004-2023 Free Software Foundation, Inc.
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

// 4.7.2 Reference modifications

#include <tr1/type_traits>
#include <testsuite_hooks.h>
#include <testsuite_tr1.h>

void test01()
{
  using std::tr1::remove_reference;
  using std::tr1::is_same;
  using namespace __gnu_test;

  VERIFY( (is_same<remove_reference<int&>::type, int>::value) );
  VERIFY( (is_same<remove_reference<int>::type, int>::value) );
  VERIFY( (is_same<remove_reference<const int&>::type, const int>::value) );
  VERIFY( (is_same<remove_reference<int*&>::type, int*>::value) );
  VERIFY( (is_same<remove_reference<ClassType&>::type, ClassType>::value) );
  VERIFY( (is_same<remove_reference<ClassType>::type, ClassType>::value) );
}

int main()
{
  test01();
  return 0;
}
