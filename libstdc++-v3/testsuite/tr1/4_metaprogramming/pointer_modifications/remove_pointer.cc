// 2004-12-12  Paolo Carlini  <pcarlini@suse.de>
//
// Copyright (C) 2004 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// 4.7.2 Reference modifications

#include <tr1/type_traits>
#include <testsuite_hooks.h>
#include <testsuite_tr1.h>

void test01()
{
  bool test __attribute__((unused)) = true;
  using std::tr1::remove_pointer;
  using std::tr1::is_same;
  using namespace __gnu_test;

  VERIFY( (is_same<remove_pointer<int*>::type, int>::value) );
  VERIFY( (is_same<remove_pointer<int>::type, int>::value) );
  VERIFY( (is_same<remove_pointer<const int*>::type, const int>::value) );
  VERIFY( (is_same<remove_pointer<int**>::type, int*>::value) );
  VERIFY( (is_same<remove_pointer<ClassType*>::type, ClassType>::value) );
  VERIFY( (is_same<remove_pointer<ClassType>::type, ClassType>::value) );
}

int main()
{
  test01();
  return 0;
}
