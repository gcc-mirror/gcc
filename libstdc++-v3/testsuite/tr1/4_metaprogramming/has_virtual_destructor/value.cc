// 2007-04-08  Paolo Carlini  <pcarlini@suse.de>
//
// Copyright (C) 2007-2024 Free Software Foundation, Inc.
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

// 4.5.3 Type properties

#include <tr1/type_traits>
#include <iostream>
#include <testsuite_hooks.h>
#include <testsuite_tr1.h>

void test01()
{
  using std::tr1::has_virtual_destructor;
  using namespace __gnu_test;

  // Positive tests.
  VERIFY( (test_category<has_virtual_destructor,
	   VirtualDestructorClass>(true)) );
  VERIFY( (test_category<has_virtual_destructor, std::iostream>(true)) );
  VERIFY( (test_category<has_virtual_destructor, std::streambuf>(true)) );

  // Negative tests.
  VERIFY( (test_category<has_virtual_destructor, PolymorphicClass>(false)) );
  VERIFY( (test_category<has_virtual_destructor, DerivedPolymorphic>(false)) );
  VERIFY( (test_category<has_virtual_destructor, AbstractClass>(false)) );
  VERIFY( (test_category<has_virtual_destructor, void>(false)) );
  VERIFY( (test_category<has_virtual_destructor, int (int)>(false)) );
  VERIFY( (test_category<has_virtual_destructor, int&>(false)) );
  VERIFY( (test_category<has_virtual_destructor, EnumType>(false)) );
  VERIFY( (test_category<has_virtual_destructor, ClassType>(false)) );
  VERIFY( (test_category<has_virtual_destructor, DerivedType>(false)) );
}

int main()
{
  test01();
  return 0;
}
