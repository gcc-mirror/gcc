// 2007-04-08  Paolo Carlini  <pcarlini@suse.de>
//
// Copyright (C) 2007-2021 Free Software Foundation, Inc.
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

// 4.5.1 Primary type categories

#include <tr1/type_traits>
#include <testsuite_hooks.h>
#include <testsuite_tr1.h>

void test01()
{
  using std::tr1::is_class;
  using namespace __gnu_test;

  // Positive tests.
  VERIFY( (test_category<is_class, ClassType>(true)) );
  VERIFY( (test_category<is_class, DerivedType>(true)) );
  VERIFY( (test_category<is_class, ConvType>(true)) );
  VERIFY( (test_category<is_class, AbstractClass>(true)) );
  VERIFY( (test_category<is_class, PolymorphicClass>(true)) );
  VERIFY( (test_category<is_class, DerivedPolymorphic>(true)) );

  // Negative tests.
  VERIFY( (test_category<is_class, UnionType>(false)) );
  VERIFY( (test_category<is_class, void>(false)) );
  VERIFY( (test_category<is_class, int>(false)) );
  VERIFY( (test_category<is_class, float>(false)) );
  VERIFY( (test_category<is_class, int[2]>(false)) );
  VERIFY( (test_category<is_class, int*>(false)) );
  VERIFY( (test_category<is_class, int(*)(int)>(false)) );
  VERIFY( (test_category<is_class, float&>(false)) );
  VERIFY( (test_category<is_class, float(&)(float)>(false)) );
  VERIFY( (test_category<is_class, int (ClassType::*)>(false)) );
  VERIFY( (test_category<is_class, int (ClassType::*) (int)>(false)) );
  VERIFY( (test_category<is_class, int (int)>(false)) );
  VERIFY( (test_category<is_class, EnumType>(false)) );
}

int main()
{
  test01();
  return 0;
}
