// 2005-02-25  Paolo Carlini  <pcarlini@suse.de>
//
// Copyright (C) 2005 Free Software Foundation, Inc.
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

// 4.5.2 Composite type traits

#include <tr1/type_traits>
#include <testsuite_hooks.h>
#include <testsuite_tr1.h>

void test01()
{
  bool test __attribute__((unused)) = true;
  using std::tr1::__is_union_or_class;
  using namespace __gnu_test;

  // Positive tests.
  VERIFY( (test_category<__is_union_or_class, UnionType>(true)) );
  VERIFY( (test_category<__is_union_or_class, ClassType>(true)) );
  VERIFY( (test_category<__is_union_or_class, DerivedType>(true)) );
  VERIFY( (test_category<__is_union_or_class, ConvType>(true)) );
  VERIFY( (test_category<__is_union_or_class, AbstractClass>(true)) );
  VERIFY( (test_category<__is_union_or_class, PolymorphicClass>(true)) );
  VERIFY( (test_category<__is_union_or_class, DerivedPolymorphic>(true)) );

  // Negative tests.
  VERIFY( (test_category<__is_union_or_class, void>(false)) );
  VERIFY( (test_category<__is_union_or_class, int>(false)) );
  VERIFY( (test_category<__is_union_or_class, float>(false)) );
  VERIFY( (test_category<__is_union_or_class, int[2]>(false)) );
  VERIFY( (test_category<__is_union_or_class, int*>(false)) );
  VERIFY( (test_category<__is_union_or_class, int(*)(int)>(false)) );
  VERIFY( (test_category<__is_union_or_class, float&>(false)) );
  VERIFY( (test_category<__is_union_or_class, float(&)(float)>(false)) );
  VERIFY( (test_category<__is_union_or_class, int (ClassType::*)>(false)) );
  VERIFY( (test_category<__is_union_or_class,
	   int (ClassType::*) (int)>(false)) );
  VERIFY( (test_category<__is_union_or_class, int (int)>(false)) );
  VERIFY( (test_category<__is_union_or_class, EnumType>(false)) );
}

int main()
{
  test01();
  return 0;
}
