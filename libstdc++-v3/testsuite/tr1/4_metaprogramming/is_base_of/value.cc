// 2005-03-04  Paolo Carlini  <pcarlini@suse.de>
//
// Copyright (C) 2005-2014 Free Software Foundation, Inc.
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

// 4.6 Relationships between types

#include <tr1/type_traits>
#include <testsuite_hooks.h>
#include <testsuite_tr1.h>

class HiddenCons
{
  HiddenCons();
  HiddenCons(const HiddenCons&);
};

class DerivedHiddenCons
: private HiddenCons
{
  DerivedHiddenCons();
  DerivedHiddenCons(const DerivedHiddenCons&);
};

class MultiDerivedHiddenCons
: private HiddenCons, private __gnu_test::ClassType 
{
  MultiDerivedHiddenCons();
  MultiDerivedHiddenCons(const MultiDerivedHiddenCons&);
};

void test01()
{
  bool test __attribute__((unused)) = true;
  using std::tr1::is_base_of;
  using namespace __gnu_test;

  // Positive tests.
  VERIFY( (test_relationship<is_base_of, int, int>(true)) );
  VERIFY( (test_relationship<is_base_of, EnumType, EnumType>(true)) );
  VERIFY( (test_relationship<is_base_of, UnionType, UnionType>(true)) );
  VERIFY( (test_relationship<is_base_of, AbstractClass,
	   AbstractClass>(true)) );
  VERIFY( (test_relationship<is_base_of, ClassType, DerivedType>(true)) );
  VERIFY( (test_relationship<is_base_of, ClassType, const DerivedType>(true)) );
  VERIFY( (test_relationship<is_base_of, volatile ClassType,
	   volatile DerivedType>(true)) );  
  VERIFY( (test_relationship<is_base_of, PolymorphicClass,
	   DerivedPolymorphic>(true)) );
  VERIFY( (test_relationship<is_base_of, HiddenCons,
	   DerivedHiddenCons>(true)) );
  VERIFY( (test_relationship<is_base_of, HiddenCons,
	   MultiDerivedHiddenCons>(true)) );
  VERIFY( (test_relationship<is_base_of, ClassType,
	   MultiDerivedHiddenCons>(true)) );

  // Negative tests.
  VERIFY( (test_relationship<is_base_of, int, const int>(false)) );
  VERIFY( (test_relationship<is_base_of, volatile UnionType,
	   UnionType>(false)) );
  VERIFY( (test_relationship<is_base_of, int&, ClassType>(false)) );
  VERIFY( (test_relationship<is_base_of, AbstractClass, ClassType>(false)) );
  VERIFY( (test_relationship<is_base_of, ClassType, AbstractClass>(false)) );  
  VERIFY( (test_relationship<is_base_of, DerivedType, ClassType>(false)) );
  VERIFY( (test_relationship<is_base_of, DerivedPolymorphic,
	   PolymorphicClass>(false)) );
  VERIFY( (test_relationship<is_base_of, DerivedHiddenCons,
	   HiddenCons>(false)) );
  VERIFY( (test_relationship<is_base_of, MultiDerivedHiddenCons,
	   HiddenCons>(false)) );
  VERIFY( (test_relationship<is_base_of, MultiDerivedHiddenCons,
	   ClassType>(false)) );
}

int main()
{
  test01();
  return 0;
}
