// { dg-options "-std=gnu++0x" }

// 2010-09-22  Paolo Carlini  <paolo.carlini@oracle.com>
//
// Copyright (C) 2010-2013 Free Software Foundation, Inc.
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

#include <typeindex>
#include <testsuite_hooks.h>

void test01()
{
  bool test __attribute__((unused)) = true;
  using namespace std;

  class Abraca { };
  Abraca a1, a2_;
  const Abraca a2 = a2_;

  const type_info& to1 = typeid(int);
  const type_index ti1(to1);
  VERIFY( ti1.hash_code() == to1.hash_code() );

  const type_info& to2 = typeid(double);
  const type_index ti2(to2);
  VERIFY( ti2.hash_code() == to2.hash_code() );

  const type_info& to3 = typeid(Abraca);
  const type_index ti3(to3);
  VERIFY( ti3.hash_code() == to3.hash_code() );

  const type_info& to4 = typeid(const Abraca);
  const type_index ti4(to4);
  VERIFY( ti4.hash_code() == to4.hash_code() );

  const type_info& to5 = typeid(const Abraca&);
  const type_index ti5(to5);
  VERIFY( ti5.hash_code() == to5.hash_code() );

  const type_info& to6 = typeid(a1);
  const type_index ti6(to6);
  VERIFY( ti6.hash_code() == to6.hash_code() );

  const type_info& to7 = typeid(a2);
  const type_index ti7(to7);
  VERIFY( ti7.hash_code() == to7.hash_code() );
}

int main()
{
  test01();
  return 0;
}
