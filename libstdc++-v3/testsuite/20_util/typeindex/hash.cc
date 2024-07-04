// { dg-do run { target c++11 } }
// { dg-require-effective-target rtti }

// 2010-09-22  Paolo Carlini  <paolo.carlini@oracle.com>
//
// Copyright (C) 2010-2024 Free Software Foundation, Inc.
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
  using namespace std;

  class Abraca { };
  Abraca a1, a2_;
  const Abraca a2 = a2_;

  const type_index ti1(typeid(int));
  VERIFY( hash<type_index>()(ti1) == ti1.hash_code() );

  const type_index ti2(typeid(double));
  VERIFY( hash<type_index>()(ti2) == ti2.hash_code() );

  const type_index ti3(typeid(Abraca));
  VERIFY( hash<type_index>()(ti3) == ti3.hash_code() );

  const type_index ti4(typeid(const Abraca));
  VERIFY( hash<type_index>()(ti4) == ti4.hash_code() );

  const type_index ti5(typeid(const Abraca&));
  VERIFY( hash<type_index>()(ti5) == ti5.hash_code() );

  const type_index ti6(typeid(a1));
  VERIFY( hash<type_index>()(ti6) == ti6.hash_code() );

  const type_index ti7(typeid(a2));
  VERIFY( hash<type_index>()(ti7) == ti7.hash_code() );
}

int main()
{
  test01();
  return 0;
}
