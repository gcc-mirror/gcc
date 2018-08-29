// { dg-do run { target c++11 } }

// 2010-09-21  Paolo Carlini  <paolo.carlini@oracle.com>
//
// Copyright (C) 2010-2018 Free Software Foundation, Inc.
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

#include <typeinfo>
#include <testsuite_hooks.h>

void test01()
{
  using namespace std;

  class Abraca { };
  Abraca a1, a2_;

  typedef const Abraca cAbraca;
  cAbraca a2 = a2_;

  class Dabra { };
  Dabra d1;

  const type_info& to01 = typeid(int);
  const type_info& to02 = typeid(double);
  VERIFY( to01 != to02 );
  VERIFY( to01.hash_code() != to02.hash_code() );

  const type_info& to03 = typeid(a1);
  const type_info& to04 = typeid(a2);
  VERIFY( to03 == to04 );
  VERIFY( to03.hash_code() == to04.hash_code() );

  const type_info& to05 = typeid(Abraca);
  const type_info& to06 = typeid(cAbraca);
  VERIFY( to05 == to06 );
  VERIFY( to05.hash_code() == to06.hash_code() );

  const type_info& to07 = typeid(Abraca);
  const type_info& to08 = typeid(a2);
  VERIFY( to07 == to08 );
  VERIFY( to07.hash_code() == to08.hash_code() );

  const type_info& to09 = typeid(Abraca);
  const type_info& to10 = typeid(const Abraca&);
  VERIFY( to09 == to10 );
  VERIFY( to09.hash_code() == to10.hash_code() );

  const type_info& to11 = typeid(Abraca);
  const type_info& to12 = typeid(Dabra);
  VERIFY( to11 != to12 );
  VERIFY( to11.hash_code() != to12.hash_code() );

  const type_info& to13 = typeid(a1);
  const type_info& to14 = typeid(d1);
  VERIFY( to13 != to14 );
  VERIFY( to13.hash_code() != to14.hash_code() );
}

int main()
{
  test01();
  return 0;
}
