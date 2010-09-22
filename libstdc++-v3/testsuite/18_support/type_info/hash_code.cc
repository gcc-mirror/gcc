// { dg-options "-std=gnu++0x" }

// 2010-09-21  Paolo Carlini  <paolo.carlini@oracle.com>
//
// Copyright (C) 2010 Free Software Foundation
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

class Abraca { };
Abraca a1, a2_;
const Abraca a2 = a2_;

class Dabra { };
Dabra d1;

void test01()
{
  bool test __attribute__((unused)) = true;

  VERIFY( typeid(int) != typeid(double) );
  VERIFY( typeid(int).hash_code() != typeid(double).hash_code() );

  VERIFY( typeid(a1) == typeid(a2) );
  VERIFY( typeid(a1).hash_code() == typeid(a2).hash_code() );

  VERIFY( typeid(Abraca) == typeid(const Abraca) );
  VERIFY( typeid(Abraca).hash_code() == typeid(const Abraca).hash_code() );

  VERIFY( typeid(Abraca) == typeid(a2) );
  VERIFY( typeid(Abraca).hash_code() == typeid(a2).hash_code() );

  VERIFY( typeid(Abraca) == typeid(const Abraca&) );
  VERIFY( typeid(Abraca).hash_code() == typeid(const Abraca&).hash_code() );

  VERIFY( typeid(Abraca) != typeid(Dabra) );
  VERIFY( typeid(Abraca).hash_code() != typeid(Dabra).hash_code() );

  VERIFY( typeid(a1) != typeid(d1) );
  VERIFY( typeid(a1).hash_code() != typeid(d1).hash_code() );
}

int main()
{
  test01();
  return 0;
}
