// { dg-options "-std=gnu++0x" }
// 2010-03-23  Paolo Carlini  <paolo.carlini@oracle.com>
//
// Copyright (C) 2010-2013 Free Software Foundation, Inc.
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

#include <type_traits>
#include <testsuite_hooks.h>
#include <testsuite_tr1.h>

void test01()
{
  bool test __attribute__((unused)) = true;
  using std::is_literal_type;
  using namespace __gnu_test;

  VERIFY( (test_category<is_literal_type, int>(true)) );
  VERIFY( (test_category<is_literal_type, unsigned char>(true)) );

  VERIFY( (test_category<is_literal_type, TType>(true)) );
  VERIFY( (test_category<is_literal_type, PODType>(true)) );

  VERIFY( (test_category<is_literal_type, NType>(false)) );
  VERIFY( (test_category<is_literal_type, SLType>(false)) );

  VERIFY( (test_category<is_literal_type, LType>(true)) );
  VERIFY( (test_category<is_literal_type, LType[5]>(true)) );

  VERIFY( (test_category<is_literal_type, NLType>(false)) );
  VERIFY( (test_category<is_literal_type, NLType[5]>(false)) );

  VERIFY( (test_category<is_literal_type, LTypeDerived>(true)) );
  VERIFY( (test_category<is_literal_type, LTypeDerived[5]>(true)) );
}

int main()
{
  test01();
  return 0;
}
