// 2004-12-06  Paolo Carlini  <pcarlini@suse.de>
//
// Copyright (C) 2004-2024 Free Software Foundation, Inc.
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
  using std::tr1::is_reference;
  using namespace __gnu_test;

  VERIFY( (test_category<is_reference, int&>(true)) );
  VERIFY( (test_category<is_reference, ClassType&>(true)) );
  VERIFY( (test_category<is_reference, int(&)(int)>(true)) );

  // Sanity check.
  VERIFY( (test_category<is_reference, ClassType>(false)) );
}

int main()
{
  test01();
  return 0;
}
