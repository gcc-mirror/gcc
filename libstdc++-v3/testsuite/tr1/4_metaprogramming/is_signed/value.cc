// 2005-01-24  Paolo Carlini  <pcarlini@suse.de>
//
// Copyright (C) 2005-2024 Free Software Foundation, Inc.
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
#include <testsuite_hooks.h>
#include <testsuite_tr1.h>

void test01()
{
  using std::tr1::is_signed;
  using namespace __gnu_test;

  VERIFY( (test_category<is_signed, void>(false)) );

  VERIFY( (test_category<is_signed, char>(false)) );
  VERIFY( (test_category<is_signed, signed char>(true)) );
  VERIFY( (test_category<is_signed, unsigned char>(false)) );
  VERIFY( (test_category<is_signed, wchar_t>(false)) );
  VERIFY( (test_category<is_signed, short>(true)) );
  VERIFY( (test_category<is_signed, unsigned short>(false)) );
  VERIFY( (test_category<is_signed, int>(true)) );
  VERIFY( (test_category<is_signed, unsigned int>(false)) );
  VERIFY( (test_category<is_signed, long>(true)) );
  VERIFY( (test_category<is_signed, unsigned long>(false)) );
  VERIFY( (test_category<is_signed, long long>(true)) );
  VERIFY( (test_category<is_signed, unsigned long long>(false)) );

  VERIFY( (test_category<is_signed, float>(false)) );
  VERIFY( (test_category<is_signed, double>(false)) );
  VERIFY( (test_category<is_signed, long double>(false)) );

  // Sanity check.
  VERIFY( (test_category<is_signed, ClassType>(false)) );
}

int main()
{
  test01();
  return 0;
}
