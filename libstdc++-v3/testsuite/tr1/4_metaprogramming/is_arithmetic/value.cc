// 2004-12-03  Paolo Carlini  <pcarlini@suse.de>
//
// Copyright (C) 2004-2016 Free Software Foundation, Inc.
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

// 4.5.2 Composite type traits

#include <tr1/type_traits>
#include <testsuite_hooks.h>
#include <testsuite_tr1.h>

void test01()
{
  bool test __attribute__((unused)) = true;
  using std::tr1::is_arithmetic;
  using namespace __gnu_test;

  VERIFY( (test_category<is_arithmetic, void>(false)) );

  VERIFY( (test_category<is_arithmetic, char>(true)) );
  VERIFY( (test_category<is_arithmetic, signed char>(true)) );
  VERIFY( (test_category<is_arithmetic, unsigned char>(true)) );
#ifdef _GLIBCXX_USE_WCHAR_T
  VERIFY( (test_category<is_arithmetic, wchar_t>(true)) );
#endif
  VERIFY( (test_category<is_arithmetic, short>(true)) );
  VERIFY( (test_category<is_arithmetic, unsigned short>(true)) );
  VERIFY( (test_category<is_arithmetic, int>(true)) );
  VERIFY( (test_category<is_arithmetic, unsigned int>(true)) );
  VERIFY( (test_category<is_arithmetic, long>(true)) );
  VERIFY( (test_category<is_arithmetic, unsigned long>(true)) );
  VERIFY( (test_category<is_arithmetic, long long>(true)) );
  VERIFY( (test_category<is_arithmetic, unsigned long long>(true)) );
  VERIFY( (test_category<is_arithmetic, float>(true)) );
  VERIFY( (test_category<is_arithmetic, double>(true)) );
  VERIFY( (test_category<is_arithmetic, long double>(true)) );

  // Sanity check.
  VERIFY( (test_category<is_arithmetic, ClassType>(false)) );
}

int main()
{
  test01();
  return 0;
}
