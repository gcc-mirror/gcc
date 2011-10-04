// { dg-options "-std=gnu++0x" }
// 2008-05-20  Paolo Carlini  <paolo.carlini@oracle.com>
//
// Copyright (C) 2008, 2009, 2010, 2011 Free Software Foundation, Inc.
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
  using std::is_integral;
  using namespace __gnu_test;
  
  VERIFY( (test_category<is_integral, void>(false)) );
  
  VERIFY( (test_category<is_integral, char>(true)) );
  VERIFY( (test_category<is_integral, signed char>(true)) );
  VERIFY( (test_category<is_integral, unsigned char>(true)) );
#ifdef _GLIBCXX_USE_WCHAR_T
  VERIFY( (test_category<is_integral, wchar_t>(true)) );
#endif
  VERIFY( (test_category<is_integral, char16_t>(true)) );
  VERIFY( (test_category<is_integral, char32_t>(true)) );
  VERIFY( (test_category<is_integral, short>(true)) );
  VERIFY( (test_category<is_integral, unsigned short>(true)) );
  VERIFY( (test_category<is_integral, int>(true)) );
  VERIFY( (test_category<is_integral, unsigned int>(true)) );
  VERIFY( (test_category<is_integral, long>(true)) );
  VERIFY( (test_category<is_integral, unsigned long>(true)) );
  VERIFY( (test_category<is_integral, long long>(true)) );
  VERIFY( (test_category<is_integral, unsigned long long>(true)) );

  VERIFY( (test_category<is_integral, float>(false)) );
  VERIFY( (test_category<is_integral, double>(false)) );
  VERIFY( (test_category<is_integral, long double>(false)) );

  // GNU Extensions.
#ifdef _GLIBCXX_USE_INT128
  VERIFY( (test_category<is_integral, __int128>(true)) );
  VERIFY( (test_category<is_integral, unsigned __int128>(true)) );
#endif

#ifdef _GLIBCXX_USE_FLOAT128
  VERIFY( (test_category<is_integral, __float128>(false)) );
#endif

  // Sanity check.
  VERIFY( (test_category<is_integral, ClassType>(false)) );
}

int main()
{
  test01();
  return 0;
}
