// { dg-options "-std=gnu++0x" }
// 2005-01-24  Paolo Carlini  <pcarlini@suse.de>
//
// Copyright (C) 2005, 2009, 2010, 2011 Free Software Foundation, Inc.
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
  using std::is_signed;
  using namespace __gnu_test;
  
  VERIFY( (test_category<is_signed, void>(false)) );
  
  VERIFY( (test_category<is_signed, char>(char(-1) < char(0))) );
  VERIFY( (test_category<is_signed, signed char>(true)) );
  VERIFY( (test_category<is_signed, unsigned char>(false)) );
#ifdef _GLIBCXX_USE_WCHAR_T
  VERIFY( (test_category<is_signed, wchar_t>(wchar_t(-1) < wchar_t(0))) );
#endif
  VERIFY( (test_category<is_signed, short>(true)) );
  VERIFY( (test_category<is_signed, unsigned short>(false)) );
  VERIFY( (test_category<is_signed, int>(true)) );
  VERIFY( (test_category<is_signed, unsigned int>(false)) );
  VERIFY( (test_category<is_signed, long>(true)) );
  VERIFY( (test_category<is_signed, unsigned long>(false)) );
  VERIFY( (test_category<is_signed, long long>(true)) );
  VERIFY( (test_category<is_signed, unsigned long long>(false)) );

  VERIFY( (test_category<is_signed, float>(true)) );
  VERIFY( (test_category<is_signed, double>(true)) );
  VERIFY( (test_category<is_signed, long double>(true)) );

  // GNU Extensions.
#ifdef _GLIBCXX_USE_INT128
  VERIFY( (test_category<is_signed, __int128>(true)) );
  VERIFY( (test_category<is_signed, unsigned __int128>(false)) );
#endif

#ifdef _GLIBCXX_USE_FLOAT128
  VERIFY( (test_category<is_signed, __float128>(true)) );
#endif

  // Sanity check.
  VERIFY( (test_category<is_signed, ClassType>(false)) );
}

int main()
{
  test01();
  return 0;
}
