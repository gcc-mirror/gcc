// 2004-12-03  Paolo Carlini  <pcarlini@suse.de>
//
// Copyright (C) 2004-2020 Free Software Foundation, Inc.
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
  using std::tr1::is_void;
  using namespace __gnu_test;

  VERIFY( (test_category<is_void, void>(true)) );

  VERIFY( (test_category<is_void, char>(false)) );
  VERIFY( (test_category<is_void, signed char>(false)) );
  VERIFY( (test_category<is_void, unsigned char>(false)) );
#ifdef _GLIBCXX_USE_WCHAR_T
  VERIFY( (test_category<is_void, wchar_t>(false)) );
#endif
  VERIFY( (test_category<is_void, short>(false)) );
  VERIFY( (test_category<is_void, unsigned short>(false)) );
  VERIFY( (test_category<is_void, int>(false)) );
  VERIFY( (test_category<is_void, unsigned int>(false)) );
  VERIFY( (test_category<is_void, long>(false)) );
  VERIFY( (test_category<is_void, unsigned long>(false)) );
  VERIFY( (test_category<is_void, long long>(false)) );
  VERIFY( (test_category<is_void, unsigned long long>(false)) );
  VERIFY( (test_category<is_void, float>(false)) );
  VERIFY( (test_category<is_void, double>(false)) );
  VERIFY( (test_category<is_void, long double>(false)) );

  // Sanity check.
  VERIFY( (test_category<is_void, ClassType>(false)) );
}

int main()
{
  test01();
  return 0;
}
