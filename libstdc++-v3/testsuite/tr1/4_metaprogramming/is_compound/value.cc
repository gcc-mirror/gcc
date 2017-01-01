// 2004-12-11  Paolo Carlini  <pcarlini@suse.de>
//
// Copyright (C) 2004-2017 Free Software Foundation, Inc.
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
  using std::tr1::is_compound;
  using namespace __gnu_test;

  VERIFY( (test_category<is_compound, void>(false)) );
  VERIFY( (test_category<is_compound, char>(false)) );
  VERIFY( (test_category<is_compound, signed char>(false)) );
  VERIFY( (test_category<is_compound, unsigned char>(false)) );
#ifdef _GLIBCXX_USE_WCHAR_T
  VERIFY( (test_category<is_compound, wchar_t>(false)) );
#endif
  VERIFY( (test_category<is_compound, short>(false)) );
  VERIFY( (test_category<is_compound, unsigned short>(false)) );
  VERIFY( (test_category<is_compound, int>(false)) );
  VERIFY( (test_category<is_compound, unsigned int>(false)) );
  VERIFY( (test_category<is_compound, long>(false)) );
  VERIFY( (test_category<is_compound, unsigned long>(false)) );
  VERIFY( (test_category<is_compound, long long>(false)) );
  VERIFY( (test_category<is_compound, unsigned long long>(false)) );
  VERIFY( (test_category<is_compound, float>(false)) );
  VERIFY( (test_category<is_compound, double>(false)) );
  VERIFY( (test_category<is_compound, long double>(false)) );

  // Sanity check.
  VERIFY( (test_category<is_compound, ClassType>(true)) );
}

int main()
{
  test01();
  return 0;
}
