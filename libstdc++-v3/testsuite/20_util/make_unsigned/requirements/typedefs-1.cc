// { dg-options "-std=gnu++0x" }

// 2007-05-03  Benjamin Kosnik  <bkoz@redhat.com>
//
// Copyright (C) 2007, 2008, 2009 Free Software Foundation, Inc.
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

enum test_enum { first_selection };

void test01()
{
  bool test __attribute__((unused)) = true;
  using std::make_unsigned;
  using std::is_same;
  using std::is_unsigned;

  // Positive tests.
  typedef make_unsigned<const unsigned int>::type  	test2_type;
  VERIFY( (is_same<test2_type, const unsigned int>::value) );

  typedef make_unsigned<const signed int>::type  	test21c_type;
  VERIFY( (is_same<test21c_type, const unsigned int>::value) );

  typedef make_unsigned<volatile signed int>::type  	test21v_type;
  VERIFY( (is_same<test21v_type, volatile unsigned int>::value) );

  typedef make_unsigned<const volatile signed int>::type  	test21cv_type;
  VERIFY( (is_same<test21cv_type, const volatile unsigned int>::value) );

  typedef make_unsigned<const char>::type  	test22_type;
  VERIFY( (is_same<test22_type, const unsigned char>::value) );

#ifdef _GLIBCXX_USE_WCHAR_T
  typedef make_unsigned<volatile wchar_t>::type  	test23_type;
  VERIFY( (is_same<test23_type, volatile wchar_t>::value) );
#endif

  // Chapter 48, chapter 20. Smallest rank such that new unsigned type
  // same size.
  typedef make_unsigned<test_enum>::type  	test25_type;
  VERIFY( is_unsigned<test25_type>::value );
  VERIFY( sizeof(test25_type) == sizeof(test_enum) );
}

int main()
{
  test01();
  return 0;
}
