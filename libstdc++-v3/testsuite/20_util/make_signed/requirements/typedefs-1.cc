// { dg-do compile { target c++11 } }

// 2007-05-03  Benjamin Kosnik  <bkoz@redhat.com>
//
// Copyright (C) 2007-2017 Free Software Foundation, Inc.
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

enum test_enum { first_selection };

void test01()
{
  using std::make_signed;
  using std::is_same;
  using std::is_signed;
  using std::is_volatile;

  // Positive tests.
  typedef make_signed<const int>::type  	test2_type;
  static_assert( is_same<test2_type, const int>::value,
                 "make_signed<const int>" );

  typedef make_signed<const unsigned int>::type  	test21c_type;
  static_assert( is_same<test21c_type, const signed int>::value,
                 "make_signed<const unsigned int>" );

  typedef make_signed<volatile unsigned int>::type  	test21v_type;
  static_assert( is_same<test21v_type, volatile signed int>::value,
                 "make_signed<volatile unsigned int>" );

  typedef make_signed<const volatile unsigned int>::type  	test21cv_type;
  static_assert( is_same<test21cv_type, const volatile signed int>::value,
                 "make_signed<const volatile unsigned int>" );

  typedef make_signed<const char>::type  	test22_type;
  static_assert( is_same<test22_type, const signed char>::value,
                 "make_signed<const char>" );

#ifdef _GLIBCXX_USE_WCHAR_T
  typedef make_signed<volatile wchar_t>::type  	test23_type;
  static_assert( is_signed<test23_type>::value
                 && is_volatile<test23_type>::value
                 && sizeof(test23_type) == sizeof(volatile wchar_t),
                 "make_signed<volatile wchar_t>" );
#endif

  // Chapter 48, chapter 20. Smallest rank such that new signed type same size.
  typedef make_signed<test_enum>::type  	test24_type;
  static_assert( is_signed<test24_type>::value,
                 "make_signed<test_enum> makes signed type" );
  static_assert( sizeof(test24_type) == sizeof(test_enum),
                 "make_signed<test_enum> makes type of same size" );

#ifndef __STRICT_ANSI__
  // GNU Extensions.
#ifdef _GLIBCXX_USE_INT128
  typedef make_signed<unsigned __int128>::type  test25_type;
  static_assert( is_same<test25_type, __int128>::value,
                 "make_signed<unsigned __int128>" );

  typedef make_signed<__int128>::type  	        test26_type;
  static_assert( is_same<test26_type, __int128>::value,
                 "make_signed<__int128>" );
#endif
#endif
}
