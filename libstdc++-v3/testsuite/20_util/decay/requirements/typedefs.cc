// { dg-do compile { target c++11 } }

// 2007-05-03  Benjamin Kosnik  <bkoz@redhat.com>
//
// Copyright (C) 2007-2019 Free Software Foundation, Inc.
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

void test01()
{
  using std::decay;
  using std::is_same;

  // Positive tests.
  typedef decay<bool>::type     	test1_type;
  static_assert( is_same<test1_type, bool>(), "decay<bool>" );

  // NB: DR 705.
  typedef decay<const int>::type  	test2_type;
  static_assert( is_same<test2_type, int>(), "decay<const int>" );

  typedef decay<int[4]>::type     	test3_type;
  static_assert( is_same<test3_type, std::remove_extent<int[4]>::type*>(),
                 "decay<int[4]>" );

  typedef void (fn_type) ();
  typedef decay<fn_type>::type  	test4_type;
  static_assert( is_same<test4_type, std::add_pointer<fn_type>::type>(),
                 "decay<fn_type>" );

  typedef void (cfn_type) () const;
  typedef decay<cfn_type>::type  	test5_type;
  static_assert( is_same<test5_type, cfn_type>(), "decay<cfn_type>" );
}
