// { dg-do link }

// 2007-03-12  Stephen M. Webb  <stephen.webb@bregmasoft.com>
//
// Copyright (C) 2007-2017 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// tr1 [7.10.1] class template match_results constructor

#include <tr1/regex>
#include <testsuite_hooks.h>

// Tests default constructor of the match_result class.
void test01()
{
  typedef std::tr1::match_results<char*> test_type;

  // default constructor
  test_type m;

  // Check for required typedefs
  typedef test_type::value_type      value_type;
  typedef test_type::const_reference const_reference;
  typedef test_type::reference       reference;
  typedef test_type::const_iterator  const_iterator;
  typedef test_type::iterator        iterator;
  typedef test_type::difference_type difference_type;
  typedef test_type::size_type       size_type;
  typedef test_type::allocator_type  allocator_type;
  typedef test_type::char_type       char_type;
  typedef test_type::string_type     string_type;

  VERIFY( m.size() == 0 );
  VERIFY( m.str() == std::basic_string<char_type>() );
}

int
main()
{
  test01();
  return 0;
}
