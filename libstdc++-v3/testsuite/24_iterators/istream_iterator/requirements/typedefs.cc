// { dg-do compile }
// 2001-06-25  Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001-2014 Free Software Foundation, Inc.
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

// 24.5.1 Template class istream_iterator

#include <iterator>
#include <sstream>
#include <testsuite_hooks.h>

void test01()
{
  using namespace std;

  // Check for required typedefs
  typedef istream_iterator<long> test_iterator;
  typedef test_iterator::value_type value_type;
  typedef test_iterator::difference_type difference_type;
  typedef test_iterator::pointer pointer;
  typedef test_iterator::reference reference;
  typedef test_iterator::iterator_category iteratory_category;

  typedef test_iterator::char_type char_type;
  typedef test_iterator::traits_type traits_type;
  typedef test_iterator::istream_type istream_type;
}
