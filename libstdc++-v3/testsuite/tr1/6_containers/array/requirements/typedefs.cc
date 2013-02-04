// { dg-do compile }

// 2004-10-20  Benjamin Kosnik  <bkoz@redhat.com>
//
// Copyright (C) 2004-2013 Free Software Foundation, Inc.
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


// 6.2.2 Class template array

#include <tr1/array>

void test01()
{
  // Check for required typedefs
  typedef std::tr1::array<int, 5> test_type;
  typedef test_type::reference reference;
  typedef test_type::const_reference const_reference;
  typedef test_type::iterator iterator;
  typedef test_type::const_iterator const_iterator;
  typedef test_type::size_type size_type;
  typedef test_type::difference_type difference_type;
  typedef test_type::value_type value_type;
  typedef test_type::reverse_iterator reverse_iterator;
  typedef test_type::const_reverse_iterator const_reverse_iterator;
}
