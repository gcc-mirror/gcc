// { dg-do compile { target c++11 } }
// 2009-05-27 Benjamin Kosnik <bkoz@redhat.com>

// Copyright (C) 2009-2016 Free Software Foundation, Inc.
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

#include <initializer_list>

void test01()
{
  // Check for required typedefs
  typedef std::initializer_list<int> test_type;
  typedef test_type::value_type type1;
  typedef test_type::size_type type2;
  typedef test_type::reference type3;
  typedef test_type::const_reference type4;
  typedef test_type::iterator type5;
  typedef test_type::const_iterator type5;
}
