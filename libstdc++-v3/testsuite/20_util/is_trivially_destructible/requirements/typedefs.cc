// { dg-do compile { target c++11 } }
//
// 2012-04-15  Paolo Carlini  <paolo.carlini@oracle.com>
//
// Copyright (C) 2012-2024 Free Software Foundation, Inc.
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

// 
// NB: This file is for testing type_traits with NO OTHER INCLUDES.

#include <type_traits>

void test01()
{
  // Check for required typedefs
  typedef std::is_trivially_destructible<int> test_type;
  typedef test_type::value_type               value_type;
  typedef test_type::type                     type;
  typedef test_type::type::value_type         type_value_type;
  typedef test_type::type::type               type_type;
}
