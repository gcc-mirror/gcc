// { dg-options "-std=gnu++17" }
//
// Copyright (C) 2015-2019 Free Software Foundation, Inc.
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

// { dg-do compile }

void test01()
{
  // Check for required typedefs
  typedef std::conjunction<std::true_type, std::true_type>      test_type;
  typedef test_type::value_type               value_type;
  typedef test_type::type                     type;
  typedef test_type::type::value_type         type_value_type;
  typedef test_type::type::type               type_type;
}

void test02()
{
  // Check for required typedefs
  typedef std::disjunction<std::false_type, std::true_type>      test_type;
  typedef test_type::value_type               value_type;
  typedef test_type::type                     type;
  typedef test_type::type::value_type         type_value_type;
  typedef test_type::type::type               type_type;
}

void test03()
{
  // Check for required typedefs
  typedef std::negation<std::false_type>      test_type;
  typedef test_type::value_type               value_type;
  typedef test_type::type                     type;
  typedef test_type::type::value_type         type_value_type;
  typedef test_type::type::type               type_type;
}
