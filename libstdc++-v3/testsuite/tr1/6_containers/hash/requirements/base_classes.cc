// { dg-options "-Wno-deprecated-declarations" }
// { dg-do compile }

// Copyright (C) 2007-2025 Free Software Foundation, Inc.
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

#include <tr1/functional>

void test01()
{
  // Check for required base class.
  typedef long value_type;
  typedef std::tr1::hash<value_type> test_type;
  typedef std::unary_function<value_type, std::size_t> base_type;

  test_type b;
  const test_type& obj = b;
  const base_type* base __attribute__((unused)) = &obj;
}
