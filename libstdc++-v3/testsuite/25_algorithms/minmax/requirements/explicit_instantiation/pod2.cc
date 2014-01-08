// { dg-options "-std=gnu++0x" }
// { dg-do compile }

// 2008-09-16  Chris Fairles  <chris.fairles@gmail.com>

// Copyright (C) 2008-2014 Free Software Foundation, Inc.
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


#include <algorithm>
#include <functional>
#include <testsuite_character.h>

namespace std
{
  using __gnu_test::pod_int;

  typedef pod_int 		value_type;
  typedef value_type* 		iterator_type;
  typedef std::less<value_type> compare_type;

  template pair<value_type, value_type> minmax(initializer_list<value_type>);
  template pair<value_type, value_type> minmax(initializer_list<value_type>,
					       compare_type);
}
