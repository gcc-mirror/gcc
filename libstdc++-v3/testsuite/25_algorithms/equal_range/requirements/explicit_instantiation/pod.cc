
// { dg-do compile }

// 2007-09-20 Benjamin Kosnik <bkoz@redhat.com>

// Copyright (C) 2007-2024 Free Software Foundation, Inc.
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
#include <testsuite_character.h>

namespace std
{
  using __gnu_test::pod_int;

  typedef pod_int 		value_type;
  typedef value_type* 		iterator_type;
  typedef std::pair<iterator_type, iterator_type> pair_type;
  typedef std::less<value_type>	compare_type;

  template pair_type equal_range(iterator_type, iterator_type, 
				 const value_type&);

  template pair_type equal_range(iterator_type, iterator_type, 
				 const value_type&, compare_type);
} 
