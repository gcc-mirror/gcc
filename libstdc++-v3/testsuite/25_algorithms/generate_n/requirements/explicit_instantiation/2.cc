// { dg-do compile }

// 2007-09-20 Benjamin Kosnik <bkoz@redhat.com>

// Copyright (C) 2007-2021 Free Software Foundation, Inc.
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
#include <testsuite_api.h>
#include <testsuite_api.h>

namespace std
{
  using __gnu_test::NonDefaultConstructible;
  using __gnu_test::void_function;

  typedef NonDefaultConstructible 		value_type;
  typedef value_type* 		iterator_type;
  typedef unsigned short 	size_type;
  typedef void_function<value_type> generator_type;

  template iterator_type generate_n(iterator_type, size_type, generator_type);
} 
