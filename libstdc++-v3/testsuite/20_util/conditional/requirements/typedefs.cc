// { dg-do compile { target c++11 } }

// 2007-05-02  Benjamin Kosnik  <bkoz@redhat.com>
//
// Copyright (C) 2007-2025 Free Software Foundation, Inc.
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

#include <type_traits>

using std::conditional;
using std::is_same;

typedef conditional<true, char, int>::type     test1_type;
static_assert( is_same<test1_type, char>::value, "conditional<true, ...>" );
  
typedef conditional<false, char, int>::type     test2_type;
static_assert( is_same<test2_type, int>::value, "conditional<false, ...>" );
