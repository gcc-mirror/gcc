// { dg-options "-std=gnu++0x" }
// { dg-do compile }

// 2011-04-27  Paolo Carlini  <paolo.carlini@oracle.com>

// Copyright (C) 2011 Free Software Foundation, Inc.
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

// NB: This file is for testing utility with NO OTHER INCLUDES.

#include <utility>

namespace std
{
  typedef short test_type;
  
  template
  std::conditional<(!std::is_nothrow_move_constructible<test_type>::value
		    && std::is_copy_constructible<test_type>::value),
		   const test_type&, test_type&&>::type
  move_if_noexcept(test_type&);
}
