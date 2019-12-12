// { dg-do compile { target c++11 } }

// 2007-10-19  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2007-2019 Free Software Foundation, Inc.
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

namespace std
{
  using __gnu_test::NonDefaultConstructible;

  typedef NonDefaultConstructible 		value_type;
  typedef value_type* 		iterator_type;

  template iterator_type move(iterator_type, iterator_type, iterator_type);
} 
