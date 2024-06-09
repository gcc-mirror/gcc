// { dg-do compile { target c++11 } }
// { dg-require-cstdint "" }

// 2010-03-19  Paolo Carlini  <paolo.carlini@oracle.com>

// Copyright (C) 2010-2024 Free Software Foundation, Inc.
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
#include <random>
#include <testsuite_api.h>

namespace std
{
  using __gnu_test::NonDefaultConstructible;

  typedef NonDefaultConstructible         value_type;
  typedef value_type* 		       iterator_type;
  typedef std::mt19937_64            ugenerator_type;

  template void shuffle(iterator_type, iterator_type, ugenerator_type&&);
} 
