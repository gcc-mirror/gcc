// { dg-do run { target c++11 } }
// { dg-require-cstdint "" }

// 2009-09-09  Benjamin Kosnik  <benjamin@redhat.com>

// Copyright (C) 2009-2020 Free Software Foundation, Inc.
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

#include <deque>
#include <exception/safety.h>

// Container requirement testing, exceptional behavior
int main()
{
  typedef __gnu_cxx::throw_value_limit value_type;
  typedef __gnu_cxx::throw_allocator_limit<value_type> allocator_type;
  typedef std::deque<value_type, allocator_type> test_type;
  __gnu_test::propagation_consistent<test_type> test;
  return 0;
}
