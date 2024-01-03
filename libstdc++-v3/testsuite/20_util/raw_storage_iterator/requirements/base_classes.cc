// { dg-do compile }

// 2001-06-18  Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001-2024 Free Software Foundation, Inc.
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

// 20.4.2 raw storage iterator

#include <memory>

void test01()
{
  using namespace std;

  // Check for required base class.
  long l;
  typedef raw_storage_iterator<long*, long> test_iterator;                     // { dg-warning "is deprecated" "" { target c++17 } }
  typedef iterator<output_iterator_tag, void, void, void, void> base_iterator; // { dg-warning "is deprecated" "" { target c++17 } }
  test_iterator rs_it(&l);
  base_iterator* base __attribute__((unused)) = &rs_it;
}
