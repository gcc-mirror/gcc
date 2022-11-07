// { dg-do compile }
// { dg-require-effective-target hosted }

// 2001-06-25  Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001-2022 Free Software Foundation, Inc.
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

// 24.5.4 Template class ostream_iterator

#include <iterator>
#include <ostream>
#include <sstream>

void test01()
{
  using namespace std;

  // Check for required base class.
  typedef ostream_iterator<long> test_iterator;
  typedef iterator<output_iterator_tag, void, void, void, void> base_iterator; // { dg-warning "is deprecated" "" { target c++17 } }
  ostringstream osstream("this tag");
  test_iterator  r_it(osstream);
  base_iterator* base __attribute__((unused)) = &r_it;
}
