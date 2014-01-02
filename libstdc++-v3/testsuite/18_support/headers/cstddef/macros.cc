// { dg-do compile }
// { dg-options "-Wno-conversion-null" }
// 2001-02-06  Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001-2014 Free Software Foundation, Inc.
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

#include <cstddef>

namespace gnu
{
  struct test_type
  {
    int i;
    int j;
  };

  // offsetof
  void test01()
  { 
    std::size_t i __attribute__((unused)) = offsetof(struct test_type, i);
#ifndef offsetof
    #error "offsetof_must_be_a_macro"
#endif
  }

  // NULL
  void test02()
  {
    // Must not be (void*)0
    const int j __attribute__((unused)) = NULL;

#ifndef NULL
    #error "NULL_must_be_a_macro"
#endif
  }
}
