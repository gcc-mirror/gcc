// { dg-do compile }
// 1999-06-28 bkoz

// Copyright (C) 1999-2018 Free Software Foundation, Inc.
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

// 24.5.3 template class istreambuf_iterator

#include <sstream>
#include <iterator>
#include <testsuite_hooks.h>

void test01()
{
  using namespace std;

  // Check for required base class.
  typedef istreambuf_iterator<char> test_iterator;
  typedef char_traits<char>::off_type off_type;

  typedef iterator<input_iterator_tag, char, off_type, char*,
#if __cplusplus >= 201103L
    char>
#else
    char&>
#endif
    base_iterator;

  istringstream isstream("this tag");
  test_iterator  r_it(isstream);
  base_iterator* base __attribute__((unused)) = &r_it;
}
