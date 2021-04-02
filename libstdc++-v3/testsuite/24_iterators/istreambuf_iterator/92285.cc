// Copyright (C) 2020-2021 Free Software Foundation, Inc.
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

#include <iterator>
#include <iostream>
#include <testsuite_hooks.h>

// PR libstdc++/92285
// See https://gcc.gnu.org/ml/libstdc++/2019-10/msg00129.html

typedef std::input_iterator_tag category;
typedef std::char_traits<char>::off_type off_type;
typedef std::iterator<category, char, off_type, char*, char> good;
typedef std::iterator<category, char, off_type, char*, char&> bad;

bool check(good&) { return true; }
void check(bad&) { }

void
test01()
{
  typedef std::istreambuf_iterator<char> I;
  I it;
  VERIFY( check(it) );
#if __cplusplus < 201103L
  char c = 'c';
  I::reference r = c;
  VERIFY( &r == &c );
#else
  static_assert( std::is_same<I::reference, char>::value, "LWG 445" );
#endif
}

int main()
{
  test01();
}
