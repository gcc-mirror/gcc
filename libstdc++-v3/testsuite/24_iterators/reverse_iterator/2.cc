// 2001-06-21  Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001-2021 Free Software Foundation, Inc.
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

// 24.4.1.2 Reverse iterators

#include <iterator>

void test02()
{
  typedef std::reverse_iterator<int*> iterator_type;
  iterator_type it01;
  iterator_type it02;

  // Sanity check non-member operators and functions can be instantiated.
  it01 == it02;
  it01 != it02;
  it01 < it02;
  it01 <= it02;
  it01 > it02;
  it01 >= it02;
  it01 - it02;
  0 + it02;
}

int main()
{
  test02();
  return 0;
}
