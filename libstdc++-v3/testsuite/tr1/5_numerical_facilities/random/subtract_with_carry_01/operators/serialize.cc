// 2006-08-22  Paolo Carlini  <pcarlini@suse.de>
//
// Copyright (C) 2006-2023 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// 5.1.4.4 class template subtract_with_carry_01 [tr.rand.eng.sub1]
// 5.1.1 Table 16

#include <sstream>
#include <tr1/random>
#include <testsuite_hooks.h>

void
test01()
{
  using std::tr1::subtract_with_carry_01;

  std::stringstream str;
  subtract_with_carry_01<float, 24, 10, 24> u;
  subtract_with_carry_01<float, 24, 10, 24> v;

  u(); // advance
  str << u;

  VERIFY( u != v );

  str >> v;
  VERIFY( u == v );
}

int main()
{
  test01();
  return 0;
}
