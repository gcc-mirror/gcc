// 2006-06-19  Paolo Carlini  <pcarlini@suse.de>
//
// Copyright (C) 2006-2016 Free Software Foundation, Inc.
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

// 5.1.4.6 class template xor_combine [tr.rand.eng.xor]
// 5.1.1 Table 16

#include <sstream>
#include <tr1/random>
#include <testsuite_hooks.h>

void
test01()
{
  bool test __attribute__((unused)) = true;
  using std::tr1::xor_combine;
  using std::tr1::minstd_rand;
  using std::tr1::mt19937;

  std::stringstream str;
  xor_combine
    <
    minstd_rand, 1,
    mt19937, 2
    > u, v;

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
