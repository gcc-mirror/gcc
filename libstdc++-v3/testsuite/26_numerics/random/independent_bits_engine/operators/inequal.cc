// { dg-do run { target c++11 } }
// { dg-require-cstdint "" }
//
// 2010-03-16  Paolo Carlini  <paolo.carlini@oracle.com>
//
// Copyright (C) 2010-2025 Free Software Foundation, Inc.
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

// 26.5.4.2 class template independent_bits_engine [rand.adapt.bits]

#include <random>
#include <testsuite_hooks.h>

void
test01()
{
  std::independent_bits_engine
    <std::subtract_with_carry_engine<uint_fast64_t, 48, 5, 12>,
     48, uint_fast64_t> u, v;

  VERIFY( !(u != v) );

  u.discard(100);
  v.discard(100);

  VERIFY( !(u != v) );
}

int main()
{
  test01();
  return 0;
}
