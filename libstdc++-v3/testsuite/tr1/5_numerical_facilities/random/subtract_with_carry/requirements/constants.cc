// { dg-do link }
//
// 2009-09-29  Paolo Carlini <paolo.carlini@oracle.com>
//
// Copyright (C) 2009-2019 Free Software Foundation, Inc.
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

#include <tr1/random>

void test01()
{
  std::tr1::subtract_with_carry<unsigned long, (1UL << 24), 10, 24> swc;

  const void* p = &swc.modulus;
  p = &swc.long_lag;
  p = &swc.short_lag;
  p = p; // Suppress unused warning.
}

int main()
{
  test01();
  return 0;
}
