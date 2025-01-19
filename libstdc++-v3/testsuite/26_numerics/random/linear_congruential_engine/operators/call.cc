// Copyright (C) 2020-2025 Free Software Foundation, Inc.
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

// { dg-do run { target c++11 } }

#include <random>
#include <testsuite_hooks.h>

void
test01()
{
  std::linear_congruential_engine<unsigned, 0, 0, 0> l;
  auto r = l(); // this used to result in divide by zero
  VERIFY( r == 0 );
  l.seed(2);
  r = l();
  VERIFY( r == 0 );
  VERIFY( l() == 0 );
}

void
test02()
{
  std::linear_congruential_engine<unsigned, 0, 0, 3> l;
  auto r = l(); // this used to result in a different divide by zero
  VERIFY( r == 0 );
  l.seed(2);
  r = l();
  VERIFY( r == 0 );
  VERIFY( l() == 0 );
}

void
test03()
{
  std::linear_congruential_engine<unsigned, 0, 2, 3> l;
  auto r = l();
  VERIFY( r == 2 );
  l.seed(4);
  r = l();
  VERIFY( r == 2 );
  VERIFY( l() == 2 );
}

int main()
{
  test01();
  test02();
  test03();
}
