// Copyright (C) 2019-2025 Free Software Foundation, Inc.
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

// { dg-do run }

#include <algorithm>
#include <testsuite_hooks.h>

int a[4] = { 0, 1, 2, 3 };
int g;
int gen() { return g; }
enum E { e2 = 2 };

struct Num
{
  char val;

  operator char() const { return val; }

private:
  void operator+() const;
  void operator+(int) const;
  void operator+(Num) const;
  void operator<(int) const;
  void operator>(int) const;
  void operator<=(int) const;
  void operator>=(int) const;
  void operator==(int) const;
  void operator!=(int) const;
};

void
test01()
{
  int* p;

  g = -1;
  p = std::generate_n(a, true, &gen); // bool as Size
  VERIFY( p == a+1 );
  VERIFY( a[0] == g );
  VERIFY( a[1] == 1 );
  VERIFY( a[2] == 2 );
  VERIFY( a[3] == 3 );

  g = -2;
  p = std::generate_n(a, e2, &gen); // enumeration type as Size
  VERIFY( p == a+2 );
  VERIFY( a[0] == g );
  VERIFY( a[1] == g );
  VERIFY( a[2] == 2 );
  VERIFY( a[3] == 3 );

  g = -3;
  p = std::generate_n(a, 3.5, &gen); // floating point type as Size
  VERIFY( p == a+3 );
  VERIFY( a[0] == g );
  VERIFY( a[1] == g );
  VERIFY( a[2] == g );
  VERIFY( a[3] == 3 );

  g = -4;
  Num n = { 3 };
  p = std::generate_n(a, n, &gen); // non-scalar type as Size
  VERIFY( p == a+3 );
  VERIFY( a[0] == g );
  VERIFY( a[1] == g );
  VERIFY( a[2] == g );
  VERIFY( a[3] == 3 );
}

int main()
{
  test01();
}
