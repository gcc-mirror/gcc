// { dg-do compile }
// 1999-08-24 bkoz

// Copyright (C) 1999-2014 Free Software Foundation, Inc.
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

// 22.2.1 The ctype category

// 1: Test that the locale headers are picking up the correct declaration
// of the internal type `ctype_base::mask'.
int mask ();

#include <locale>

// 2: Sanity check ctype_base::mask bitmask requirements
void
test01()
{
  using namespace std;

  ctype_base::mask m01;
  ctype_base::mask m02;
  ctype_base::mask res;

  m01 = ctype_base::space;
  m02 = ctype_base::xdigit;

  res = m01 & m02;
  res = m01 | m02;
  res = m01 ^ m02;
  res = ~m01;
  res = res; // Suppress unused warning.

  m01 &= m02;
  m01 |= m02;
  m01 ^= m02;
}

int main() 
{ 
  test01();
  return 0;
}
