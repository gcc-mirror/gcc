// Copyright (C) 2020-2024 Free Software Foundation, Inc.
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

void
test01()
{
  volatile unsigned char i[] = { 0, 1, 2, 3 };
  volatile unsigned char j[] = { 1, 2, 3 };
  unsigned char k[] = { 2, 3, 4 };

  // v = volatile, c = const, cv = const volatile, u = unqualified

  VERIFY( ! std::lexicographical_compare(i+1, i+4, j, j+2) ); // v v
  VERIFY( std::lexicographical_compare(i+2, i+4, k, k+3) );   // v u
  VERIFY( ! std::lexicographical_compare(k, k+3, i+1, i+4) ); // u v

  const volatile unsigned char* cj = j;
  VERIFY( std::lexicographical_compare(cj, cj+2, cj+1, cj+3) );	// cv cv
  VERIFY( ! std::lexicographical_compare(cj, cj+3, i+1, i+4) );	// cv v
  VERIFY( std::lexicographical_compare(i, i+2, cj, cj+2) );	// v cv
  VERIFY( std::lexicographical_compare(cj+1, cj+3, k, k+3) );	// cv u
  VERIFY( ! std::lexicographical_compare(k, k+2, cj, cj+3) );	// u cv
  const unsigned char* ck = k;
  VERIFY( ! std::lexicographical_compare(ck, ck+2, i+1, i+2) );	// c v
  VERIFY( std::lexicographical_compare(i, i+3, ck, ck+3) );	// v c
  VERIFY( std::lexicographical_compare(cj+1, cj+3, ck, ck+3) );	// cv c
  VERIFY( ! std::lexicographical_compare(ck, ck+1, cj, cj+2) );	// c cv

#if __cplusplus > 201703L
  using std::ranges::lexicographical_compare;
  VERIFY( ! lexicographical_compare(i+1, i+4, j, j+2) ); // v v
  VERIFY( lexicographical_compare(i+2, i+4, k, k+3) );   // v u
  VERIFY( ! lexicographical_compare(k, k+3, i+1, i+4) ); // u v

  VERIFY( lexicographical_compare(cj, cj+2, cj+1, cj+3) );	// cv cv
  VERIFY( ! lexicographical_compare(cj, cj+3, i+1, i+4) );	// cv v
  VERIFY( lexicographical_compare(i, i+2, cj, cj+2) );		// v cv
  VERIFY( lexicographical_compare(cj+1, cj+3, k, k+3) );	// cv u
  VERIFY( ! lexicographical_compare(k, k+2, cj, cj+3) );	// u cv

  VERIFY( ! lexicographical_compare(ck, ck+2, i+1, i+2) );	// c v
  VERIFY( lexicographical_compare(i, i+3, ck, ck+3) );		// v c
  VERIFY( lexicographical_compare(cj+1, cj+3, ck, ck+3) );	// cv c
  VERIFY( ! lexicographical_compare(ck, ck+1, cj, cj+2) );	// c cv
#endif
}

int
main()
{
  test01();
}
