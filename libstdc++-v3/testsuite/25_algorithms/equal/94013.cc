// Copyright (C) 2020-2023 Free Software Foundation, Inc.
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
  volatile int i[] = { 0, 1, 2, 3 };
  volatile int j[] = { 1, 2, 3 };
  int k[] = { 2, 3, 4 };

  VERIFY( std::equal(i+1, i+4, j) );	  // volatile, volatile
  VERIFY( std::equal(i+2, i+4, k) );	  // volatile, unqual
  VERIFY( ! std::equal(k, k+3, i+1) );	  // unqual, volatile

  const volatile int* cj = j;
  VERIFY( ! std::equal(cj, cj+2, cj+1) ); // const volatile, const volatile
  VERIFY( std::equal(cj, cj+3, i+1) );	  // const volatile, volatile
  VERIFY( ! std::equal(i, i+2, cj) );	  // volatile, const volatile
  VERIFY( std::equal(cj+1, cj+3, k) );	  // const volatile, unqual
  VERIFY( ! std::equal(k, k+2, cj) );	  // unqual, const volatile
  const int* ck = k;
  VERIFY( std::equal(ck, ck+2, i+2) );	  // const, volatile
  VERIFY( ! std::equal(i, i+3, ck) );	  // volatile, const
  VERIFY( std::equal(cj+1, cj+3, ck) );	  // const volatile, const
  VERIFY( ! std::equal(ck, ck+1, cj) );	  // const, const volatile

#if __cplusplus > 201703L
  using std::ranges::equal;
  VERIFY( equal(i+1, i+4, j, j+3) );	  // volatile, volatile
  VERIFY( equal(i+2, i+4, k, k+2) );	  // volatile, unqual
  VERIFY( ! equal(k, k+3, i+1, i+4) );	  // unqual, volatile

  VERIFY( ! equal(cj, cj+2, cj+1, cj+3) );// const volatile, const volatile
  VERIFY( equal(cj, cj+3, i+1, i+4) );	  // const volatile, volatile
  VERIFY( ! equal(i, i+2, cj, cj+2) );	  // volatile, const volatile
  VERIFY( equal(cj+1, cj+3, k, k+2) );	  // const volatile, unqual
  VERIFY( ! equal(k, k+2, cj, cj+2) );	  // unqual, const volatile

  VERIFY( equal(ck, ck+2, i+2, i+4) );	  // const, volatile
  VERIFY( ! equal(i, i+3, ck, ck+3) );	  // volatile, const
  VERIFY( equal(cj+1, cj+3, ck, ck+2) );  // const volatile, const
  VERIFY( ! equal(ck, ck+1, cj, cj+1) );  // const, const volatile
#endif
}

int
main()
{
  test01();
}
