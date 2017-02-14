// Copyright (C) 2014-2017 Free Software Foundation, Inc.
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

#include <memory>
#include <testsuite_hooks.h>

// test specialization for trivial types
void
test01()
{
  const int N = 10;
  int arr[N] = { };
  const int n = 5;
  const int over9000 = 9001;
  int* end = std::uninitialized_fill_n(arr, n, over9000);
  VERIFY( end = arr + n );
  for (int i = 0; i < n; ++i)
    VERIFY( arr[i] == over9000 );
  for (int i = n; i < N; ++i)
    VERIFY( arr[i] == 0 );
}

struct T
{
  T() { }
  T(const T&) { ++counter; }
  static int counter;
};

int T::counter;

// test non-trivial
void
test02()
{
  const int n = 5;
  char* mem = new char[sizeof(T)*n];
  T* p = reinterpret_cast<T*>(mem);
  T* end = std::uninitialized_fill_n(p, n, T());
  VERIFY( end = p + n );
  VERIFY( T::counter == n );
  delete[] mem;
}

int
main()
{
  test01();
  test02();
}
