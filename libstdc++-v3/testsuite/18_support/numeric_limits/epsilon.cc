// { dg-options "-mieee" { target alpha*-*-* } }
// { dg-options "-mieee" { target sh*-*-* } }

// 1999-08-23 bkoz

// Copyright (C) 1999, 2001, 2002, 2003, 2004, 2009 Free Software Foundation
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

// 18.2.1.1 template class numeric_limits

#include <limits>
#include <limits.h>
#include <float.h>
#include <cwchar>
#include <testsuite_hooks.h>

template<typename T>
void test_epsilon()
{
  bool test __attribute__((unused)) = true;
  T epsilon = std::numeric_limits<T>::epsilon();
  T one = 1;

  VERIFY( one != (one + epsilon) );
}

int main()
{
  test_epsilon<float>();
  test_epsilon<double>();
  test_epsilon<long double>();

  return 0;
}
