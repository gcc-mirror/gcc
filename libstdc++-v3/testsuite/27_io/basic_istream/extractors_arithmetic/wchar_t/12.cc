// Copyright (C) 2004, 2005, 2006, 2007, 2009, 2010
// Free Software Foundation, Inc.
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

// 27.6.1.2.2 arithmetic extractors

// XXX This test fails on Solaris 8 and 9 because of a bug in libc
// XXX sscanf for very long input.  See:
// XXX http://gcc.gnu.org/ml/gcc/2002-12/msg01422.html
// { dg-do run { xfail { { *-*-solaris2.[89] } || lax_strtofp } } }

#include <istream>
#include <sstream>
#include <limits>
#include <testsuite_hooks.h>

// libstdc++/3720
// excess input should not cause a core dump
template<typename T>
bool test12_aux(bool integer_type)
{
  bool test __attribute__((unused)) = true;
  
  int digits_overflow;
  if (integer_type)
    // This many digits will overflow integer types in base 10.
    digits_overflow = std::numeric_limits<T>::digits10 + 2;
  else
    // This might do it, unsure.
    digits_overflow = std::numeric_limits<T>::max_exponent10 + 1;
  
  std::wstring st;
  std::wstring part = L"1234567890123456789012345678901234567890";
  for (std::size_t i = 0; i < digits_overflow / part.size() + 1; ++i)
    st += part;
  std::wstringbuf sb(st);
  std::wistream is(&sb);
  T t;
  is >> t;
  VERIFY( is.fail() );
  return test;
}

bool test12()
{
  bool test __attribute__((unused)) = true;
  VERIFY( test12_aux<short>(true) );
  VERIFY( test12_aux<int>(true) );
  VERIFY( test12_aux<long>(true) );
  VERIFY( test12_aux<float>(false) );
  VERIFY( test12_aux<double>(false) );
  VERIFY( test12_aux<long double>(false) );
  return test;
}

int main()
{
  test12();
  return 0;
}
