// { dg-add-options ieee }

// 1999-08-23 bkoz

// Copyright (C) 1999-2019 Free Software Foundation, Inc.
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
void
test_is_iec559()
{
  bool test;

  if (std::numeric_limits<T>::is_iec559)
    {
      // IEC 559 requires all of the following.
      test = (std::numeric_limits<T>::has_infinity
	      && std::numeric_limits<T>::has_quiet_NaN
	      && std::numeric_limits<T>::has_signaling_NaN);
    }
  else
    {
      // If we had all of the following, why didn't we set IEC 559?
      test = (!std::numeric_limits<T>::has_infinity
	      || !std::numeric_limits<T>::has_quiet_NaN
	      || !std::numeric_limits<T>::has_signaling_NaN);
    }

  VERIFY (test);
}

// libstdc++/8949
void test04()
{
  VERIFY( !std::numeric_limits<short>::is_iec559 );
  VERIFY( !std::numeric_limits<unsigned short>::is_iec559 );
  VERIFY( !std::numeric_limits<int>::is_iec559 );
  VERIFY( !std::numeric_limits<unsigned int>::is_iec559 );
  VERIFY( !std::numeric_limits<long>::is_iec559 );
  VERIFY( !std::numeric_limits<unsigned long>::is_iec559 );
  VERIFY( !std::numeric_limits<long long>::is_iec559 );
  VERIFY( !std::numeric_limits<unsigned long long>::is_iec559 );
}

int main()
{
  test_is_iec559<float>();
  test_is_iec559<double>();
  test_is_iec559<long double>();

  test04();

  return 0;
}
