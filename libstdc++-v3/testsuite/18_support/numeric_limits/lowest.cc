// { dg-options "-std=gnu++0x" }
// { dg-add-options ieee }

// 2010-02-25  Ed Smith-Rowland

// Copyright (C) 2010-2013 Free Software Foundation, Inc.
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
#include <type_traits>
#include <testsuite_hooks.h>

template<typename T>
  void
  do_test(std::true_type)
  {
    bool test __attribute__((unused)) = true;
    T limits_min = std::numeric_limits<T>::min();
    VERIFY( std::numeric_limits<T>::lowest() == limits_min );
  }

template<typename T>
  void
  do_test(std::false_type)
  {
    bool test __attribute__((unused)) = true;
    T limits_max = std::numeric_limits<T>::max();
    VERIFY( std::numeric_limits<T>::lowest() == -limits_max );
  }

template<typename Tp>
  void
  do_test()
  { do_test<Tp>(typename std::is_integral<Tp>::type()); }

void test01()
{
  do_test<char>();
  do_test<signed char>();
  do_test<unsigned char>();
#ifdef _GLIBCXX_USE_WCHAR_T
  do_test<wchar_t>();
#endif
  do_test<char16_t>();
  do_test<char32_t>();

  do_test<short>();
  do_test<unsigned short>();

  do_test<int>();
  do_test<unsigned int>();

  do_test<long>();
  do_test<unsigned long>();

  do_test<long long>();
  do_test<unsigned long long>();

  // GNU Extensions.
#ifdef _GLIBCXX_USE_INT128
  do_test<__int128>();
  do_test<unsigned __int128>();
#endif

  do_test<float>();
  do_test<double>();
  do_test<long double>();
}

int main()
{
  test01();
  return 0;
}
