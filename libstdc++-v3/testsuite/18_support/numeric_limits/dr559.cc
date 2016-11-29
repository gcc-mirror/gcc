// { dg-do run { target c++11 } }

// 2010-02-17  Paolo Carlini  <paolo.carlini@oracle.com>
//
// Copyright (C) 2010-2016 Free Software Foundation, Inc.
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

#include <limits>
#include <type_traits>
#include <testsuite_hooks.h>

template<typename T>
  void do_test_aux()
  {
    typedef std::numeric_limits<T> cv_limits;
    typedef std::numeric_limits<typename std::remove_cv<T>::type> limits;

    VERIFY( cv_limits::is_specialized == limits::is_specialized );
    VERIFY( cv_limits::min() == limits::min() );
    VERIFY( cv_limits::max() == limits::max() );
    VERIFY( cv_limits::lowest() == limits::lowest() );
    VERIFY( cv_limits::digits == limits::digits );
    VERIFY( cv_limits::digits10 == limits::digits10 );
    VERIFY( cv_limits::max_digits10 == limits::max_digits10 );
    VERIFY( cv_limits::is_signed == limits::is_signed );
    VERIFY( cv_limits::is_integer == limits::is_integer );
    VERIFY( cv_limits::is_exact == limits::is_exact );
    VERIFY( cv_limits::radix == limits::radix );
    VERIFY( cv_limits::epsilon() == limits::epsilon() );
    VERIFY( cv_limits::round_error() == limits::round_error() );
    VERIFY( cv_limits::min_exponent == limits::min_exponent );
    VERIFY( cv_limits::min_exponent10 == limits::min_exponent10 );
    VERIFY( cv_limits::max_exponent == limits::max_exponent );
    VERIFY( cv_limits::max_exponent10 == limits::max_exponent10 );
    VERIFY( cv_limits::has_infinity == limits::has_infinity );
    VERIFY( cv_limits::has_quiet_NaN == limits::has_quiet_NaN );
    VERIFY( cv_limits::has_signaling_NaN == limits::has_signaling_NaN );
    VERIFY( cv_limits::has_denorm == limits::has_denorm );
    VERIFY( cv_limits::has_denorm_loss == limits::has_denorm_loss );
    VERIFY( cv_limits::infinity() == limits::infinity() );
    if (!std::is_floating_point<T>::value)
      {
	VERIFY( cv_limits::quiet_NaN() == limits::quiet_NaN() );
	VERIFY( cv_limits::signaling_NaN() == limits::signaling_NaN() );
      }
    VERIFY( cv_limits::denorm_min() == limits::denorm_min() );
    VERIFY( cv_limits::is_iec559 == limits::is_iec559 );
    VERIFY( cv_limits::is_bounded == limits::is_bounded );
    VERIFY( cv_limits::is_modulo == limits::is_modulo );
    VERIFY( cv_limits::traps == limits::traps );
    VERIFY( cv_limits::tinyness_before == limits::tinyness_before );
    VERIFY( cv_limits::round_style == limits::round_style );
  }

template<typename T>
  void
  do_test()
  {
    do_test_aux<T>();
    do_test_aux<const T>();
    do_test_aux<volatile T>();
    do_test_aux<const volatile T>();
  }

// DR 559.
int main()
{
  do_test<bool>();
  do_test<char>();
  do_test<signed char>();
  do_test<unsigned char>();
  do_test<wchar_t>();
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
  return 0;
}
