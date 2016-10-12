// { dg-do run { target c++11 } }
// { dg-require-cstdint "" }

// 2008-05-20  Paolo Carlini  <paolo.carlini@oracle.com>
//
// Copyright (C) 2008-2016 Free Software Foundation, Inc.
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
#include <cstdint>
#include <testsuite_hooks.h>

// Test specializations for char16_t and char32_t, in C++0x.
template<typename T, typename R>
  void
  do_test()
  {
    typedef std::numeric_limits<T> char_type;
    typedef std::numeric_limits<R> impl_type;

    VERIFY( char_type::is_specialized == impl_type::is_specialized );
    VERIFY( char_type::min() == impl_type::min() );
    VERIFY( char_type::max() == impl_type::max() );
    VERIFY( char_type::digits == impl_type::digits );
    VERIFY( char_type::digits10 == impl_type::digits10 );
    VERIFY( char_type::is_signed == impl_type::is_signed );
    VERIFY( char_type::is_integer == impl_type::is_integer );
    VERIFY( char_type::is_exact == impl_type::is_exact );
    VERIFY( char_type::radix == impl_type::radix );
    VERIFY( char_type::epsilon() == impl_type::epsilon() );
    VERIFY( char_type::round_error() == impl_type::round_error() );
    VERIFY( char_type::min_exponent == impl_type::min_exponent );
    VERIFY( char_type::min_exponent10 == impl_type::min_exponent10 );
    VERIFY( char_type::max_exponent == impl_type::max_exponent );
    VERIFY( char_type::max_exponent10 == impl_type::max_exponent10 );
    VERIFY( char_type::has_infinity == impl_type::has_infinity );
    VERIFY( char_type::has_quiet_NaN == impl_type::has_quiet_NaN );
    VERIFY( char_type::has_signaling_NaN == impl_type::has_signaling_NaN );
    VERIFY( char_type::has_denorm == impl_type::has_denorm );
    VERIFY( char_type::has_denorm_loss == impl_type::has_denorm_loss );
    VERIFY( char_type::infinity() == impl_type::infinity() );
    VERIFY( char_type::quiet_NaN() == impl_type::quiet_NaN() );
    VERIFY( char_type::signaling_NaN() == impl_type::signaling_NaN() );
    VERIFY( char_type::denorm_min() == impl_type::denorm_min() );
    VERIFY( char_type::is_iec559 == impl_type::is_iec559 );
    VERIFY( char_type::is_bounded == impl_type::is_bounded );
    VERIFY( char_type::is_modulo == impl_type::is_modulo );
    VERIFY( char_type::traps == impl_type::traps );
    VERIFY( char_type::tinyness_before == impl_type::tinyness_before );
    VERIFY( char_type::round_style == impl_type::round_style );
  }

int main()
{
  do_test<char16_t, uint_least16_t>();
  do_test<char32_t, uint_least32_t>();

  return 0;
}
